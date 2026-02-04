"""CalTrack — extract and report billable hours from an ICS calendar.

Subcommands:
    clients   List all billable clients (with total hours and optional revenue)
    hours     List individual billable events (optionally filtered by client)

Environment variables (all optional, flags take precedence):
    CALTRACK_URL      ICS calendar feed URL
    CALTRACK_MONTH    Month shortcut (e.g. '2026-01', 'january 2026')
    CALTRACK_SINCE    Start of the time range (natural language or ISO date)
    CALTRACK_UNTIL    End of the time range (natural language or ISO date)
    CALTRACK_CLIENT   Filter by client name
    CALTRACK_RATE     Hourly rate for revenue calculation
    CALTRACK_JSON     Set to "1" to force JSON output
"""

import argparse
import json
import os
import sys
from dataclasses import asdict, dataclass
from datetime import datetime, timezone

import dateparser
import requests
from icalendar import Calendar
from rich.console import Console
from rich.table import Table

BILL_PREFIX = "bill to "


# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------


@dataclass
class BillableEvent:
    client: str
    summary: str
    start: datetime
    end: datetime
    hours: float


# ---------------------------------------------------------------------------
# Fetching & parsing
# ---------------------------------------------------------------------------


def fetch_ics(url: str) -> bytes:
    """Fetch raw ICS bytes from the given HTTP URL."""
    try:
        response = requests.get(url, timeout=30)
        response.raise_for_status()
    except requests.RequestException as e:
        print(f"Error fetching calendar: {e}", file=sys.stderr)
        sys.exit(1)
    return response.content


def extract_client(component) -> str | None:
    """Return the client name if any CATEGORIES value
    matches 'Bill to <name>'."""
    categories = component.get("CATEGORIES")
    if categories is None:
        return None

    if not isinstance(categories, list):
        categories = [categories]

    for cat_group in categories:
        for cat in cat_group.to_ical().decode().split(","):
            cat = cat.strip()
            if cat.lower().startswith(BILL_PREFIX):
                return cat[len(BILL_PREFIX):]

    return None


def to_aware(dt) -> datetime:
    """Ensure a datetime is timezone-aware (UTC if naive)."""
    if hasattr(dt, "hour"):
        # It's a datetime, not a date
        if dt.tzinfo is None:
            return dt.replace(tzinfo=timezone.utc)
        return dt
    # It's a date — convert to midnight UTC datetime
    return datetime(dt.year, dt.month, dt.day, tzinfo=timezone.utc)


def parse_events(ics_bytes: bytes) -> list[BillableEvent]:
    """Parse ICS bytes and return all billable events."""
    cal = Calendar.from_ical(ics_bytes)
    events: list[BillableEvent] = []

    for component in cal.walk():
        if component.name != "VEVENT":
            continue

        client = extract_client(component)
        if client is None:
            continue

        dtstart = component.get("DTSTART")
        dtend = component.get("DTEND")
        if dtstart is None or dtend is None:
            continue

        start = to_aware(dtstart.dt)
        end = to_aware(dtend.dt)
        hours = (end - start).total_seconds() / 3600

        events.append(
            BillableEvent(
                client=client,
                summary=str(component.get("SUMMARY", "")),
                start=start,
                end=end,
                hours=hours,
            )
        )

    return events


# ---------------------------------------------------------------------------
# Date parsing
# ---------------------------------------------------------------------------


def parse_date(text: str) -> datetime | None:
    """Parse a date string using dateparser (natural language or ISO)."""
    result = dateparser.parse(
        text, settings={"RETURN_AS_TIMEZONE_AWARE": True}
    )
    return result


def parse_month(text: str) -> tuple[datetime, datetime] | None:
    """Parse a month string into (first day, first day of next month).

    Accepts: '2026-01', 'january 2026', 'jan 2026', 'last month', etc.
    Returns a pair of timezone-aware datetimes or None if unparseable.
    """
    # Try ISO format first (e.g. "2026-01")
    try:
        parts = text.strip().split("-")
        if len(parts) == 2:
            year, month = int(parts[0]), int(parts[1])
            if 1 <= month <= 12:
                return _month_bounds(year, month)
    except (ValueError, IndexError):
        pass

    # Fall back to dateparser — force it to the 1st of the month
    parsed = dateparser.parse(
        text,
        settings={
            "RETURN_AS_TIMEZONE_AWARE": True,
            "PREFER_DAY_OF_MONTH": "first",
        },
    )
    if parsed is None:
        return None

    return _month_bounds(parsed.year, parsed.month)


def _month_bounds(year: int, month: int) -> tuple[datetime, datetime]:
    """Return (first day, first day of next month) as aware datetimes."""
    first = datetime(year, month, 1, tzinfo=timezone.utc)
    # First moment of the next month
    if month == 12:
        next_first = datetime(year + 1, 1, 1, tzinfo=timezone.utc)
    else:
        next_first = datetime(year, month + 1, 1, tzinfo=timezone.utc)
    return first, next_first


# ---------------------------------------------------------------------------
# Filtering
# ---------------------------------------------------------------------------


def filter_events(
    events: list[BillableEvent],
    since: datetime | None,
    until: datetime | None,
    client: str | None,
) -> list[BillableEvent]:
    """Filter events by time range and/or client name (case-insensitive)."""
    filtered = events

    if since is not None:
        filtered = [e for e in filtered if e.start >= since]
    if until is not None:
        filtered = [e for e in filtered if e.start <= until]
    if client is not None:
        client_lower = client.lower()
        filtered = [
            e for e in filtered if e.client.lower() == client_lower
        ]

    return filtered


# ---------------------------------------------------------------------------
# Output — rich tables
# ---------------------------------------------------------------------------


def print_clients_table(
    events: list[BillableEvent], rate: float | None
) -> None:
    """Print a summary table grouped by client."""
    # Aggregate hours per client
    client_hours: dict[str, float] = {}
    for event in events:
        client_hours[event.client] = (
            client_hours.get(event.client, 0.0) + event.hours
        )

    total_hours = sum(client_hours.values())

    table = Table(show_header=True, show_lines=False)
    table.add_column("Client", style="bold")
    table.add_column("Hours", justify="right")
    if rate is not None:
        table.add_column("Revenue", justify="right", style="green")

    for client in sorted(client_hours):
        hours = client_hours[client]
        row = [client, f"{hours:.1f}"]
        if rate is not None:
            row.append(f"€{hours * rate:,.2f}")
        table.add_row(*row)

    # Totals row
    table.add_section()
    totals = ["TOTAL", f"{total_hours:.1f}"]
    if rate is not None:
        totals.append(f"€{total_hours * rate:,.2f}")
    table.add_row(*totals, style="bold")

    Console().print(table)


def print_hours_table(
    events: list[BillableEvent], rate: float | None
) -> None:
    """Print a detailed table of individual events."""
    total_hours = sum(e.hours for e in events)

    table = Table(show_header=True, show_lines=False)
    table.add_column("Date", style="bold")
    table.add_column("Client")
    table.add_column("Summary")
    table.add_column("Hours", justify="right")
    if rate is not None:
        table.add_column("Revenue", justify="right", style="green")

    for event in sorted(events, key=lambda e: e.start):
        row = [
            event.start.strftime("%Y-%m-%d"),
            event.client,
            event.summary,
            f"{event.hours:.1f}",
        ]
        if rate is not None:
            row.append(f"€{event.hours * rate:,.2f}")
        table.add_row(*row)

    # Totals row
    table.add_section()
    totals = ["", "", "TOTAL", f"{total_hours:.1f}"]
    if rate is not None:
        totals.append(f"€{total_hours * rate:,.2f}")
    table.add_row(*totals, style="bold")

    Console().print(table)


# ---------------------------------------------------------------------------
# Output — JSON
# ---------------------------------------------------------------------------


def event_to_dict(event: BillableEvent) -> dict:
    """Serialize a BillableEvent to a JSON-friendly dict."""
    data = asdict(event)
    data["start"] = event.start.isoformat()
    data["end"] = event.end.isoformat()
    return data


def output_clients_json(
    events: list[BillableEvent], rate: float | None
) -> None:
    """Print clients summary as JSON."""
    client_hours: dict[str, float] = {}
    for event in events:
        client_hours[event.client] = (
            client_hours.get(event.client, 0.0) + event.hours
        )

    total_hours = sum(client_hours.values())

    clients = [
        {"client": c, "hours": h, **({"revenue": h * rate} if rate else {})}
        for c, h in sorted(client_hours.items())
    ]

    output = {
        "clients": clients,
        "total_hours": round(total_hours, 2),
        **({"total_revenue": round(total_hours * rate, 2)} if rate else {}),
    }

    print(json.dumps(output, indent=2))


def output_hours_json(
    events: list[BillableEvent], rate: float | None
) -> None:
    """Print individual events as JSON."""
    total_hours = sum(e.hours for e in events)
    sorted_events = sorted(events, key=lambda e: e.start)

    event_list = []
    for event in sorted_events:
        entry = event_to_dict(event)
        if rate is not None:
            entry["revenue"] = round(event.hours * rate, 2)
        event_list.append(entry)

    output = {
        "events": event_list,
        "total_hours": round(total_hours, 2),
        **(
            {"total_revenue": round(total_hours * rate, 2)}
            if rate
            else {}
        ),
    }

    print(json.dumps(output, indent=2))


# ---------------------------------------------------------------------------
# Argparse — shared helpers
# ---------------------------------------------------------------------------


def env(name: str) -> str | None:
    """Read an env var, return None if empty or unset."""
    value = os.environ.get(name, "").strip()
    return value or None


def add_common_args(parser: argparse.ArgumentParser) -> None:
    """Add flags shared across all subcommands."""
    parser.add_argument(
        "--month",
        default=env("CALTRACK_MONTH"),
        help=(
            "Filter by month. Accepts '2026-01', "
            "'january 2026', 'last month', etc. "
            "Shortcut for --since/--until. "
            "Env: CALTRACK_MONTH"
        ),
    )
    parser.add_argument(
        "--since",
        default=env("CALTRACK_SINCE"),
        help=(
            "Start of the time range. Accepts natural language "
            "('last monday', '2 weeks ago') or ISO date "
            "(2026-01-15). Env: CALTRACK_SINCE"
        ),
    )
    parser.add_argument(
        "--until",
        default=env("CALTRACK_UNTIL"),
        help=(
            "End of the time range. Same formats as --since. "
            "Env: CALTRACK_UNTIL"
        ),
    )
    parser.add_argument(
        "--rate",
        type=float,
        default=(
            float(env("CALTRACK_RATE"))
            if env("CALTRACK_RATE")
            else None
        ),
        help=(
            "Hourly rate for revenue calculation. "
            "Env: CALTRACK_RATE"
        ),
    )
    parser.add_argument(
        "--json",
        action="store_true",
        default=env("CALTRACK_JSON") == "1",
        dest="use_json",
        help="Output as JSON. Env: CALTRACK_JSON=1",
    )


# ---------------------------------------------------------------------------
# Subcommands
# ---------------------------------------------------------------------------


def cmd_clients(args) -> None:
    """Handler for the 'clients' subcommand."""
    since, until = resolve_date_range(args)
    events = filter_events(
        parse_events(fetch_ics(args.url)), since, until, None
    )

    if not events:
        print("No billable events found.", file=sys.stderr)
        return

    if args.use_json:
        output_clients_json(events, args.rate)
    else:
        print_clients_table(events, args.rate)


def cmd_hours(args) -> None:
    """Handler for the 'hours' subcommand."""
    since, until = resolve_date_range(args)
    events = filter_events(
        parse_events(fetch_ics(args.url)),
        since,
        until,
        args.client,
    )

    if not events:
        print("No billable events found.", file=sys.stderr)
        return

    if args.use_json:
        output_hours_json(events, args.rate)
    else:
        print_hours_table(events, args.rate)


# ---------------------------------------------------------------------------
# Date range resolution
# ---------------------------------------------------------------------------


def resolve_date_range(
    args,
) -> tuple[datetime | None, datetime | None]:
    """Parse --month / --since / --until into aware datetimes.

    --month is a shortcut that expands to since=first day,
    until=first day of next month. It is mutually exclusive
    with --since and --until.
    """
    # Guard: --month and --since/--until are mutually exclusive
    if args.month and (args.since or args.until):
        print(
            "Error: --month cannot be used together "
            "with --since or --until",
            file=sys.stderr,
        )
        sys.exit(1)

    if args.month:
        bounds = parse_month(args.month)
        if bounds is None:
            print(
                f"Cannot parse --month: '{args.month}'",
                file=sys.stderr,
            )
            sys.exit(1)
        return bounds

    since = None
    until = None

    if args.since:
        since = parse_date(args.since)
        if since is None:
            print(
                f"Cannot parse --since: '{args.since}'",
                file=sys.stderr,
            )
            sys.exit(1)

    if args.until:
        until = parse_date(args.until)
        if until is None:
            print(
                f"Cannot parse --until: '{args.until}'",
                file=sys.stderr,
            )
            sys.exit(1)

    return since, until


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------


def main() -> None:
    url_default = env("CALTRACK_URL")

    parser = argparse.ArgumentParser(
        prog="caltrack",
        description=(
            "Extract and report billable hours from an ICS "
            "calendar feed. Events are identified by a "
            "'Bill to <client>' category."
        ),
        epilog=(
            "Most options can be set via environment variables. "
            "CLI flags always take precedence. See subcommand "
            "help (-h) for details."
        ),
    )
    parser.add_argument(
        "url",
        nargs="?",
        default=url_default,
        help=(
            "HTTP(S) URL of the ICS feed. "
            "Env: CALTRACK_URL"
        ),
    )

    subparsers = parser.add_subparsers(
        dest="command", help="Available commands"
    )
    subparsers.required = True

    # --- clients ---
    clients_parser = subparsers.add_parser(
        "clients",
        help="List all billable clients with total hours",
        description=(
            "List every client found in 'Bill to <name>' "
            "categories, with their total hours. Add --rate "
            "to see revenue."
        ),
    )
    add_common_args(clients_parser)
    clients_parser.set_defaults(func=cmd_clients)

    # --- hours ---
    hours_parser = subparsers.add_parser(
        "hours",
        help="List billable events with hours",
        description=(
            "List individual billable events. Optionally filter "
            "by client with --client. Add --rate to see revenue."
        ),
    )
    hours_parser.add_argument(
        "--client",
        default=env("CALTRACK_CLIENT"),
        help="Filter by client name (case-insensitive). Env: CALTRACK_CLIENT",
    )
    add_common_args(hours_parser)
    hours_parser.set_defaults(func=cmd_hours)

    args = parser.parse_args()

    if args.url is None:
        parser.error(
            "URL is required (positional arg or CALTRACK_URL env var)"
        )

    args.func(args)


main()
