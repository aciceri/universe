# Start OmniWM at login (declarative). Alternative to OmniWM's own in-app
# "Launch at Login" toggle — enable only ONE of the two, or you get a double
# launch. `open -a` gives the app a proper GUI launch context; the Accessibility
# grant is keyed to the signed bundle, so it survives this launch method.
{
  configurations.darwin.archer.module = {
    launchd.user.agents.omniwm.serviceConfig = {
      ProgramArguments = [
        "/usr/bin/open"
        "-a"
        "OmniWM"
      ];
      RunAtLoad = true;
      KeepAlive = false;
    };
  };
}
