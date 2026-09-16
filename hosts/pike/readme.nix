{
  readme.parts.nixos = ''
    ### Pike

    My mobile workstation—an Acer Nitro V15 laptop equipped with an i5-13420H CPU,
    32GB of DDR5 RAM, and an Nvidia RTX 2050 GPU.
    Configured to closely mirror my main workstation `picard` for seamless work when away from home.

    #### Persistent multiverse agent

    With `multiverse.enable`, `ccr` runs `omp-multiverse.service` without a terminal.
    It starts in the multiverse checkout with `anthropic/claude-opus-5`, continues
    its dedicated history under `~/.local/state/omp-multiverse/sessions`, and
    appears as `multiverse` in the existing OMP session gateway. Open the session
    and select **Control** to send prompts and answer dialogs from the phone.
    **Start live voice** also works here: the phone supplies the microphone and
    speaker, while pike keeps the realtime connection, existing Codex credentials
    and device attestation. No microphone, speaker or terminal is needed on pike.
    Voice uses the configured `live.voice`; the main agent remains Opus 5.

    User lingering starts the service at boot and keeps it running after logout.
    Pike must remain awake and online for phone access. The private RPC FIFO is
    `''${XDG_RUNTIME_DIR}/omp-multiverse/stdin`; it is not a network listener.
    The `m` and `geo` commands remain separate interactive sessions.

    ```sh
    systemctl --user status omp-multiverse.service
    systemctl --user restart omp-multiverse.service
    systemctl --user stop omp-multiverse.service
    systemctl --user start omp-multiverse.service
    journalctl --user -u omp-multiverse.service -f
    ```

    An explicit stop also stops FIFO activation and leaves the session history
    intact. Unexpected exits restart automatically; setting `multiverse.enable`
    to `false` removes this service and socket from the configuration.
    Journal output includes RPC events and can contain conversation and tool data.
  '';
}
