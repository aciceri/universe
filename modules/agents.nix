let
  instructions = ''
    # Personal Preferences

    ## Language & Communication
    - Always produce code and comments in English, regardless of my query language
    - Reply to me in the same language I use (Italian, English, etc.)
    - Keep technical terms in English even in non-English responses

    ## Environment: NixOS
    - System: NixOS with Home Manager and Flakes
    - Never attempt imperative installations (no apt, dnf, curl | bash, etc.)
    - If a program is missing:
      1. First check if it's already in PATH
      2. For project-specific tools: add to flake.nix devShell
      3. For one-off usage: use `yes | , <command>` (comma tool via nix-index)

    ## Shell: Nushell (NOT POSIX)
    - This is NOT bash/zsh - different syntax and philosophy
    - Key differences:
      * Variables: `let var = "value"` not `var="value"`
      * Pipes pass structured data, not just text
      * Command substitution: `(command)` not `$(command)`
      * Conditionals: `if condition { } else { }` not `if [ ]; then; fi`
      * Loops: `for item in $list { }` not `for item in list; do; done`
    - Environment variables: use `with-env {VAR: "value"} { command }`
    - External commands need `^` prefix: `^git status`

    ## Development Workflow
    - Check for flake.nix before suggesting installations
    - Prefer `nix develop` shells over global installations
    - Test commands before committing
    - Use `direnv` integration when available (auto-load devShell)
    - Feel free to use `gh` for GitHub operations

    ## Code Style
    - Prefer clarity over cleverness
    - Meaningful variable names (no single letters except loops)
    - Comments for "why", not "what"
    - Break complex operations into readable steps
  '';
  codeConfigFor =
    config:
    let
      stylixColors = config.lib.stylix.colors.withHashtag;
    in
    {
      approval_policy = "untrusted";
      sandbox_mode = "danger-full-access";
      tui = {
        theme = {
          colors = with stylixColors; {
            primary = base0D;
            secondary = base0E;
            background = base00;
            foreground = base05;
            border = base01;
            border_focused = base0D;
            selection = base02;
            cursor = base05;
            success = base0B;
            warning = base0A;
            error = base08;
            info = base0C;
            text = base05;
            text_dim = base04;
            text_bright = base06;
            keyword = base0E;
            string = base0B;
            comment = base03;
            function = base09;
            spinner = base0E;
            progress = base0D;
          };
          name = "custom";
        };
      };
    };
in
{
  flake.modules.homeManager.workstation =
    { pkgs, config, ... }:
    {
      programs = {
        claude-code = {
          enable = true;
          memory.text = instructions;
          mcpServers = {
            openmemory = {
              type = "stdio";
              command = "docker";
              args = [
                "run"
                "-i"
                "-v"
                "claude-memory:/app/dist"
                "--rm"
                "mcp/memory"
              ];
            };
          };
        };

        codex = {
          enable = true;
          custom-instructions = instructions;
          settings = {
            sandbox_mode = "danger-full-access";
            approval_policy = "untrusted";
          };
        };

        gemini-cli = {
          enable = true;
          context.GEMINI = instructions;
        };

        opencode = {
          enable = true;
          rules = instructions;
        };
      };

      home.file.".code/config.toml".source = (pkgs.formats.toml { }).generate "codex-config" (codeConfigFor config);

      home.packages = with pkgs; [
        code
        cursor-cli
      ];
    };
}
