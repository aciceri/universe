{
  flake.modules.homeManager.workstation = {
    programs.claude-code = {
      enable = true;
      memory.text = ''
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

        ## Memory & Context Management
        - ALWAYS use the openmemory MCP server to store and retrieve context
        - Store important information about:
          * User preferences and habits
          * Project-specific decisions and architecture
          * Recurring issues and their solutions
          * Key file locations and project structure
        - Query openmemory before making assumptions about project context
        - Update entities when learning new information about the user or projects
      '';

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
  };
}
