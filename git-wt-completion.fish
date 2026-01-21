#!/usr/bin/env fish
# Fully self-contained completion script for 'git wt'

# Helper function to check if we're at the git command with no subcommand yet
function __git_wt_needs_command
    set -l cmdline (commandline -opc)
    # git with no args or git followed by options only
    if [ (count $cmdline) -eq 1 -a "$cmdline[1]" = "git" ]
        return 0
    end
    # git with options but no subcommand
    if [ (count $cmdline) -gt 1 -a "$cmdline[1]" = "git" ]
        for i in $cmdline[2..-1]
            if not string match -q -- "-*" $i
                return 1
            end
        end
        return 0
    end
    return 1
end

# Helper function to check if 'git wt' is the current command
function __git_wt_using_command
    set -l cmdline (commandline -opc)
    if [ (count $cmdline) -ge 2 -a "$cmdline[1]" = "git" -a "$cmdline[2]" = "wt" ]
        return 0
    end
    return 1
end

# Helper function to check if a specific subcommand is used
function __git_wt_using_subcommand
    set -l cmdline (commandline -opc)
    set -l subcommand $argv[1]
    if [ (count $cmdline) -ge 3 -a "$cmdline[1]" = "git" -a "$cmdline[2]" = "wt" -a "$cmdline[3]" = "$subcommand" ]
        return 0
    end
    return 1
end

# Helper function to get git branches for completion
function __git_wt_get_branches
    git branch --no-color 2>/dev/null | string replace -r '^\*?\s*' ''
end

# Register 'wt' as a git subcommand
complete -f -c git -n "__git_wt_needs_command" -a "wt" -d "Lightweight worktree manager"

# Register subcommands for 'git wt'
complete -f -c git -n "__git_wt_using_command" -a "new" -d "Create a new worktree"
complete -f -c git -n "__git_wt_using_command" -a "add" -d "Add an existing branch as worktree"
complete -f -c git -n "__git_wt_using_command" -a "path" -d "Get path of a worktree"
complete -f -c git -n "__git_wt_using_command" -a "prune" -d "Remove all deleted worktrees"

# Add branch completions for the relevant subcommands
complete -f -c git -n "__git_wt_using_subcommand new" -a "(__git_wt_get_branches)" -d "Branch"
complete -f -c git -n "__git_wt_using_subcommand add" -a "(__git_wt_get_branches)" -d "Branch"
complete -f -c git -n "__git_wt_using_subcommand path" -a "(__git_wt_get_branches)" -d "Branch"
