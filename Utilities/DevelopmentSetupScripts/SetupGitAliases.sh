#!/usr/bin/env bash

# Set up some useful git aliases, global aliases for general things
echo -n "Would you like general Git aliases to be global? [y/N]:"
read answer

if [ "$answer" == "y" ]; then
  global="--global"
elif [ "$answer" == "yes" ]; then
  global="--global"
elif [ "$answer" == "Yes" ]; then
  global="--global"
else
  global=""
fi

GIT=git

GITCONFIG="${GIT} config ${global}"

# General aliases that could be global

# Pull all updates - first a general pull and then submodules.
${GITCONFIG} alias.pullall "!bash -c \"git pull && git submodule update --init\""
# Useful alias to see what commits are on the current branch with respect
# to origin/master.
${GITCONFIG} alias.prepush 'log --graph --stat origin/master..'

# Staging aliases - can help streamline staging topic branches.
GITCONFIG="${GIT} config"
stage_cmd='ssh git@itk.org stage ITK'
git_branch="\$(git symbolic-ref HEAD | sed -e 's|^refs/heads/||')"
# General alias to run the SSH command, e.g. git stage-cmd print.
${GITCONFIG} alias.stage-cmd "!${stage_cmd}"
# Push the current topic branch to the stage.
${GITCONFIG} alias.stage-push "!bash -c \"git fetch stage --prune && git push stage HEAD\""
# List all staged topic branches.
${GITCONFIG} alias.stage-branch "!bash -c \"${stage_cmd} print\""
# Merge the current topic branch (if staged) into the next branch.
${GITCONFIG} alias.stage-merge-next "!bash -c \"${stage_cmd} merge -b next ${git_branch}\""
# Merge the current topic branch (if staged) into the master branch.
${GITCONFIG} alias.stage-merge "!bash -c \"${stage_cmd} merge ${git_branch}\""
# Alias to push the current topic branch to Gerrit
${GITCONFIG} alias.gerrit-push "!bash Utilities/Git/git-gerrit-push"
# Alias to push and merge the active topic to the stage
${GITCONFIG} alias.gerrit-merge "!bash Utilities/Git/git-gerrit-merge"
