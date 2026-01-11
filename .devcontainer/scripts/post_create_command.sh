#!/usr/bin/env bash
# This script is used in the "postCreateCommand" of the `devcontainer.json` configuration file.
# It runs exactly once after a (re-)build of the devcontainer. It is run inside the container.
# the working directory is the repository root (/workspaces/fortran-playground).
# See: https://code.visualstudio.com/docs/devcontainers/create-dev-container#_rebuild
set -eo pipefail

if [[ ! -d ".vscode" ]]; then
    cp -R .devcontainer/vscode_example/ .vscode/
fi
