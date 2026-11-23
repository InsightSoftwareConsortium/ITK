# Documenting Utility Scripts

Shell scripts under `Utilities/` describe themselves in one comment block,
which is also their `--help` text. A reader of the file and a user running
the script see the same words, and there is only one copy to keep current.

## The comment block

Each line of the block starts with `#%`. It follows the license header, if
any, and has these sections, in this order, modeled on those of a manual
page:

- `NAME`: the script name and a one-line summary in the imperative.
- `SYNOPSIS`: each way to call the script, including `-h | --help`.
- `DESCRIPTION`: what the script does and what it changes.
- `OPTIONS`: every option and argument; `-h, --help` is always listed.
- `IMPLEMENTATION`: what a user needs that the code does not show, such as
  required tools, network access, the directory to run from, and side
  effects.
- `EXAMPLES`: commands to copy, each with one line saying what it does.
- `SEE ALSO`: related scripts and documents, by path.

A section with nothing to say holds `None.`, so every script has all seven.

```bash
#% NAME
#%     WrappingModulesCheck.sh - find wrapping files in the wrong module
#%
#% SYNOPSIS
#%     WrappingModulesCheck.sh
#%     WrappingModulesCheck.sh -h | --help
#%
#% DESCRIPTION
#%     ...
```

## Printing the block

`usage()` prints the block, and `-h` or `--help` calls it before the script
does anything with an effect:

```bash
usage() {
  sed -n 's/^#% \{0,1\}//p' "${BASH_SOURCE[0]}"
}

case "${1-}" in
  -h | --help)
    usage
    exit 0
    ;;
esac
```

A script that already parses its options keeps its parser, and its `-h`
and `--help` cases call `usage` with the exit status they had before. A
script that reports a usage error may call `usage` and exit with a nonzero
status.

## Scope

The convention covers the shell scripts that ITK maintains. Scripts copied
from other projects, such as `Utilities/GitSetup/`, `Utilities/Hooks/`,
`Utilities/Maintenance/clang-format.bash`, and
`Utilities/Maintenance/update-third-party.bash`, keep their upstream form
so they can be updated from their source.
