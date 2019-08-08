# Barbly

Barbly allows you to create status bar menus for macOS. It's similar to
[bitbar](https://github.com/matryer/bitbar) and supports some of the same
syntax.

Each instance of a barbly executable creates only one menu item. You specify
the period at which to refresh, and the command to generate the menu contents.

The first line becomes the title of the menu, and subsequent items become
the contents of the menu, with possible clickable actions.

    # Create a clock based on the output of the `date` command. Updates every
    # second.
    barbly --period 1 date

## Sample Scripts

Some sample scripts that are useful to use with barbly can be found in the
[scripts](./scripts) directory.

Here is an example of running the [`github_issues.sh`](./scripts/github_issues.sh)
script with barbly to monitor for new issues.

    barbly -p 60 ./scripts/github_issues.sh nixos nixpkgs

![Example Menu](./doc/demo.png)

Clicking on an item will open the issue in your browser.

## Syntax

The text that appears in the status bar is the output of the script up until a line
containing only `---`.

After this, each line represents an item in a drop down menu. Submenus can be nested
arbitrarily by prefixing the line with `--` (one pair for each level of nesting).

Each line can contain some paramters which appear after the first `|` as key value
pairs separated by an `=`.

| Paramter Name | Effect                                       | Example                        |
|---------------|----------------------------------------------|--------------------------------|
| href          | Open the given URL or file.                  | href=https://www.google.com    |
|               |                                              | href=/Applications/Firefox.app |

See the scripts in the [scripts](./scripts) directory for some examples.
