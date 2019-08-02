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

    barbly -p 60 ./scripts/github_issues.sh luke-clifton shh

![Example Menu](./doc/demo.png)

Clicking on an item will open the issue in your browser.

## Syntax

First line creates a title, subsequent lines create menu entries. An entry
that contains a `|` character can provide extra metadata. Currently the
only supported metadata is `href=`, to which you can supply a URL (include
the scheme!) or a file that will be opened with the default viewer.

A line containing only `---` will create a menu separator.

