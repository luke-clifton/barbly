# Barbly
[![](https://img.shields.io/hackage/v/barbly.svg?colorB=%23999&label=barbly)](http://hackage.haskell.org/package/barbly)

Barbly allows you to create status bar menus for macOS. It's similar to
[bitbar](https://github.com/matryer/bitbar) and supports some of the same
syntax.

Each instance of a barbly executable creates only one menu item. You specify
the period at which to refresh, and the command to generate the menu contents.

## Sample Scripts

Some sample scripts that are useful to use with barbly can be found in the
[scripts](./scripts) directory.

Here is an example of running the [`github_issues.sh`](./scripts/github_issues.sh)
script with barbly to monitor for new issues.

    barbly -p 60 ./scripts/github_issues.sh nixos nixpkgs

![Example Menu](./doc/demo.png)

Clicking on an item will open the issue in your browser.

## Other Features

## Syntax

Barbly can decode either JSON objects or BitBar syntax for the script outputs.
By default, it will attempt to auto-detect the format, but you can explicitly
tell it which format to use with the `--json` and `--bitbar` flags.

### JSON

The top level object has two fields, `title`, which is a string that will
be displayed in the status bar, and `items` which is an array of menu items
that will be displayed in the drop down menu when the title is clicked.

Each menu item is either `{}`, which creates a menu separator, or an object
with a `label` field, which will be the text for that menu item. Optionally
a menu item can have either an `exec` or a `items` field. An `items` field
would contain an array of menu items and would create a sub-menu. An `exec`
field would create a clickable menu item which executes the command described
by the array of strings in the `exec` field.

```json
{
  "title": "Example",
  "items": [
    {
      "label": "Say Hello",
      "exec": [ "say", "Hello" ]
    },
    {},
    {
      "label": "Sub Menu",
      "items": [
        {
          "label": "DuckDuckGo",
          "exec": [ "open", "https://duckduckgo.com/" ]
        }
      ]
    },
    { "label": "Information Only" }
  ]
}
```

### BitBar

The bitbar simulation is not complete. Only the following features are supported.

The text that appears in the status bar is the output of the script up until a line
containing only `---`.

After this, each line represents an item in a drop down menu. Submenus can be nested
arbitrarily by prefixing the line with `--` (one pair for each level of nesting).

Each line can contain some paramters which appear after the first `|` as key value
pairs separated by an `=`.

| Paramter Name | Effect                                       | Example                        |
|---------------|----------------------------------------------|--------------------------------|
| `href`        | Open the given URL or file.                  | href=https://www.google.com    |
|               |                                              | href=/Applications/Firefox.app |
| `bash`        | Run the given bash script                    | bash=/my/script.bash           |
| `paramX`      | Arguments to pass to the bash script above   | bash=/script.sh param1=5 param2=example |

Menu separators can be created with lines containing only `---`.

### Example Scripts

See the scripts in the [scripts](./scripts) directory for some examples.

### Multiple Menus

Because each instance of barbly creates just one menu, you need to launch
multiple processes to get multiple menus. If you launch them all from a
script, this can cause the order of the resulting menu items to depend on
the whims of the scheduler. This can be alleviated by yielding between
spawning each process. A yield can be achieved by sleeping the process
for 0 seconds.
