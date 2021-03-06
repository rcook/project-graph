# Project Graph

_A simple graph-based project-scheduling tool_

![Project Graph running on macOS](/docs/macos.png)

Example output produced using:

```bash
stack build --fast --exec 'project-graph -p samples/project.yaml -a samples/availability.yaml -s 2018-11-02'
```

## Install prerequisites

### Notes

* Project Graph is written in [Haskell][haskell-dot-org]
* I use [Stack][haskell-stack] to build it
* It uses [GTK+ 3][gtk3]
* And [Graphviz][graphviz]
* It's been tested on the following platforms:
  * macOS
  * Ubuntu 18.04

### Ubuntu

```bash
sudo apt install \
  libatk1.0-dev \
  libcairo2-dev \
  libgdk-pixbuf2.0 \
  libgirepository1.0-dev \
  libglib2.0-dev \
  libgtk-3-dev \
  libpango1.0-dev \
  pkg-config
```

### macOS

```bash
brew install \
  gtk+3 \
  gtk-mac-integration
```

You may also need to set up the `PKG_CONFIG_PATH` environment variable:

```bash
eval $(brew info libffi | grep PKG_CONFIG_PATH)
```

## Use ghcid for iterative development

```bash
stack exec ghcid -- -T":main"
```

or

```bash
script/loop
```

## Build

```bash
stack build --fast
```

## Sample output

Here's a sample command line:

```bash
stack exec project-graph -- \
  --project samples/project.yaml \
  --availability samples/availability.yaml \
  --start 2018-10-29 \
  --output output.csv
```

Notes:

* [`samples/project.yaml`][project-yaml] defines the project's groups and tasks
* [`samples/availability.yaml`][availability-yaml] defines the team members and their availability
* `2018-10-29` is the start date of the project

The output looks something like:

```csv
Task A,1,foo,2018-10-29,2018-10-29
Task E,1,foo,2018-10-30,2018-10-30
Task B,2,foo,2018-11-02,2018-11-05
Task C,1,foo,2018-11-06,2018-11-06
Task D,1,foo,2018-11-07,2018-11-07
```

The tool currently supports output in plain text, CSV and Graphviz (DOT) formats.

## Licence

Released under [MIT License][licence]

Copyright &copy; 2018, Richard Cook. All rights reserved.

[availability-yaml]: samples/availability.yaml
[graphviz]: https://www.graphviz.org/
[gtk3]: https://developer.gnome.org/gtk3/stable/
[haskell-dot-org]: https://www.haskell.org/
[haskell-stack]: https://haskellstack.org/
[licence]: LICENSE
[project-yaml]: samples/project.yaml
