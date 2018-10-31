# ProjectGraph

_A simple graph-based project-scheduling tool_

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
stack exec project-graph samples/project.yaml samples/availability.yaml 2018-10-29 output.csv
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

The tool currently supports output in plain text, CSV and GraphViz (DOT) formats.

## Licence

Released under [MIT License][licence]

Copyright &copy; 2018, Richard Cook. All rights reserved.

[availability-yaml]: samples/availability.yaml
[licence]: LICENSE
[project-yaml]: samples/project.yaml