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

Notes:

* [`samples/project.yaml`][project-yaml] defines the project's groups and tasks
* [`samples/availability.yaml`][availability-yaml] defines the team members and their availability
* `2018-10-29` is the start date of the project

The output looks something like:

```bash
$ stack exec project-graph samples/project.yaml samples/availability.yaml 2018-10-29
task: Task A, effort: 1 days, owner: Person "foo", startDay: 2018-10-29, endDay: 2018-10-29
task: Task E, effort: 1 days, owner: Person "foo", startDay: 2018-10-30, endDay: 2018-10-30
task: Task B, effort: 2 days, owner: Person "foo", startDay: 2018-11-02, endDay: 2018-11-05
task: Task C, effort: 1 days, owner: Person "foo", startDay: 2018-11-06, endDay: 2018-11-06
task: Task D, effort: 1 days, owner: Person "foo", startDay: 2018-11-07, endDay: 2018-11-07
```

The plan is for this tool to generate output in Excel, CSV and/or Graphviz format eventually.

## Licence

Released under [MIT License][licence]

Copyright &copy; 2018, Richard Cook. All rights reserved.

[availability-yaml]: samples/availability.yaml
[licence]: LICENSE
[project-yaml]: samples/project.yaml