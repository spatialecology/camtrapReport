# List available report modules

List modules available in the camtrapReport module library.

## Usage

``` r
list_Modules(tree, brief, include_trash, validate)

# S4 method for class 'ANY'
list_Modules(tree, brief, include_trash, validate)
```

## Arguments

- tree:

  Logical. If TRUE, return modules as a tree table.

- brief:

  Logical. If TRUE, return a brief table.

- include_trash:

  Logical. If TRUE, include active trash records.

- validate:

  Logical. If TRUE, validate module YAML files.

## Value

A data frame, or a list containing modules and trash records.
