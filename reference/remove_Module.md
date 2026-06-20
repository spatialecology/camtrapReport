# Remove a report module

Move a module, and optionally its child modules, to the module trash
folder.

## Usage

``` r
remove_Module(name, recursive)

# S4 method for class 'character'
remove_Module(name, recursive)
```

## Arguments

- name:

  Name of the module to remove.

- recursive:

  Logical. If TRUE, remove child modules as well.

## Value

Invisibly returns information about the deleted module batch.
