# Move a report module

Move an existing module before or after another module, or under a new
parent.

## Usage

``` r
move_Module(name, before, after, parent, level0)

# S4 method for class 'character'
move_Module(name, before, after, parent, level0)
```

## Arguments

- name:

  Name of the module to move.

- before:

  Optional module name before which to move the module.

- after:

  Optional module name after which to move the module.

- parent:

  Optional new parent module.

- level0:

  Character vector defining the root-level module order.

## Value

Invisibly returns the updated module information table.
