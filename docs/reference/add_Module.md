# Add a report module

Add a new YAML report module to the camtrapReport module library.

## Usage

``` r
add_Module(x, before, after, test, object)

# S4 method for class 'character'
add_Module(x, before, after, test, object)
```

## Arguments

- x:

  Path to a YAML module file.

- before:

  Optional module name before which the new module should be inserted.

- after:

  Optional module name after which the new module should be inserted.

- test:

  Logical. If TRUE, test the module before adding it.

- object:

  Optional camReport object used for testing the module.

## Value

Invisibly returns information about the added module.
