# Restore a deleted report module

Restore a module from the module trash folder.

## Usage

``` r
restore_Module(name, batch_id, test)

# S4 method for class 'character'
restore_Module(name, batch_id, test)
```

## Arguments

- name:

  Name of the module to restore.

- batch_id:

  Optional deletion batch ID.

- test:

  Logical. If TRUE, test the restored module.

## Value

Invisibly returns information about restored modules.
