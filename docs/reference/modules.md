# Managing modules

A list of functions are available to manage modules in the package
including listing the existing modules, adding a new one or deleting an
existing one, or moving the position of a module in a report

## Usage

``` r
add_Module(x,before,after,test)

move_Module(name,before,after,parent,level0)

remove_Module(name,recursive)

empty_trash(name,id)

list_Modules(tree,brief,include_trash,validate)
```

## Arguments

- x:

  The new module YAML filename

- before:

  The name of module before which the new module should be added

- after:

  The name of module after which the new module should be added

- test:

  logical; specifying whether the module should be tested

- name:

  The module name

- parent:

  The parent name of module (modules are organised hierarchically as
  parent and childs)

- level0:

  Optional; names of the modules in root: c("introduction", "methods",
  "results", "acknowledgement", "appendix")

- recursive:

  logical; when it is TRUE, by deleting a parent module, its child
  modules are also deleted to avoid any inconsistencies in the report

- id:

  The module id

- tree:

  logical; specifies whether the list of modules should represents their
  hierarchical tree structure

- brief:

  logical; if tree is FALSE, it specifies whether brief module
  information should be returned

- include_trash:

  logical; if tree is FALSE, it specifies whether deleted modules in
  trash should also be listed

- validate:

  logical; whether the modules should be tested for their validity

## Details

Given that the package is extensible, designed based on a modular
architecture, a set of functions are provided to facilitate module
management.

A module refers to an object designed to contain the contents of a
section in the report. These contents can be texts, tables, executable R
codes, etc. Multiple interfaces are available for a user to define a
module.

An easy way is to use a YAML file (a template can be used) to define a
module. While the file is created, it can be added to the package using
the `add_Module` function.

Other functions can also be used to list the existing modules, delete or
recover a module, or move a module to a different location within the
hierarchical structure the modules are organised.

## References

Ebrahimi et al. (2026) camtrapReport: xxx

## Author

Elham Ebrahimi

## Examples

``` r
list_Modules()
#>                      ID                 name             parent level
#> introduction          1         introduction              .root     0
#> methods               2              methods              .root     0
#> study_area            3           study_area            methods     1
#> sampling              4             sampling            methods     1
#> location              5             location            methods     1
#> effort                6               effort            methods     1
#> image_processing      7     image_processing            methods     1
#> data_processing       8      data_processing            methods     1
#> results               9              results              .root     0
#> captures             10             captures            results     1
#> abundance_trends     11     abundance_trends            results     1
#> population_density   12   population_density            results     1
#> model_parameters     13     model_parameters population_density     2
#> population_densities 14 population_densities population_density     2
#> activity_patterns    15    activity_patterns            results     1
#> richness             16             richness            results     1
#> co_occurrence        17        co_occurrence            results     1
#> spatial_density      18      spatial_density            results     1
#> habitat_preferences  19  habitat_preferences            results     1
#> species_accumulation 20 species_accumulation            results     1
#> acknowledgements     21     acknowledgements              .root     0
#> appendix             22             appendix              .root     0
#>                                           label
#> introduction                     - introduction
#> methods                               - methods
#> study_area                         - study_area
#> sampling                             - sampling
#> location                             - location
#> effort                                 - effort
#> image_processing             - image_processing
#> data_processing               - data_processing
#> results                               - results
#> captures                             - captures
#> abundance_trends             - abundance_trends
#> population_density         - population_density
#> model_parameters             - model_parameters
#> population_densities     - population_densities
#> activity_patterns           - activity_patterns
#> richness                             - richness
#> co_occurrence                   - co_occurrence
#> spatial_density               - spatial_density
#> habitat_preferences       - habitat_preferences
#> species_accumulation     - species_accumulation
#> acknowledgements             - acknowledgements
#> appendix                             - appendix
```
