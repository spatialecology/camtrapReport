# Generate a Data Status Check Report

Generates an automated data-status report from a \`camReport\` object.
The report summarises key information about data completeness, spatial
and temporal coverage, annotation quality and validation status.

## Usage

``` r
status(object, filename, view)

# S4 method for class 'camReport'
status(object, filename = "data_status", view)
```

## Arguments

- object:

  A \`camReport\` object.

- filename:

  Output filename or file path without extension. Defaults to
  \`"data_status"\`.

- view:

  Logical. If \`TRUE\`, the generated HTML report is opened after
  rendering.

## Value

Invisibly returns the path to the generated HTML report.
