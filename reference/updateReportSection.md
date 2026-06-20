# Update a report section

Updates the title, text, code chunk, code settings or package list of an
existing report section in a \`camReport\` object.

## Usage

``` r
updateReportSection(
  x,
  section,
  text,
  title,
  code,
  code_name,
  code_setting,
  packages,
  append_text,
  append_code
)

# S4 method for class 'camReport'
updateReportSection(
  x,
  section,
  text,
  title,
  code,
  code_name,
  code_setting,
  packages,
  append_text,
  append_code
)
```

## Arguments

- x:

  A \`camReport\` object.

- section:

  A single character string identifying the section by name or title.

- text:

  Optional replacement text.

- title:

  Optional replacement title.

- code:

  Optional replacement or appended R code. Code can be supplied as a
  character string or inside braces.

- code_name:

  Optional code chunk name. Required if a section contains multiple
  chunks and a specific chunk should be updated.

- code_setting:

  Optional R Markdown chunk settings.

- packages:

  Optional character vector of packages required by the chunk.

- append_text:

  Logical. If \`TRUE\`, append text to the existing section text.

- append_code:

  Logical. If \`TRUE\`, append code to the existing code chunk.

## Value

Invisibly returns the updated \`camReport\` object.
