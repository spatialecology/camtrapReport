# Install packages required by camtrapReport

Installs packages required for the full camtrapReport workflow,
including packages used by optional report modules.

## Usage

``` r
install_All(
  pkgs = NULL,
  update = FALSE,
  include_dev = FALSE,
  include_github = TRUE,
  include_gitlab = TRUE,
  ...
)
```

## Arguments

- pkgs:

  Optional character vector of package names to install. If \`NULL\`,
  the default camtrapReport package list is used.

- update:

  Logical. If \`TRUE\`, reinstall GitHub/GitLab packages and attempt to
  install or update CRAN packages.

- include_dev:

  Logical. If \`TRUE\`, also install developer packages such as
  \`testthat\`, \`pkgdown\`, \`covr\`, \`usethis\` and \`devtools\`.

- include_github:

  Logical. If \`TRUE\`, install required GitHub packages.

- include_gitlab:

  Logical. If \`TRUE\`, install required GitLab packages if a GitLab
  package list is available.

- ...:

  Additional arguments passed to \`install.packages()\`.

## Value

Invisibly returns \`TRUE\` if all required packages are available,
otherwise \`FALSE\`.
