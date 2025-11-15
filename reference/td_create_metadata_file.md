# Create a new metadata template file

This function creates a new metadata template file used to describe a
new trait dataset. Two formats are currently available: `xlsx` or
`yaml`. The metadata template file will be stored in a subdirectory
named `name`.

## Usage

``` r
td_create_metadata_file(name, path = ".", format = "yaml", overwrite = FALSE)
```

## Arguments

- name:

  a `character` of length 1. The trait dataset identifier used to create
  files and folders. Should be short, explicit and without special
  characters (including accents).

- path:

  a `character` of length 1. The folder name to stored the metadata
  template file in. Must exist. Default is the current directory.

- format:

  a `character` of length 1. One of `xlsx` or `yaml`. Default is `yaml`.

- overwrite:

  a `logical` of length 1. If `TRUE` overwrites the metadata template
  file. Default is `FALSE`.

## Value

No return value.

## Examples

``` r
if (FALSE) { # \dontrun{
td_create_metadata_file(name = "pantheria")
} # }
```
