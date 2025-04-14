#' Create a new metadata template file
#'
#' @description
#' This function creates a new metadata template file used to describe a new
#' trait dataset. Two formats are currently available: `xlsx` or `yaml`. The
#' metadata template file will be stored in a subdirectory named `name`.
#'
#' @param name a `character` of length 1. The trait dataset identifier used to
#'   create files and folders. Should be short, explicit and without special
#'   characters (including accents).
#'
#' @param path a `character` of length 1. The folder name to stored the
#'   metadata template file in. Must exist.
#'   Default is the current directory.
#'
#' @param format a `character` of length 1. One of `xlsx` or `yaml`.
#'   Default is `yaml`.
#'
#' @param overwite a `logical` of length 1. If `TRUE` overwrites the metadata
#'   template file.
#'   Default is `FALSE`.
#'
#' @return No return value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' td_create_metadata_file(name = "pantheria")
#' }

td_create_metadata_file <- function(
  name,
  path = ".",
  format = "yaml",
  overwrite = FALSE
) {
  check_character_arg(name)
  check_nonascii_char(name)
  check_character_arg(path)
  check_path_exists(path)
  check_character_arg(format)
  check_format_value(format)
  check_logical_arg(overwrite)

  name <- name |>
    tolower() |>
    trimws() |>
    gsub("\\s+", "_", x = _) |>
    gsub("[[:punct:]]", "_", x = _) |>
    gsub("_+", "_", x = _)

  dir_path <- file.path(path, name)
  file_ext <- ifelse(format == "yaml", ".yml", ".xlsx")
  filename <- file.path(dir_path, paste0(name, "_metadata", file_ext))

  if (!file.exists(filename) || overwrite) {
    dir.create(
      path = dir_path,
      showWarnings = FALSE,
      recursive = TRUE
    )

    if (format == "yaml") {
      metadata <- metadata_as_yaml() |>
        gsub("\\.dataset_key", name, x = _)

      cat(metadata, file = filename, append = FALSE)
    } else {
      metadata <- metadata_as_df()
      metadata[["dataset"]]$"value" <- gsub(
        "\\.dataset_key",
        name,
        metadata[["dataset"]]$"value"
      )

      writexl::write_xlsx(x = metadata, path = filename)
    }
  } else {
    message(
      "The file '",
      filename,
      "' already exists.",
      "\nUse 'overwrite = TRUE' to replace its content."
    )
  }

  invisible(NULL)
}


#' Create default metadata as a YAML
#'
#' @noRd

metadata_as_yaml <- function() {
  glue::glue(
    "
    dataset:
      key: .dataset_key           # Dataset identifier
      name: .dataset_name         # Dataset full name
      description: .description   # Short description
      license: .license           # Dataset license
      bibtex: .path               # Dataset citation
      taxon: .taxon               # Taxonomic group (mammals, birds, etc.)
      taxonomic_level: species    # Taxonomic resolution (species, genus, etc.)
      type: static                # One of 'static' or 'api'
      url: .url                   # URL to download the static file
      filename: .filename         # Filename of the static file
      extension: .ext             # File extension of the static file
      manual_download: no         # One of 'yes' or 'no'
      long_format: no             # One of 'yes' or 'no' (traits in columns)
      skip_rows: .na              # Number of header rows to remove
      col_separator: ','          # Character used to separate columns
      na_value: .na               # Character used for missing values
    taxonomy:
      genus: .na                  # Column name of the genus
      species: .na                # Column name of the species
      binomial: .column           # Column name of the binomial name
    traits:
    - name: .trait_name           # Full name of the trait
      variable: .col_name         # Column name of the trait
      type: .type                 # One of 'quantitative' or 'categoric'
      levels: .levels             # Values for categorical trait
      units: .na                  # Original unit
      category: .na               # Category of the trait
      rename_to: .na              # New name of the trait
      convert_to: .na             # New unit of the trait
    - name: .trait_name           # Full name of the trait
      variable: .col_name         # Column name of the trait
      type: .type                 # One of 'quantitative' or 'categoric'
      levels: .levels             # Values for categorical trait
      units: .na                  # Original unit
      category: .category         # Category of the trait
      rename_to: .na              # New name of the trait
      convert_to: .na             # New unit of the trait
    "
  )
}


#' Convert YAML to list of data.frame
#'
#' @noRd

metadata_as_df <- function() {
  metadata <- metadata_as_yaml() |>
    yaml::read_yaml(text = _)

  sheets <- list()

  sheets[["dataset"]] <- data.frame(
    "key" = names(metadata$"dataset"),
    "value" = unlist(metadata$"dataset")
  )

  rownames(sheets[["dataset"]]) <- NULL

  sheets[["taxonomy"]] <- data.frame(
    "key" = names(metadata$"taxonomy"),
    "value" = unlist(metadata$"taxonomy")
  )

  rownames(sheets[["taxonomy"]]) <- NULL

  sheets[["traits"]] <- as.data.frame(metadata$"traits"[[1]])
  sheets[["traits"]] <- rbind(sheets[["traits"]], sheets[["traits"]])

  sheets
}
