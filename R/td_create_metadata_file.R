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
    status: draft                 # One of 'draft' or 'complete'
    dataset:
      id: .dataset_id             # Dataset identifier
      title: .dataset_title       # Dataset title
      description: .description   # Short description
      license: .license           # Dataset license
      bibtex: .filename           # Dataset citation
      doi: .doi                   # DOI of the dataset description (paper)
      url: .url                   # URL of the dataset description (paper)
      taxon: .taxon               # Taxonomic group (mammals, birds, etc.)
      taxonomic_level: species    # Taxonomic resolution (species, genus, etc.)
      type: static                # One of 'static' or 'api'
      file_url: .url              # Full URL to download the static file
      file_name: .filename        # Name of the static file
      file_extension: .ext        # File extension of the static file
      manual_download: no         # One of 'yes' or 'no'
      long_format: no             # One of 'yes' or 'no' (traits in columns)
      skip_rows: .na              # Number of header rows to remove
      col_separator: ','          # Character used to separate columns
      na_value: .na               # Character used for missing values
      comment: .na
    taxonomy:
      genus: .na                  # Column name of the genus
      species: .na                # Column name of the species
      binomial: .column           # Column name of the binomial name
    traits:
    - name: .trait_name_1         # Full name of the trait
      variable: .col_name         # Column name of the trait
      category: .na               # Category of the trait
      type: quantitative          # One of 'quantitative' or 'categoric'
      units: .unit                # Original unit
    - name: .trait_name_2         # Full name of the trait
      variable: .col_name         # Column name of the trait
      category: .na               # Category of the trait
      type: categorical           # One of 'quantitative' or 'categorical'
      units: .na                  # Original unit
      levels: 
      - value: .value             # Value 1 for categorical trait
        description: .descr       # Description of the category
      - value: .value             # Value 2 for categorical trait
        description: .descr       # Description of the category
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

  sheets[["status"]] <- data.frame("status" = "draft")

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

  trait_q <- as.data.frame(metadata$"traits"[[1]])
  trait_q <- data.frame(
    trait_q,
    "levels_value" = NA,
    "levels_description" = NA
  )

  trait_c <- as.data.frame(metadata$"traits"[[2]])
  trait_c <- trait_c[, -grep("^levels", colnames(trait_c))]

  trait_c <- data.frame(
    trait_c,
    "levels_value" = ".value",
    "levels_description" = ".descr"
  )

  trait_c <- rbind(trait_c, trait_c)

  sheets[["traits"]] <- rbind(trait_q, trait_c)

  sheets
}
