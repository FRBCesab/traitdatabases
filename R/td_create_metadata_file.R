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
#' @param overwrite a `logical` of length 1. If `TRUE` overwrites the metadata
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

    metadata <- read_yaml_template()

    if (format == "yaml") {
      metadata <- gsub("\\.dataset_key", name, x = metadata)

      cat(metadata, file = filename, append = FALSE)
    } else {
      metadata <- yaml_to_df(metadata)
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
