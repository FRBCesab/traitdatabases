

td_download_data_file <- function(name, path = ".") {
  check_character_arg(name)
  check_nonascii_char(name)
  check_character_arg(path)
  check_path_exists(path)

  dir_path <- file.path(path, name)

  if (!dir.exists(dir_path)) {
    stop("The directory '", dir_path, "' does not exist", call. = FALSE)
  }

  filename <- file.path(dir_path, paste0(name, "_metadata.yml"))

  if (!file.exists(filename)) {
    stop("The metadata file '", filename, "' does not exist", call. = FALSE)
  }

  metadata <- yaml::read_yaml(filename)

  check_key_in_yaml(metadata$"dataset", "manual_download")

  manual_download <- metadata$"dataset"$"manual_download"

  if (manual_download) {
    message("The dataset for '", name, "' needs to be downloaded manually")
    return(NULL)
  }

  check_key_in_yaml(metadata$"dataset", "file_url")
  check_key_in_yaml(metadata$"dataset", "file_name")
  check_key_in_yaml(metadata$"dataset", "file_extension")

  file_url <- metadata$"dataset"$"file_url"
  file_name <- metadata$"dataset"$"file_name"
  file_extension <- metadata$"dataset"$"file_extension"

  download_file(name, dir_path, file_url, file_name)

  # extract_zip()
  invisible(NULL)
}


#' Download a file from an URL
#'
#' @noRd

download_file <- function(name, path, url, filename) {
  path <- file.path(path, "raw-data")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  if (!(filename %in% list.files(path))) {
    utils::download.file(
      url = url,
      destfile = file.path(path, filename),
      mode = "wb",
      quiet = TRUE
    )
  }

  invisible(NULL)
}
