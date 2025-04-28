#' Transform a yaml metadata to data.frame
#'
#' @description
#' This function transform a metadata written in yaml to
#' a metadata in data.frame format with three items:
#' `status`, `dataset`, and `traits`.
#' The structure of the metadata must follow the structure in `metadata_as_yaml()`
#'
#' @param x a `character` of length 1. It could be a filename with extension '.yaml' or a full yaml string.
#'
#' @return A list with three data.frames: `status`, `dataset`, and `traits`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' metadata_as_yaml() |> yaml_as_df()
#' }
yaml_as_df <- function(x) {
  # here 'x' could be a text or a filename
  check_character_arg(x)
  # might need some more testing
  if (substr(x, nchar(x) - 3, nchar(x)) == ".yml") {
    metadata <- yaml::read_yaml(file = x, readLines.warn = FALSE)
  } else {
    metadata <- yaml::read_yaml(text = x)
  }

  # load the expected structure of metadata
  skeleton <- metadata_as_yaml() |>
    yaml::read_yaml(text = _)

  # check the main structure of the metadata
  # make sure that status, dataset and traits are in metadata
  if (any(!names(skeleton) %in% names(metadata))) {
    miss <- paste(
      names(skeleton)[!names(skeleton) %in% names(metadata)],
      sep = " "
    )
    stop(paste(
      "Missing level",
      miss,
      "\n Check the expected structure of the yaml file with 'metadata_as_yaml()'"
    ))
  }

  sheets <- list()

  # status
  if (length(metadata$"status") != 1) {
    stop(paste("status should contain a single value"))
  } else {
    sheets[["status"]] <- data.frame("status" = metadata$"status")
  }

  # dataset
  expected_dataset <- names(unlist(skeleton$"dataset"))
  if (any(!expected_dataset %in% names(unlist(metadata$"dataset")))) {
    miss <- expected_dataset[
      !expected_dataset %in% names(unlist(metadata$"dataset"))
    ]
    warning(paste(
      "Missing field in 'dataset':",
      paste(miss, collapse = ","),
      "\n Check the expected structure of the yaml file with 'metadata_as_yaml()'"
    ))
  }

  sheets[["dataset"]] <- data.frame(
    "key" = names(unlist(metadata$"dataset")), # to handle sublevel taxonomy
    "value" = unlist(metadata$"dataset")
  )
  rownames(sheets[["dataset"]]) <- NULL
  # replace '.' by '_' for spelling harmonization
  sheets[["dataset"]]$key <- gsub("\\.", "_", sheets[["dataset"]]$key)

  # traits
  expected_traits <- unique(names(unlist(skeleton$"traits")))
  if (any(!expected_traits %in% names(unlist(metadata$"traits")))) {
    miss <- expected_traits[
      !expected_traits %in% names(unlist(metadata$"traits"))
    ]
    warning(paste(
      "Missing field in 'traits':",
      paste(miss, collapse = ","),
      "\n Check the expected structure of the yaml file with 'metadata_as_yaml()'"
    ))
  }
  simple_trait <- lapply(metadata$"traits", FUN = handle_traits_yml)

  sheets[["traits"]] <- data.frame(
    do.call(rbind, simple_trait)
  )

  sheets
}

#' Create a xlsx file from a yaml metadata
#'
#' @description
#' This function transform a metadata saved in yaml format
#' into a metadata saved in a xlsx file.
#'
#' @param name a `character` of length 1. The trait dataset identifier used to
#'   create files and folders. Should be short, explicit and without special
#'   characters (including accents).
#'
#' @param path a `character` of length 1. The folder name to stored the
#'   metadata template file in. Must exist.
#'   Default is the current directory.
#'
#' @param out_suffix a `character` of length 1. The suffix to be added to the output file.
#'
#' @param overwrite a `logical` of length 1. If `TRUE` overwrites the metadata
#'   template file.
#'   Default is `FALSE`.
#'
#' @return No return value.
#'
#' @export
#'
yaml_to_xlsx <- function(
  name,
  path = ".",
  out_suffix = '',
  overwrite = FALSE
) {
  check_character_arg(name)
  check_nonascii_char(name)
  check_character_arg(path)
  check_path_exists(path)
  check_logical_arg(overwrite)

  dir_path <- file.path(path, name)
  infile <- file.path(dir_path, paste0(name, "_metadata.yml"))
  outfile <- file.path(dir_path, paste0(name, "_metadata", out_suffix, ".xlsx"))

  if (file.exists(outfile) & !overwrite) {
    stop(
      "The file '",
      outfile,
      "' already exists.",
      "\nUse 'overwrite = TRUE' to replace its content."
    )
  }

  if (!file.exists(infile)) {
    stop(
      "The file '",
      infile,
      "' can not be found.",
      "\nMake sure 'path' and 'name' are spelled correctly."
    )
  }

  metadata <- yaml_as_df(infile)

  writexl::write_xlsx(x = metadata, path = outfile)
}

#' Create a yaml file from a xlsx metadata
#'
#' @description
#' This function transform a metadata saved in yaml format
#' into a metadata saved in a xlsx file.
#'
#' @param name a `character` of length 1. The trait dataset identifier used to
#'   create files and folders. Should be short, explicit and without special
#'   characters (including accents).
#'
#' @param path a `character` of length 1. The folder name to stored the
#'   metadata template file in. Must exist.
#'   Default is the current directory.
#'
#' @param out_suffix a `character` of length 1. The suffix to be added to the output file.
#'
#' @param overwrite a `logical` of length 1. If `TRUE` overwrites the metadata
#'   template file. Default is `FALSE`.
#'
#' @return No return value.
#'
#' @export
#'
xlsx_to_yaml <- function(
  name,
  path = ".",
  out_suffix = '',
  overwrite = FALSE
) {
  check_character_arg(name)
  check_nonascii_char(name)
  check_character_arg(path)
  check_path_exists(path)
  check_logical_arg(overwrite)

  dir_path <- file.path(path, name)
  infile <- file.path(dir_path, paste0(name, "_metadata.xlsx"))
  outfile <- file.path(dir_path, paste0(name, "_metadata", out_suffix, ".yml"))

  if (file.exists(outfile) & !overwrite) {
    stop(
      "The file '",
      outfile,
      "' already exists.",
      "\nUse 'overwrite = TRUE' to replace its content."
    )
  }

  if (!file.exists(infile)) {
    stop(
      "The file '",
      infile,
      "' can not be found.",
      "\nMake sure 'path' and 'name' are spelled correctly."
    )
  }

  status <- readxl::read_xlsx(path = infile, sheet = "status")
  dataset <- readxl::read_xlsx(path = infile, sheet = "dataset")
  traits <- readxl::read_xlsx(path = infile, sheet = "traits")

  # load the expected structure of metadata
  skeleton_df <- metadata_as_yaml() |>
    yaml_as_df()

  # status
  if (length(status) != 1) {
    stop(paste("'status' should contain a single value"))
  } else {
    yml_status <- paste(names(status), as.character(status), sep = ": ")
  }
  cat(yml_status, sep = "\n", file = outfile)

  # dataset
  cat("dataset:", sep = "\n", file = outfile, append = TRUE)

  if (any(names(dataset) != names(skeleton_df$dataset))) {
    stop(paste(
      "'dataset' should have two columns: ",
      paste(names(skeleton_df$dataset), collapse = ", ")
    ))
  }
  if (any(!skeleton_df$dataset$key %in% dataset$key)) {
    miss <- skeleton_df$dataset$key[!skeleton_df$dataset$key %in% dataset$key]
    warning(paste("Missing field in 'dataset':", paste(miss, collapse = ",")))
  }
  # make sure col separator is surrounded by ' '
  yml_dataset <- paste0("  ", dataset$key, ": ", dataset$value)
  cat(check_yaml(yml_dataset), sep = "\n", file = outfile, append = TRUE)

  # traits
  cat("traits:", sep = "\n", file = outfile, append = TRUE)
  if (any(!names(traits) %in% names(skeleton_df$traits))) {
    stop(paste(
      "'dataset' should have seven columns: ",
      paste(names(skeleton_df$traits), collapse = ", ")
    ))
  }
  col_levels <- grep("^levels_", names(traits))
  for (trait_i in unique(traits$variable)) {
    row_i <- which(traits$variable %in% trait_i)
    if (length(row_i) == 1) {
      yaml_i <- paste0(
        "  ",
        names(traits)[-col_levels],
        ": ",
        traits[row_i, -col_levels]
      )
    } else {
      key_row <- row_i[which.min(apply(
        is.na(traits[row_i, -col_levels]),
        1,
        sum
      ))]
      yaml_i <- paste0(
        "  ",
        names(traits)[-col_levels],
        ": ",
        traits[key_row, -col_levels]
      )
      var_level <- gsub("^levels_", "    ", names(traits)[col_levels])
      # add '-' at the stat of a new trait
      var_level[1] <- gsub("^    ", "  - ", var_level[1])
      yaml_level <- apply(
        traits[row_i, col_levels],
        1,
        function(x) paste(var_level, x, sep = ": ")
      )
    }
    # add '-' at the stat of a new trait
    yaml_i[1] <- gsub("^  ", "- ", yaml_i[1])
    cat(check_yaml(yaml_i), sep = "\n", file = outfile, append = TRUE)
    if (length(row_i) > 1) {
      cat("  levels:", sep = "\n", file = outfile, append = TRUE)
      cat(check_yaml(yaml_level), sep = "\n", file = outfile, append = TRUE)
    }
  }
}


#' Handy function to convert traits in yaml to df
#'
#' @noRd
handle_traits_yml <- function(x) {
  if ("levels" %in% names(x)) {
    # base trait information
    base_i <- x[-grep("^levels", names(x))]
    # add levels information
    levels_i <- do.call(rbind, x$"levels")
    colnames(levels_i) <- paste("levels", colnames(levels_i), sep = "_")
    levels_i <- apply(levels_i, 2, unlist)
    # merge the two together
    out_i <- data.frame(
      base_i,
      levels_i
    )
  } else {
    out_i <- data.frame(
      x,
      "levels_value" = NA,
      "levels_description" = NA
    )
  }
  return(out_i)
}

#' Function to transform NA and logical values for readibility in yaml format
#'
#' @noRd
check_yaml <- function(x) {
  # replace NA
  x <- gsub(": NA", ": .na", x)
  # replace TRUE and FALSE
  x <- gsub(": FALSE", ": no", x)
  x <- gsub(": TRUE", ": yes", x)
  # yaml doesn't like ending ','
  x <- gsub(",$", "','", x)
  return(x)
}
