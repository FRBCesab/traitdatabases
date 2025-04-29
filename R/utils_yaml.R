# #' Check if a YAML file (map metadata) exists
# #'
# #' @noRd

# check_yaml_file <- function(path) {
#   if (!file.exists(path)) {
#     stop(
#       "The file '",
#       path,
#       "' doesn't exist",
#       call. = FALSE
#     )
#   }

#   invisible(NULL)
# }

#' Check if a key exists in a YAML file (named list)
#'
#' @noRd

check_key_in_yaml <- function(metadata, key) {
  check_named_list(metadata)
  check_character_arg(key)

  if (!(key %in% names(metadata))) {
    stop(
      "No key '",
      key,
      "' found in the YAML file",
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Import metadata template
#'
#' @noRd

read_yaml_template <- function() {
  yaml::read_yaml(
    file = system.file(
      file.path("templates", "metadata_template.yml"),
      package = "traitdatabases"
    )
  )
}


#' Convert YAML to list of data.frame
#'
#' @noRd

yaml_to_df <- function(metadata) {
  check_key_in_yaml(metadata, "status")
  check_key_in_yaml(metadata, "dataset")
  check_key_in_yaml(metadata, "traits")

  sheets <- list()

  sheets[["status"]] <- data.frame("status" = metadata$"status")

  sheets[["dataset"]] <- data.frame(
    "key" = names(unlist(metadata$"dataset")), # to handle sublevel taxonomy
    "value" = unlist(metadata$"dataset")
  )

  rownames(sheets[["dataset"]]) <- NULL

  traits <- lapply(metadata$"traits", function(x) {
    check_key_in_yaml(x, "name")
    check_key_in_yaml(x, "variable")
    check_key_in_yaml(x, "category")
    check_key_in_yaml(x, "type")

    traits <- as.data.frame(t(unlist(x)))

    if (traits$"type" == "categorical") {
      check_key_in_yaml(x, "levels")

      invisible(
        lapply(x$"levels", function(y) check_key_in_yaml(y, "value"))
      )

      invisible(
        lapply(x$"levels", function(y) check_key_in_yaml(y, "description"))
      )

      level_val_cols <- grep("^levels.value", colnames(traits))
      level_descr_cols <- grep("^levels.description", colnames(traits))

      categories <- data.frame(
        "name" = traits[["name"]],
        "levels.value" = unlist(traits[, level_val_cols]),
        "levels.description" = unlist(traits[, level_descr_cols])
      )

      traits <- traits[, -c(level_val_cols, level_descr_cols)]

      traits <- merge(traits, categories, by = "name")
    }

    traits
  })

  sheets[["traits"]] <- do.call(plyr::rbind.fill, traits)

  if (!("units" %in% colnames(sheets[["traits"]]))) {
    sheets[["traits"]]$"units" <- NA
  }

  if (!("levels.value" %in% colnames(sheets[["traits"]]))) {
    sheets[["traits"]]$"levels.value" <- NA
  }

  if (!("levels.description" %in% colnames(sheets[["traits"]]))) {
    sheets[["traits"]]$"levels.description" <- NA
  }

  col_order <- c(
    "name",
    "variable",
    "category",
    "type",
    "units",
    "levels.value",
    "levels.description"
  )

  sheets[["traits"]] <- sheets[["traits"]][, col_order]

  sheets
}


#' Clean database name
#'
#' @noRd

clean_database_name <- function(name) {
  name |>
    tolower() |>
    trimws() |>
    gsub("\\s+", "_", x = _) |>
    gsub("[[:punct:]]", "_", x = _) |>
    gsub("_+", "_", x = _) |>
    gsub("^_", "", x = _) |>
    gsub("_$", "", x = _)
}
