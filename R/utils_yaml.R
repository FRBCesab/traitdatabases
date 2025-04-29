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

metadata_as_df <- function() {
  metadata <- read_yaml_template()

  sheets <- list()

  sheets[["status"]] <- data.frame("status" = "draft")

  sheets[["dataset"]] <- data.frame(
    "key" = names(unlist(metadata$"dataset")), # to handle sublevel taxonomy
    "value" = unlist(metadata$"dataset")
  )

  rownames(sheets[["dataset"]]) <- NULL
  # replace '.' by '_' for spelling harmonization
  sheets[["dataset"]]$key <- gsub("\\.", "_", sheets[["dataset"]]$key)

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
