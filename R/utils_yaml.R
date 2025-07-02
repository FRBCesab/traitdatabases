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


#' Convert list of data.frame to YAML
#'
#' @noRd

df_to_yaml <- function(metadata) {
  check_key_in_yaml(metadata, "status")
  check_key_in_yaml(metadata, "dataset")
  check_key_in_yaml(metadata, "traits")

  yml <- list()

  yml[["status"]] <- metadata[["status"]][1, "status"]

  dataset <- metadata[["dataset"]]

  # Keep key order
  dataset[["split"]] <- dataset[["key"]] |>
    strsplit("\\.") |>
    lapply(function(x) x[1]) |>
    unlist()

  keys <- dataset[["split"]]
  keys <- keys[!duplicated(keys)]

  dataset[["key"]] <- lapply(
    1:nrow(dataset),
    function(x) {
      if (dataset[x, "key"] != dataset[x, "split"]) {
        dataset[x, "key"] <- gsub(
          paste0(dataset[x, "split"], "\\."),
          "",
          dataset[x, "key"]
        )
      }
      dataset[x, "key"]
    }
  )

  dataset <- split(dataset[, -3], dataset[["split"]])

  yml[["dataset"]] <- lapply(dataset, function(x) {
    if (nrow(x) == 1) {
      y <- x[1, "value"]
    } else {
      y <- as.list(x[["value"]])
      names(y) <- x[["key"]]
    }
    y
  })

  yml[["dataset"]] <- yml[["dataset"]][keys]

  traits <- metadata[["traits"]]

  keys <- colnames(traits)
  keys <- gsub("\\..*$", "", keys)
  keys <- keys[!duplicated(keys)]

  traits <- traits |>
    split(traits[["variable"]]) |>
    lapply(wide_to_long)

  traits <- lapply(traits, function(trait) {
    trait[["split"]] <- trait[["key"]] |>
      strsplit("\\.") |>
      lapply(function(x) x[1]) |>
      unlist()

    trait[["key"]] <- lapply(
      1:nrow(trait),
      function(x) {
        if (trait[x, "key"] != trait[x, "split"]) {
          trait[x, "key"] <- gsub(
            paste0(trait[x, "split"], "\\."),
            "",
            trait[x, "key"]
          )
        }
        trait[x, "key"]
      }
    )

    is_cat <- grep("[0-9]", trait[["split"]])

    if (length(is_cat) > 0) {
      trait[is_cat, "key"] <- gsub("[0-9]", "", trait[is_cat, "key"])
      trait <- trait[!duplicated(trait[["key"]]), ]
      # trait[["key"]] <- gsub("[0-9]", "", trait[["key"]])
      trait[["split"]] <- gsub("[0-9]", "", trait[["split"]])
    }

    is_cat <- grep("[0-9]", trait[["key"]])

    if (length(is_cat) > 0) {
      level_data <- unlist(trait[is_cat, "key"])
      var_names <- gsub("[0-9]", "", level_data)
      level_id <- lapply(1:length(var_names), function(i) {
        gsub(var_names[i], "", level_data[i])
      }) |>
        unlist() |>
        as.numeric()

      level_data <- data.frame("key" = var_names, "pos" = level_id)
      level_data$"key" <- factor(
        level_data$"key",
        levels = c("value", "description")
      )
      level_data$"key" <- as.numeric(level_data$"key")

      pos <- with(level_data, order(pos, key))

      trait <- rbind(trait[-is_cat, ], trait[is_cat[pos], ])
      trait[["key"]] <- gsub("[0-9]", "", trait[["key"]])
    }

    trait <- split(trait, trait[["split"]])

    trait <- lapply(trait, function(x) {
      if (nrow(x) == 1) {
        y <- x[1, "value"]
      } else {
        y <- as.list(x[["value"]])
        names(y) <- x[["key"]]
      }
      y
    })

    trait <- trait[keys]
  })

  names(traits) <- NULL

  yml[["traits"]] <- traits

  yml
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


#' Convert a data.frame from wide to long format
#'
#' @noRd

wide_to_long <- function(data) {
  data <- data |>
    as.list() |>
    unlist() |>
    as.data.frame()

  data <- data.frame("key" = rownames(data), "value" = data[, 1])

  rownames(data) <- NULL

  data
}
