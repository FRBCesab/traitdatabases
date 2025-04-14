#' Check if an argument is a character of length 1
#'
#' @noRd

check_character_arg <- function(string) {
  if (missing(string)) {
    stop(
      "Argument ",
      deparse(substitute(string)),
      " is required",
      call. = FALSE
    )
  }

  if (is.null(string)) {
    stop(
      "Argument ",
      deparse(substitute(string)),
      " is required",
      call. = FALSE
    )
  }

  if (!is.character(string)) {
    stop(
      "Argument ",
      deparse(substitute(string)),
      " must be a character",
      call. = FALSE
    )
  }

  if (length(string) != 1) {
    stop(
      "Argument ",
      deparse(substitute(string)),
      " must be of length 1",
      call. = FALSE
    )
  }

  if (is.na(string)) {
    stop(
      "Argument ",
      deparse(substitute(string)),
      " cannot be NA",
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Check if an argument is a numeric of length 1
#'
#' @noRd

check_numeric_arg <- function(number) {
  if (missing(number)) {
    stop(
      "Argument ",
      deparse(substitute(number)),
      " is required",
      call. = FALSE
    )
  }

  if (is.null(number)) {
    stop(
      "Argument ",
      deparse(substitute(number)),
      " is required",
      call. = FALSE
    )
  }

  if (!is.numeric(number)) {
    stop(
      "Argument ",
      deparse(substitute(number)),
      " must be a numeric",
      call. = FALSE
    )
  }

  if (length(number) != 1) {
    stop(
      "Argument ",
      deparse(substitute(number)),
      " must be of length 1",
      call. = FALSE
    )
  }

  if (is.na(number)) {
    stop(
      "Argument ",
      deparse(substitute(number)),
      " cannot be NA",
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Check if an argument is a logical of length 1
#'
#' @noRd

check_logical_arg <- function(boolean) {
  if (missing(boolean)) {
    stop(
      "Argument ",
      deparse(substitute(boolean)),
      " is required",
      call. = FALSE
    )
  }

  if (is.null(boolean)) {
    stop(
      "Argument ",
      deparse(substitute(boolean)),
      " is required",
      call. = FALSE
    )
  }

  if (!is.logical(boolean)) {
    stop(
      "Argument ",
      deparse(substitute(boolean)),
      " must be a logical",
      call. = FALSE
    )
  }

  if (length(boolean) != 1) {
    stop(
      "Argument ",
      deparse(substitute(boolean)),
      " must be of length 1",
      call. = FALSE
    )
  }

  if (is.na(boolean)) {
    stop(
      "Argument ",
      deparse(substitute(boolean)),
      " cannot be NA",
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Check if a string contains non ASCII characters
#'
#' @noRd

check_nonascii_char <- function(string) {
  check_character_arg(string)

  if (length(grep("[^ -~]", string)) > 0) {
    stop(
      "The argument ",
      deparse(substitute(string)),
      " should not contain non ",
      "ASCII characters",
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Check if a path exists
#'
#' @noRd

check_path_exists <- function(path) {
  check_character_arg(path)
  if (!dir.exists(path)) {
    stop(
      "The path '",
      path,
      "' does not exist",
      call. = FALSE
    )
  }

  invisible(NULL)
}


#' Check value of 'format' argument
#'
#' @noRd

check_format_value <- function(format) {
  check_character_arg(format)
  if (!(format %in% c("xlsx", "yaml"))) {
    stop(
      "The argument ",
      deparse(substitute(format)),
      " should be one of 'xlsx' and 'yaml'",
      call. = FALSE
    )
  }

  invisible(NULL)
}
