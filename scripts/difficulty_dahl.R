item_difficulty <- function(x, maximum_value = NULL) {
  # find general maximum of scale
  if (is.null(maximum_value)) {
    maximum_value <- suppressWarnings(max(vapply(x, max, numeric(1L), na.rm = TRUE)))
  } else if (!is.na(maximum_value) && !is.numeric(maximum_value)) {
    insight::format_error("`maximum_value` must be a numeric value, indicating the maximum value of an item.")
  }
  
  d <- vapply(x, function(.x) {
    # general maximum value, or per-item maximum value?
    if (is.na(maximum_value)) {
      max_val <- max(.x, na.rm = TRUE)
    } else {
      max_val <- maximum_value
    }
    .x <- .x[!is.na(.x)]
    round(sum(.x) / (max_val * length(.x)), 3)
  }, numeric(1))
  
  # ideal item item_difficulty
  fun.diff.ideal <- function(.x) {
    # general maximum value, or per-item maximum value?
    if (is.na(maximum_value)) {
      max_val <- max(.x, na.rm = TRUE)
    } else {
      max_val <- maximum_value
    }
    p <- 1 / max_val
    round(p + (1 - p) / 2, 3)
  }
  
  di <- vapply(x, fun.diff.ideal, numeric(1))
  
  structure(
    class = c("item_difficulty", "data.frame"),
    data.frame(
      Item = colnames(x),
      Difficulty = d,
      Ideal = di,
      stringsAsFactors = FALSE
    )
  )
}
