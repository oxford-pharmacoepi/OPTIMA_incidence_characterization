# Shiny utils ----
selectors <- function(data, prefix, columns, multiple = TRUE, default = list()) {
  def <- function(col) {
    if (col %in% names(default)) {
      x <- default[[col]]
    } else {
      x <- choic(col)
      if (!multiple) {
        x <- first(x)
      }
    }
    return(x)
  }
  choic <- function(col) {
    data[[col]] %>% unique() %>% sort()
  }
  purrr::map(columns, ~ pickerInput(
    inputId = paste0(prefix, "_", .),
    label = stringr::str_to_sentence(gsub("_", " ", .)),
    choices = choic(.),
    selected = def(.),
    options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
    multiple = multiple,
    inline = TRUE
  ))
}

plotSelectors <- function(prefix, choices, multiple = TRUE, default = list(), type = c("color", "facet_by")) {
  purrr::map(type, ~ pickerInput(
    inputId = paste0(prefix, "_", .),
    label = stringr::str_to_sentence(gsub("_", " ", .)),
    choices = choices,
    selected = default[[.]],
    options = list(`actions-box` = multiple, size = 10, `selected-text-format` = "count > 3"),
    multiple = multiple,
    inline = TRUE
  ))
}

filterData <- function(data, prefix, input) {
  cols <- colnames(data)
  cols <- cols[paste0(prefix, "_", cols) %in% names(input)]
  for (col in cols) {
    data <- data %>%
      dplyr::filter(.data[[col]] %in% .env$input[[paste0(prefix, "_", col)]])
  }
  return(data)
}

niceColumnNames <- function(x, cols = everything()) {
  x %>% rename_with(.fn = ~ stringr::str_to_sentence(gsub("_", " ", .x)), .cols = cols)
}

niceNum <- function(x, dec = 0) {
  trimws(format(round(as.numeric(x), dec), big.mark = ",", nsmall = dec, scientific = FALSE))
}

# PhenotyperR utils ----
formatMarkdown <- function(x) {
  lines <- strsplit(x, "\r\n\r\n") |> unlist()
  getFormat <- function(line) {
    if (grepl("###", line)) {return(h3(gsub("###", "", line)))} 
    else {h4(line)} 
  }
  purrr::map(lines, ~ getFormat(.))
}

# formatLog <- function(x) {
#   lines <- strsplit(x, "\n") |> unlist()
#   getFormat <- function(line) {
#     line <- strsplit(line, ":") |> unlist()
#     return(list(h4(line[1]), h5(paste0(gsub(" elapsed", "", line[2])))))
#   }
#   purrr::map(lines, ~ getFormat(.))
# }

formatLog <- function(x) {
  # Ensure that the last line is processed by appending a newline character if missing
  if (substr(x, nchar(x), nchar(x)) != "\n") {
    x <- paste0(x, "\n")
  }

  # Split the input string into individual lines based on newline characters
  lines <- strsplit(x, "\n")[[1]]

  # Function to format each line
  getFormat <- function(line) {
    # Split the line by colon and remove excess spaces
    line_parts <- strsplit(line, ":\\s*")[[1]]

    # Check if the line was properly split into two parts
    if (length(line_parts) == 2) {
      # Remove "elapsed" and format the time value
      time_value <- gsub(" sec elapsed", "", line_parts[2])
      return(list(h4(line_parts[1]), h5(time_value)))
    } else {
      # If line is malformed, return an error message
      return(list(h4(line), h5("Error parsing")))
    }
  }

  # Apply the helper function to each line
  purrr::map(lines, ~ getFormat(.x))
}


lscToSummarisedResult <- function(lsc) {
  lsc %>%
    mutate(
      "result_type" = "lsc",
      "package_name" = "PatientProfiles",
      "package_version" = "0.6.2",
      "variable_name" = variable,
      "estimate_name" = estimate_type,
      "estimate_type" = if_else(
        estimate_name == "count", "integer", "percentage"
      ),
      "estimate_value" = estimate,
      "group_name" = snakecase::to_snake_case(group_name),
      "strata_name" = tolower(strata_name),
      "strata_level" = tolower(strata_level)
    ) %>%
    # uniteAdditional(
    #   cols = c("table_name", "type", "concept")
    # ) %>%
    select(
      "cdm_name", "result_type", "package_name", "package_version",
      "group_name", "group_level", "strata_name", "strata_level",
      "variable_name", "variable_level", "estimate_name", "estimate_type",
      "estimate_value", "additional_name", "additional_level"
    )
  
}
