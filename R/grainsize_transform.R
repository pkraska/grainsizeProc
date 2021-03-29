#' grainsize_load
#'
#' @param x .CSV file using the template included with this package
#'
#' @return
#' @export
#' @depends
#' @import tidyverse, lubridate
#' @examples


grainsize_transform <- function(x) {
  raw <- read_csv("data/example.csv", col_names = FALSE) %>%
    filter(X1 != is.na(X1))

  # pull project, region, and crew leader name from original file
  proj <- as.character(raw[1, 2])
  reg <- as.character(raw[2, 2])
  lead <- as.character(raw[3, 2])

  # create vector of final column names, and compare to what you should have,
  # exit if not the right variable names.
  col_n <- tolower(as.vector(t(raw[, 1])))
  test_col_n <- col_n[1:24]
  good_col_names <-
    c(
      "project",
      "region",
      "leader",
      "lat",
      "long",
      "time (gmt)",
      "date",
      "event",
      "startdepth",
      "enddepth",
      "sounding",
      "analysis",
      "collection",
      "preservation",
      "storage",
      "sample type",
      "ancillary data",
      "units",
      "ids",
      "core #",
      "filter #",
      "filter spm (g/m^3)",
      "comment",
      "diameter"
    )

  if (!all(test_col_n == good_col_names)) {
    warning("Data is not in expected format, please make sure you are using the correct template!")
    stop()
  } else {
    # create new dataframe of transposed data to be melted
    df <- raw %>%
      t %>%
      as.data.frame(., stringsAsFactors = FALSE) %>%
      slice(2:n()) %>%
      filter(V4 != is.na(V4)) %>%   #filter out NAs introduced by the excel spreadsheet
      rename_with(~ col_n) %>%  # Rename solumns using the vector of scraped column names you created earlier
      remove_rownames() %>% #Remove rownames that correspond to matrix column names prior to being transposed
      replace(. == "N/A", NA) %>%
      mutate(
        project = proj,
        # Set project variables and format Datetime correctly from df, convert
        # to seconds since 1900, then combine with 'IDs', 'Core #', and 'Filter
        # #' for unique id (`uid`)
        region = reg,
        leader = lead,
        datetime = dmy_hms(paste(date, `time (gmt)`, sep = " ")),
        uid = paste(as.character(datetime),
                    ids,
                    `core #`,
                    `filter #`,
                    sep = "_")
      ) %>%
      select(uid,
             project,
             region,
             leader,
             lat,
             long,
             datetime,
             everything()) %>%
      select(-date, -`time (gmt)`, -diameter)

    df_head <- df %>%
      select(1:23) %>%
      glimpse

    df_data <- df %>%
      select(uid, 24:ncol(.)) %>%
      pivot_longer(-uid, names_to = "diameter", values_to = "measurement") %>%
      mutate(measurement = as.numeric(measurement)) %>%
      glimpse
  }
}
