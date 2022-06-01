# Library needed for database connection.
library(RPostgres)

# Clears the global environment
# rm(list = ls())

# Setting working directory

## NB! Add password and username to the "account.R" file to be able to access WRDS

#' Connect to the WRDS database
#'
#' Creates a connection to the WRDS database with login information
#' supplied in a separate R-file: account.R containing a named
#' list: 'account <- list(user = "username", password = "password")'
#' This function should in general only be called outside of a function
#' if the connection to the database times out.
#'
#' @return The function does not return anything by itself, but instead
#' adds an object named wrds to the environment, this object is
#' used by query methods to access the database.
connect_to_wrds <- function() {

  print("Connecting to WRDS...")

  # Connects to the database and adds the object wrds to the global enviromnent.
  # this object is then used by query methods to access the database.
  wrds <<- dbConnect(
    Postgres(),
    host = 'wrds-pgdata.wharton.upenn.edu',
    port = 9737,
    dbname = 'wrds',
    sslmode = 'require',
    user = account$user,
    password = account$password
  )
  print("Connected.")
}

#' Query the database
#'
#' Query the database using postgres syntax. Whatever query is sent
#' into this function in the query field gets sent to the database
#' given that the query itself is sound,
#'
#' NB! worth to set the limit to 10-20 when testing syntax.
#'
#' @param query - character, Postgres syntax query that should be sent to the database.
#' @param limit - double, number of rows to return, default unlimited.
#'
#' @return dataset returned by the query.
#'
#' @export
wrds_query <- function(query, limit = -1) {
  # Checks if the object wrds exist, if not generates it.
  if (!exists("wrds")) {
    connect_to_wrds()
  }

  cat("Sending Query: \n", query, "\n")
  # Handel querying of data
  res <- dbSendQuery(wrds, query)
  data <- dbFetch(res, n = limit)
  dbClearResult(res)
  return(data)
}

#' Automates the query syntax.
#'
#' Creates a simple postgres query for fetching data given params
#'
#' @param lib - character, library to query. (crsp, comp)
#' @param dataset - character, dataset to query. (dsf, msf, funda)
#' @param columns - character vector, columns to return, defaults to all columns.
#' @param idcol - character, column in dataset to identify which data to return, defaults to NULL
#' @param id - character vector, values of idcol the query should return, defaults to NULL
#' @param form - character, start date of the query, defaults to NULL if you want to query all time
#' @param to - character, end date of the query, defaults to NULL if you want to query all time
#' @param date_column - character, name of date column in dataset, defaults to date
#'
#' @return dataset, with the results from the query
#'
#' @example
#' query_creator(
#'   lib = "crsp",
#'   dataset = "msf",
#'   columns = "ret",
#'   idcol = "cusip",
#'   id = "02313510",
#'   from = "2014-01-01",
#'   to = "2015-01-01")
#' Returns monthly return for Amazon in the year 2014.
#'
#' @export
query_creator <- function(lib, dataset, columns = "*",
                          idcol = NULL, id = NULL, from = NULL, to = NULL, date_column = "date") {

  # Checks if a column limitation or a time limitation is supplied.
  columnlimit <- !is.null(idcol) & !is.null(id)
  timelimit <- !is.null(from) & !is.null(to)

  # Allows for arbitrary amount of columns and id's to be queried.
  id <- paste(id, collapse = "','")
  columns <- paste(columns, collapse = ", ")
  query <- paste0(
    "SELECT ", columns,
    " FROM ", lib, ".", dataset
  )

  # Case when there is only a columnlimit
  if (columnlimit & !timelimit) {
    query <- paste0(
      query,
      " WHERE CAST(", idcol, " AS varchar) IN ", "('", id, "')"
    )
  }

  # Case when there is only a timelimit
  if (timelimit & !columnlimit) {
    query <- paste0(
      query,
      " WHERE ", date_column, " between ", "'", from, "'",
      " AND ", "'", to, "'"
    )
  }

  # Case when there is both a time and column limit.
  if (timelimit & columnlimit) {
    query <- paste0(
      query,
      " WHERE CAST(", idcol, " AS varchar) IN ", "('", id, "')",
      " AND ", date_column, " between ", "'", from, "'",
      " AND ", "'", to, "'"
    )
  }

  wrds_query(query)
}


#' Filesystem lookup
#'
#' Looksup libraries/datasets/columns
#' By default returns all the available libraries.
#' Practial function for having a peek at what can be found
#' inside different levels of the database.
#'
#' @param lib - String, library name in the database
#' @param dataset - String, dataset name in corresponding library
#'
#' @return Character vector.
#' default - All available libraries in the database.
#' If supplied lib - All available dataset in library
#' If supplied lib and dataset - All available columnames in dataset.
#'
#' @example
#' wrds_lookup("crsp") - returns names of all datasets in CRSP library
#' wrds_lookup("crsp", "dsf") - returns names of all columns in dataset dsf in library crsp
#' wrds_lookup("comp") - returns names of all datasets in the Compustat library
#' wrds_lookup("comp", "funda") - returns names of all columns in dataset funda in library comp
wrds_lookup <- function(lib = NULL, dataset = NULL) {
  if (is.null(lib) & is.null(dataset)) {
    query <- paste0("
  SELECT DISTINCT table_schema
  FROM information_schema.tables
  WHERE table_type = 'VIEW'
  OR table_type = 'FOREIGN TABLE'
  ORDER BY table_schema"
    )
  } else if (is.null(dataset)) {
    query <- paste0("
  SELECT DISTINCT table_name
  FROM information_schema.columns
  WHERE table_schema = '", lib, "'
  ORDER BY table_name"
    )
  } else {
    query <- paste0("
  SELECT column_name
  FROM information_schema.columns
  where table_schema = '", lib, "'
  AND table_name = '", dataset, "'
  ORDER BY column_name"
    )
  }
  wrds_query(query)
}


# General identifier: CUSIP (Could change over time)
# CRSP unique identifiers: PERMCO (company), PERMNO (Stock class)
# Treasury: kycrspid, kytreasno
# Compustat Global Company Key: GVKEY

# Sample runs, will not run unless this file is called directly
if (sys.nframe() == 0) {
  # Setting working directory to location of file.
  rstudioapi::getSourceEditorContext()$path |>
    dirname() |>
    setwd()

  test_data <- NULL
  test_data$cusip_amazon <- "02313510"
  test_data$cusip_apple <- "03783310"
  test_data$cusip <- c(test_data$cusip_apple, test_data$cusip_amazon)
  test_data$from <- "2016-01-01"
  test_data$to <- "2022-01-01"

  # wrds_lookup("crsp","msf")
  # Amazon, Apple stock data
  amazon_apple_stock <- query_creator(
    lib = "crsp",
    dataset = "msf",
    idcol = "cusip",
    id = test_data$cusip,
    from = test_data$from,
    to = test_data$to
  )

  # wrds_lookup("crsp", "tfz_dly")
  # kycrspid = YYYYMMDD.TCCCCE, YYYY: Maturity Year, MM: Maturity Month, DD: Maturity day
  # T: Type, CCCC: Integer part of Coupon Rate (COUPRT * 100) E: Uniqueness Number (UNIQ)
  # Write directly to file.
  # Treasury information
  query_creator(
    lib = "crsp",
    dataset = "tfz_dly",
    columns = c("kycrspid", "kytreasno", "caldt", "tdbid", "tdask"),
    from = "2010-01-03",
    to = "2010-01-04",
    date_column = "caldt"
  ) |>
    write.csv("treasury_20100104.csv")

  # wrds_lookup("comp", "funda")
  # Fetches balance data for Apple
  Apple_balance <- query_creator(
    lib = "comp",
    dataset = "funda",
    idcol = "tic",
    id = "AAPL",
    from = "2016",
    to = "2018",
    date_column = "fyear"
  )

}

