library(odbc)

con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "SPAC2SVR",
  Port = 1433
)

get_chri_tbl <- function(dbname, tbl)
  dplyr::tbl(con, dbplyr::in_schema("AnnualPulls.dbo", tbl))

# export
list(
  arrests = get_chri_tbl("arrests"),
  arrestcharges = get_chri_tbl("arrestcharges")
  courtdisps = get_chri_tbl("courtdisps")
  sentences = get_chri_tbl("sentences")
)
