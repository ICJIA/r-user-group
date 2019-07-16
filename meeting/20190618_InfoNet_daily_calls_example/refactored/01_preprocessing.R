# load modules ----
utils <- source("utils.R")$value


# load libraries ----
pkgs <- c(
  "dplyr"
)

utils$install_and_load_pkgs(pkgs)

# use tidy/dplyr version of select, not MASS
select <- dplyr::select


# define globals ----
SA_CALLS <- utils$load_csv("SACalls.csv", dir = "hotline", col_types = "iDfidiffffff")
IL_MEAN_TEMP <- utils$load_csv()
DJIA <- utils$load_csv()


# define functions ----
df_to_contingency_tbl <- function(df, rownames) {
  row.names(df) <- df[[rownames]]
  df[[rownames]]<- NULL
  
  as.table(as.matrix(df))
}

prepare_us_holidays <- function(generated_holidays) {
  generated_holidays %>%
    filter(country == "US") %>%
    select(date = ds, holiday) %>%
    mutate(date = as.Date(date))
}

add_black_friday <- function(us_holidays) {
  blackfriday <-
    us_holidays %>%
    filter(holiday == "Thanksgiving") %>%
    mutate(
      date = date + 1,
      holiday = "Black Friday"
    )
  
  bind_rows(us_holidays, blackfriday) %>%
    arrange(date)
}

holidize <- function(df, datevar) {
  sym_datevar <- sym(datevar)
  
  us_holidays <-
    prophet::generated_holidays %>%
    prepare_us_holidays() %>%
    add_black_friday()
  
  levels_day_type <- c(
    "Normal",
    "Martin Luther King, Jr. Day",
    "Labor Day",
    "New Year's Day",
    "Weekend",
    "Memorial Day",
    "Independence Day",
    "Black Friday",
    "Christmas Day",
    "Thanksgiving"
  )
  
  df %>%
    left_join(us_holidays, by = datevar) %>%
    mutate(
      holiday = replace_na(holiday, "Normal Day"),
      weekday = lubridate::wday(!!sym_datevar, label = TRUE),
      month = lubridate::month(!!sym_datevar, label = TRUE),
      day_type = case_when(
        weekday %in% c("Sun", "Sat") ~ "Weekend",
        holiday != "Normal Day" ~ gsub(" (Observed)", "", holiday),
        TRUE ~ "Normal"
      ),
      workday = if_else(day_type == "Normal", 1, 0),
      day_type = ordered(day_type, levels = levels_day_type)
    )
}

prepare_all_sa_calls <- function(sa_calls) {
  sa_calls %>% 
    holidize("Date") %>%
    mutate(
      Age = na_if(Age, -1),
      call_duration = if_else(
        TotalTime < 2.25,
        (TotalTime %/% .25) * .25,
        2.25
      )
    )
}

count_daily_calls <- function(sa_calls, il_mean_temp) {
  all_dates <- tibble(
    date = seq(as.Date("2002-07-01"), as.Date("2019-04-30"), by="days")
  )
  
  daily_calls <-
    sa_calls %>%
    filter(ClientType == "Victim") %>%
    group_by(Date) %>%
    count(name = "Calls")
  
  all_dates %>%
    left_join(daily_calls, by = "date") %>%
    mutate(Calls = tidyr::replace_na(Calls, 0))
}

add_info_to_daily_calls <- function(daily_calls, il_mean_temp, djia) {
  daily_calls %>%
    left_join(il_mean_temp, by = "date") %>%
    left_join(djia, by = "date") %>%
    holidize("date") %>%
    arrange(Date) %>%
    tibble::rowid_to_column()
}

# run functions ----
