# load libraries ----
library(dplyr)
library(TSA)


# load data ----
daily_calls <- "" 


# define functions
add_waves <- function(df, var, period) {
  sym_var <- sym(var)
  
  df %>%
    mutate(
      sin6.5 = sin(2 * pi * !!sym_var / period),
      cos6.5 = cos(2 * pi * !!sym_var / period)
    )
}


# fit model ---
mod <- periodogram(daily_calls$calls)


# inspect model ---
cycles_calls <- data.frame(1 / mod$freq, mod$spec)

add_waves(harrass_fakemonth, var = "fakemonth", period = 6.5)