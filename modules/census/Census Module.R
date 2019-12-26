#### Module Description -----

#### ICJIA Census and ACS Age x Gender x Ethnicity Module v.9 

# What does this module do:

# Version .9:
# 1. Load Illinois Census data into R, specifically the Age x Gender x Ethnicity by Block (census) or Tract (acs)

# 2. Provide convenience functions for reading/decoding this data at basic level, e.g. decode Census variables


# Next Version (1.0)

# 3. Cross-reference different geographic areas within Illinois, summarize block level data

#     a. County
#     b. ZIP code
#     c. official places
#     c. eventually non-census regions, such as police districts

# 4. Provide convenience functions for random sampling

### Load libraries etc. -------

library(tidycensus) # census data connection
# setup the key right after tidycensus
# api.key.install(key="616907b994896c1dde3f6ba7e408703e51e9fd26") is Karl Gruschow's
census_api_key(
  key = "616907b994896c1dde3f6ba7e408703e51e9fd26",
  install = TRUE,
  overwrite = TRUE
)
library(tidyverse) # because useful
library(doParallel)
registerDoParallel()
library(data.table)
library(foreach)

### load the variables from the census API for the latest Census and ACS ----
# historically these have been constant
All_Census_Vars <- load_variables(2010, "sf1", cache = TRUE)
All_ACS_VARS <- load_variables(2018, "acs5", cache = TRUE)

varsP12 <- subset(All_Census_Vars, grepl("^P012", All_Census_Vars$name))
varsB1001 <- subset(All_ACS_VARS, grepl("^B01001", All_ACS_VARS$name))

### Functions for loading census and ACS block and block group level data for Illinois ----

get_p12_blocks_by_county <- function (x, y = 2010) {
  get_decennial(
    geography = "block",
    variables = varsP12$name,
    year = y,
    state = 17,
    county = x
  )
}

get_acs_tract_by_county <- function (x, y = 2018) {
  get_acs(
    geography = "tract",
    variables = varsB1001$name,
    survey="acs5",
    year = y,
    state = 17,
    county = x
  )
}

load_illinois_blocks_by_county <- function (
  counties = seq(1, 203, 2),
  year = 2010,
  cutoff = 1,
  postprocess = TRUE
) {

  # in current version, this loads illinois block level p12 data,
  # from the 2010 census by county (default all, otherwise list),
  # by default it only returns rows with a value of 1 or higher,
  # meaning at least 1 person per row.

  # it is resistant to basic errors in the API and will always return
  # a list of any failed arguments based on the actual exported file

  output <- foreach(
    county = counties,
    .errorhandling = "remove",
    .final = rbindlist
  ) %do% {
    by_county <- data.table(get_p12_blocks_by_county(county, year))
    by_county[value >= cutoff, c(1, 3, 4)]
  }

  counties_success <- as.numeric(unique(output[, substr(GEOID, 3, 5)]))
  print("Failed counties, in FIPS code:")
  print(counties[!(counties %in% counties_success)])

  if (postprocess) {
    output[, .(
      GEOID,
      value,
      gender = gender_of_p(variable),
      race = ethnicity_of_p(variable),
      age = age_of_p(variable)
    )]
  } else {
    output
  }
}

load_illinois_acs_by_county <- function(
  counties = seq(1, 203, 2),
  year = 2018,
  cutoff = 1,
  postprocess = TRUE
) {

  # this is essentially the same as blocks, except also year 

  output <- foreach(
    county = counties,
    .errorhandling = "remove",
    .final = rbindlist
  ) %do% {
    by_county <- data.table(get_acs_tract_by_county(county, year))
    by_county[estimate >= cutoff, c(1, 3, 4)]
  }

  counties_success <- as.numeric(unique(output[, substr(GEOID, 3, 5)]))
  print("Failed counties, in FIPS code:")
  print(counties[!(counties %in% counties_success)])
  
  if (postprocess) {
    output[, .(
      GEOID,
      estimate,
      gender = gender_of_b(variable),
      race = ethnicity_of_b(variable),
      age = age_of_b(variable)
    )]
  } else {
    output
  }
}


### optional debug of census API calls -----

# these test the basic loading functions, inclusive error handling, because illinois FIPS are only odd,
# so a full half the counties are invalid, works well as of 12/23/19

# bad_arg <- load_illinois_blocks_by_county(1:4)

# test_acs <- load_illinois_acs_by_year_X_county(2018,(1:4))


### 

##### Census Demographic Variable processing ----

# extract letter as string from P12 variable name
letter_of_p <- function(x) {
  if_else(nchar(x) == 8, substr(x, 5, 5), '')
}

# extract numeric portion of P12 variable name (age/gender category)
number_of_p <- function(x){
  if_else(nchar(x) == 8, substr(x, 7, 8), substr(x, 6, 7))
}

age_of_p <- function(x) {
  pn <- as.numeric(number_of_p(x))
  pn <- if_else(between(pn, 27, 49), pn - 24, pn)

  case_when(
    pn == 1 ~ 'Total',
    pn == 2 ~ 'Total',

    # these are in rough probabilistic order
    # because we expect most folks to be
    # in 20s or 30s, then 40s, then 50s
    # should process faster on average
    # because case operates in order

    pn == 12 ~ '30-34',
    pn == 13 ~ '35-39',
    pn == 11 ~ '25-29',
    pn == 10 ~ '22-24',
    pn == 14 ~ '40-44',
    pn == 9 ~ '21',
    pn == 8 ~ '20',
    pn == 7 ~ '18-19',
    pn == 15 ~ '45-49',
    pn == 16 ~ '50-54',
    pn == 17 ~ '55-59',

    # now ordered
    pn == 3 ~ '<5',
    pn == 4 ~ '5-9',
    pn == 5 ~ '10-14',
    pn == 6 ~ '15-17',
    # break and resume ordered

    pn == 18 ~ '60-61',
    pn == 19 ~ '62-64',
    pn == 20 ~ '65-66',
    pn == 21 ~ '67-69',
    pn == 22 ~ '70-74',
    pn == 23 ~ '75-79',
    pn == 24 ~ '80-84',
    pn == 25 ~ '85&older'
  )
}


gender_of_p <- function(x) {
  pn <- as.numeric(number_of_p(x))

  case_when(
    between(pn, 2, 25) ~ 'Male',
    between(pn, 26, 49) ~ 'Female',
    TRUE ~ 'Total'
  )
}

gender_of_b <- function(x) {
  pn <- as.numeric(number_of_b(x))

  case_when(
    letter_of_b(x) == '' & between(pn, 2, 25) ~ 'Male',
    letter_of_b(x) == '' & between(pn, 26, 49) ~ 'Female',
    letter_of_b(x) != '' & between(pn, 2, 16) ~ 'Male',
    letter_of_b(x) != '' & between(pn, 17, 31) ~ 'Female',
    TRUE ~ 'Total'  
  )
}


ethnicity_of_p <- function(x) {
  pl <- letter_of_p(x)

  case_when(
    pl == '' ~ 'Total', # rapidly map the P12 total table
    pl == 'I' ~ 'White (not-H)',
    pl == 'B' ~ 'Black',
    pl == 'H' ~ 'Latinx',
    pl == 'D' ~ 'Asian',
    pl == 'G' ~ 'Multiracial',
    pl == 'E' ~ 'Hawaiian-PI',
    pl == 'C' ~ 'Native Ame.',
    pl == 'F' ~ 'Other Race',
    pl == 'A' ~ 'White (both Hispanic and Not)'
    # for infoNet, ideally we ignore
    # ote that it simply ignores A (White including Hispanic)
    # and F (Other Race) because those are not mappable to InfoNet
  )
}


letter_of_b <- function(x) {
  if_else(nchar(x) == 11, substr(x, 7, 7), '')
}

ethnicity_of_b <- function(x) {
  pl <- letter_of_b(x)

  case_when(
    pl == '' ~ 'Total', # rapidly map the P12 total table
    pl == 'H' ~ 'White (not-H)',
    pl == 'B' ~ 'Black',
    pl == 'I' ~ 'Latinx',
    pl == 'D' ~ 'Asian',
    pl == 'G' ~ 'Multiracial',
    pl == 'E' ~ 'Hawaiian-PI',
    pl == 'C' ~ 'Native Ame.',
    pl == 'F' ~ 'Other Race',
    pl == 'A' ~ 'White (both Hispanic and Not)'
    # note that it simply ignores A (White including Hispanic)
    # and F (Other Race) because those are not mappable to InfoNet
  )
}

number_of_b <- function(x){
  if_else(nchar(x) == 11, substr(x, 10, 11), substr(x, 9, 10))
}

age_of_b <- function(x) {
  bn <- as.numeric(number_of_b(x))
  bl <- letter_of_b(x)
  output <- case_when(
    bl == '' & between(bn, 27, 49) ~ bn - 24,
    bl == '' ~ bn,
    bl != '' & between(bn, 17, 31) ~ bn - 15,
    TRUE ~ bn
  )

  case_when(
    output == 1 ~ 'Total',
    output == 2 ~ 'Total',

    # these are in rough probabilistic order
    # because we expect most folks to be
    # in 20s or 30s, then 40s, then 50s
    # should process faster on average
    # because case operates in order

    bl == '' & output == 12 ~ '30-34',
    bl == '' & output == 13 ~ '35-39',
    bl == '' & output == 11 ~ '25-29',
    bl == '' & output == 10 ~ '22-24',
    bl == '' & output == 14 ~ '40-44',
    bl == '' & output == 9 ~ '21',
    bl == '' & output == 8 ~ '20',
    bl == '' & output == 7 ~ '18-19',
    bl == '' & output == 15 ~ '45-49',
    bl == '' & output == 16 ~ '50-54',
    bl == '' & output == 17 ~ '55-59',

    # now ordered
    bl == '' & output == 3 ~ '<5',
    bl == '' & output == 4 ~ '5-9',
    bl == '' & output == 5 ~ '10-14',
    bl == '' & output == 6 ~ '15-17',
    # break and resume ordered

    bl == '' & output == 18 ~ '60-61',
    bl == '' & output == 19 ~ '62-64',
    bl == '' & output == 20 ~ '65-66',
    bl == '' & output == 21 ~ '67-69',
    bl == '' & output == 22 ~ '70-74',
    bl == '' & output == 23 ~ '75-79',
    bl == '' & output == 24 ~ '80-84',
    bl == '' & output == 25 ~ '85&older',

    # bl is not ''

    # now ordered
    bl != '' & output == 3 ~ '<5',
    bl != '' & output == 4 ~ '5-9',
    bl != '' & output == 5 ~ '10-14',
    bl != '' & output == 6 ~ '15-17',
    bl != '' & output == 7 ~ '18-19',
    bl != '' & output == 8 ~ '20-24',
    bl != '' & output == 9 ~ '25-29',
    bl != '' & output == 10 ~ '30-34',
    bl != '' & output == 11 ~ '35-45',
    bl != '' & output == 12 ~ '45-54',
    bl != '' & output == 13 ~ '55-64',
    bl != '' & output == 14 ~ '65-74',
    bl != '' & output == 15 ~ '75-84',
    bl != '' & output == 16 ~ '85 and older'
  )
}

# Module export list ----
list(
  acs_tracts_by_county = load_illinois_acs_by_county,
  census_blocks_by_county = load_illinois_blocks_by_county,
  B = list(
    age = age_of_b,
    ethnicity = ethnicity_of_b,
    gender = gender_of_b,
    vars = varsB1001
  ),
  P = list (
    age = age_of_p,
    ethnicity = ethnicity_of_p,
    gender = gender_of_p,
    vars = varsP12
  )
)
