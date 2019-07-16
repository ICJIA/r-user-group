# load modules ----
utils <- source("utils.R")$value


# load libraries ----
pkgs <- c("dplyr", "")
utils$install_and_load_pkgs(pkgs)


# load data ----
daily_calls <- ""


# define functions ----
prep_data <- function()

fit_lm <- function(formula, ...) lm(formula, data = daily_calls, ...)

add_model_info <- function(df, mod) {
  df %>%
    mutate(
      cooksd =  cooks.distance(mod),
      glm_resid = residuals(mod),
      glm_pred = predic(mod),
      stud_resid = studres(mod),
      rainn_pred = 100 * (Calls - glm_pred) / glm_pred
    )
}


# fit models ---
mod_1 <- fit_lm(Calls ~ as.numeric(year(Date)))
mod_2 <- fit_lm(Calls ~ DayID)
mod_3 <- fit_lm(Calls ~ DayID + Month)
mod_4 <- fit_lm(Calls ~ DayID + Fahrenheit)
mod_4b <- fit_lm(Calls ~ DayID + get('Change %'))
mod_5 <- fit_lm(Calls ~ DayID + Weekday)
mod_6 <- fit_lm(Calls ~ DayID + Weekday + holiday)
mod_7 <- fit_lm(Calls ~ DayID + TypeofDay)
mod_8 <- fit_lm(Calls ~ DayID + WorkDay)
mod_9 <- fit_lm(Calls ~ DayID + DayID:Workday)


# inspect models ---
##Tukey's HSD for Model 3 Monthes
#Model3HSD<-aov(Calls~DayID+Month,data=DailySACalls)
#TukeyHSD(Model3HSD,"Month",ordered=FALSE)
#describeBy(DailySACalls$Calls, group= DailySACalls$Month)


# add model info ---
info_mod_8 <- add_model_info(DailySACalls, mod_8)

# CallsG6 remains because it was used extensively in coding
# actual dates, it is very close to Model 8
NumericYears_X_WorkDay    <- lm(Calls ~ year(Date)+ WorkDay, data=DailySACalls)
NumericYears_X_WorkDay %>% test_lm()
DailySACalls$Cooks        <- cooks.distance(NumericYears_X_WorkDay)
DailySACalls$GLMResid     <- residuals(NumericYears_X_WorkDay)
DailySACalls$GLMPredicted <- predict(NumericYears_X_WorkDay)



# export it 
# write.csv(DailySACalls,"DailySACallsOut.csv")