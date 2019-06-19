#stored code block for RODBC----------------------------
#library(RODBC) #not yet used but someday could provide direct SQL query for updating the data
# InfoNet <- odbcDriverConnect
#   ('driver={SQL Server};
#     server=Apollo.Infonet.icjia.org;
#     database=InfonetServer;
#     username=KgruschowSQL;
#     password=stPatricks2019!')
# sqlTables(InfoNet)

#-Locations---------------------------------------------
Public_Folder <- function(filename)
  {paste0("X:/External and Public Data/", filename)}

Hotline_Folder <- function(filename)
  {paste0("P:/Center for Victim Studies/INFONET/Protected Files/Research projects involving InfoNet data/SA and DV Calls/", filename)}

Deidentified_Folder <- function(filename)
{paste0("P:/Center for Victim Studies/INFONET/Protected Files/Research projects involving InfoNet data/Deidentified+Exploratory/", filename)}
#-Load Contigency Table Function------------------------
Convert_to_Contigency_Table <-function(tibbleF,rownamesF)
{row.names(tibbleF)<- tibbleF[[rownamesF]]
 tibbleF[[rownamesF]]<- NULL
 tibbleF <- as.table(as.matrix(tibbleF))}

#-US Holidays Data Frame--------------------------------
ImportUSHolidays <- function()
{bind_rows(
    #using bind rows to combine two dataframe types 
    #add_row did not work
  
    dplyr::filter(generated_holidays,country=='US') %>%
    select(ds,holiday) %>%
    mutate(ds = as.Date(ds))
  ,
    #add Black Friday   
    
    dplyr::filter(generated_holidays, country=='US' & holiday=='Thanksgiving') %>%
    select(ds,holiday) %>%
    mutate(ds = as.Date(ds)+1) %>%
    mutate(holiday = 'Black Friday')

    ) %>%
  rename(Date=ds) %>%
  arrange(Date)

}

#-Create Holiday Variables-Holidize-----------------
holidize <- function(tibbleF, datevar)
{tibbleF %>%
    left_join( #add Holidays from Facebook
      ImportUSHolidays(),
      by=c(datevar)
    ) %>%
    mutate(holiday=replace_na(holiday,'Normal Day')) %>%
    add_column(Weekday=wday(tibbleF[[datevar]],label = TRUE)) %>%
    add_column(Month=month(tibbleF[[datevar]],label = TRUE)) %>%
    add_column(TypeofDay=case_when(
      wday(tibbleF[[datevar]],label = TRUE)=='Sun' ~ "Weekend",
      wday(tibbleF[[datevar]],label = TRUE)=='Sat' ~ "Weekend",
      .$holiday=='Thanksgiving' ~ "Thanksgiving",
      .$holiday=='Independence Day' ~ "Independence Day",
      .$holiday=='Independence Day (Observed)' ~ "Independence Day",
      .$holiday=='Labor Day' ~ "Labor Day",
      .$holiday=='Black Friday' ~ "Black Friday",
      .$holiday=='Christmas Day' ~ "Christmas Day",
      .$holiday=="New Year's Day" ~ "New Year's Day",
      .$holiday=="Christmas Day (Observed)" ~ "Christmas Day",
      .$holiday=='Martin Luther King, Jr. Day' ~ "Martin Luther King, Jr. Day",
      .$holiday=='Memorial Day' ~ "Memorial Day",
      .$holiday=='Memorial Day (Observed)' ~ "Memorial Day",
      TRUE ~ "Normal")) %>%
    add_column(WorkDay=case_when(
      .$TypeofDay=="Normal" ~ 1,
      TRUE ~ 0
    )) %>%
    mutate(TypeofDay=ordered(.$TypeofDay, 
                              levels=c("Normal",
                                       "Martin Luther King, Jr. Day",
                                       "Labor Day",
                                       "New Year's Day",
                                       "Weekend",
                                       "Memorial Day",
                                       "Independence Day",
                                       "Black Friday",
                                       "Christmas Day",
                                       "Thanksgiving")))
}

#-Run Regression Summary and ANOVA----------------------

test_lm <- function(model)
{ list(    summary(model)
         , anova_stats(model)) }
  
