# import modules ----

# import data ----

# import libraries ----

# Descriptives----
mean(DailySACalls$Calls)
sd(DailySACalls$Calls)
summary(DailySACalls$Calls)

describeBy(DailySACalls$Calls, group=DailySACalls$Weekday)
describeBy(DailySACalls$Calls, group=DailySACalls$Month)
describeBy(DailySACalls$Calls, group=year(DailySACalls$Date))

# plots ----
# Histogram of Calls
DailySACalls %>%
  ggplot(aes(Calls)) +
  geom_histogram(breaks = seq(0,45, by=1), colour = "black", fill = "white") +
  labs(
    title = "Histogram of Calls",
    x = "Calls per Day",
    y = "Number of Days"
  )


# "Scatter Plot" of all data with Model Predictions
DailySACalls %>%
  ggplot(aes(x = DayID, y = Calls)) +
  geom_hex(bins = 45)+
  scale_fill_gradient(low = "light gray", high = "black")+
  geom_point(aes(y = GLMPredicted8))+
  ggtitle("Sexual Assault Hotline Calls")


# Box Plot Call by Month
DailySACalls %>%
  ggplot(aes(x = Month, y = Calls)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0,30)) +
  ggtitle("Distribution of Monthly Calls")

# Bar Graph of Calls by Month
DailySACalls %>%
  ggplot(aes(x = Month, y = Calls))+
  geom_col()+
  ggtitle("Total Calls by Month")

# Box Plot of Calls by Year
DailySACalls %>%
  ggplot(aes(x = year(Date), group = year(Date), y = Calls)) +
  geom_boxplot() +
  labs(title = "Distribution of Calls by Year", x = "Year")

# Box Plot of Calls by Day of Week
DailySACalls %>%
  ggplot(aes(x = Weekday, y = Calls)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0,30)) +
  ggtitle("Calls by Day of Week")

# Box Plot of Calls by Holiday


# Box Plot of Type of Day
DailySACalls %>%
  mutate(TypeofDay = fct_reorder(TypeofDay, Calls)) %>%
  ggplot(
    aes(x=TypeofDay,y=Calls)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = c(0,25))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Holidays")+
  ggtitle("Comparison of Holidays and Weekdays")

# Box Plot of Residuals by Month
DailySACalls %>%
  ggplot(aes(x = Month, y = GLMResid8)) +
  geom_boxplot()