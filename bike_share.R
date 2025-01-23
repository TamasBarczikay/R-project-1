################################################################################
# Load dataset
################################################################################
library(ggplot2)
library(dplyr)
library(lubridate)
library(forcats)
library(data.table)
library(circular)

train_raw <- read.csv("/Users/tamasbarczikay/R_projects/R_project_1/train.csv")
station_data_raw <- read.csv('/Users/tamasbarczikay/R_projects/R_project_1/station_data.csv')
weather_data_raw <- read.csv("/Users/tamasbarczikay/R_projects/R_project_1/weather_data.csv")

################################################################################
# Cleaning dataset
################################################################################
### Ride dataset - times are in char - should be in datetime
str(train_raw)

# Missing observations (NA or empty string) - no missing values
print(colSums(is.na(train_raw) | train_raw == ""))

# Datetime object correction
train_raw$start_time <- as.POSIXct(train_raw$start_time, format="%Y-%m-%d %H:%M:%S")
train_raw$end_time <- as.POSIXct(train_raw$end_time, format="%Y-%m-%d %H:%M:%S")

# Duplicates - there were no duplicates
print(train_raw[duplicated(train_raw) | duplicated(train_raw, fromLast = TRUE), ])

# Create duration column in minutes
train_raw$duration <- as.numeric(difftime(train_raw$end_time, train_raw$start_time, units = "mins"))

# Checking outliers - 552 unique values
unique(sort(train_raw$duration))

# Many are over 600 mins - 10 hrs - that could be a reasonable cut-off value
# since a shift at a company, with a ride to the premises and back could take that much time

hist(as.numeric(names(table(train_raw$duration))), 
     breaks = 30, 
     col = "grey", 
     main = "Histogram of Travel Durations", 
     xlab = "Duration (minutes)", 
     ylab = "Frequency")


# Checking how many of the rides are outliers - 0.1% they can be removed (106 obs)
total_rides <- nrow(train_raw)
rides_above_600 <- sum(train_raw$duration > 600)
percentage_share_above_600 <- (rides_above_600 / total_rides) * 100
print(round(percentage_share_above_600*100, 2))

# Filter rows
train_raw <- train_raw %>%
  filter(duration < 600)

# Drop non-used dfs from the global environment
rm(percentage_share_above_600, total_rides, rides_above_600)

################################################################################
### Station dataset - same problem with datetime objects
str(station_data_raw)

# Missing observations (NA or empty string) - no missing values
print(colSums(is.na(station_data_raw) | station_data_raw == ""))

# Datetime object correction
station_data_raw$datetime_start <- as.POSIXct(station_data_raw$datetime_start, format="%Y-%m-%d %H:%M:%S")
station_data_raw$datetime_end <- as.POSIXct(station_data_raw$datetime_end, format="%Y-%m-%d %H:%M:%S")

# Duplicated stations
# In some cases rack number was increased, or place was relocated.


## Relocations should be considered as one, since they were only put meters away
# 611 - Nyugati tér - relocated twice, same number of racks - 1 min walk

station_data_raw <- station_data_raw %>%
  group_by(place_id) %>%
  filter(ifelse(place_id == 611, row_number() == 1, TRUE)) %>%
  ungroup()

# 1302 - Szent István park - relocated 26 meters, I consider them as one
station_data_raw <- station_data_raw %>%
  group_by(place_id) %>%
  filter(ifelse(place_id == 1302, row_number() == 1, TRUE)) %>%
  ungroup()


## Not enough time to have an effect
# 103 - Clark Ádám tér - num_of_rack +1, 9 days before the end of the period - 2015-05-22 15:35:03
station_data_raw <- station_data_raw %>%
  group_by(place_id) %>%
  filter(ifelse(place_id == 103, row_number() == 1, TRUE)) %>%
  ungroup()

# 517 - Városháza Park - num_of_rack +7, 4 days before the end - 2015-05-27 15:25:02
station_data_raw <- station_data_raw %>%
  group_by(place_id) %>%
  filter(ifelse(place_id == 517, row_number() == 1, TRUE)) %>%
  ungroup()

# 707 - Kéthly Anna tér -  num_of_rack +2/+1, within a week since 2015-05-21 16:35:02
station_data_raw <- station_data_raw %>%
  group_by(place_id) %>%
  filter(ifelse(place_id == 707, row_number() == 1, TRUE)) %>%
  ungroup()



## Possible biasing effects
# 508 - Erzsébet tér - num_of_rack +2, 3 weeks before the end - 2015-05-08 12:50:02

# 518 - Deák tér - num_of_rack +3, 3 weeks before the end - 2015-05-11 12:40:02

# 806 - József körút - Baross utca - num_of_rack +6 at 2015-05-11 14:45:02

# 808 - Keleti pályaudvar - num_of_rack +6/+6 - 2015-03-19 21:30:01 and 2015-05-23 14:50:03

filtered_train_raw <- train_raw %>%
  filter(start_location %in% c(508, 518, 806, 808))

filtered_station_data_raw <- station_data_raw %>%
  filter(place_id %in% c(508, 518, 806, 808))

joined_data <- left_join(filtered_train_raw, 
                         filtered_station_data_raw, 
                         by = c("start_location" = "place_id"),
                         relationship = "many-to-many") %>%
  filter(start_time >= datetime_start & start_time <= datetime_end) %>%
  mutate(
    operation_duration_days = as.numeric(difftime(datetime_end, datetime_start, units = "days"))
  )

grouped_counts <- joined_data %>%
  group_by(place_name, datetime_start, operation_duration_days) %>%
  count() %>%
  mutate(avd_usage = n/operation_duration_days)

# Print or view the result
print(grouped_counts)

rm(filtered_station_data_raw, filtered_train_raw, grouped_counts, joined_data)

# Conclusion: THESE MUST BE KEPT SEPARATELY

################################################################################
### Weather dataset - same problem with datetime objects
str(weather_data_raw)

# Missing observations (NA or empty string) - no missing values
print(colSums(is.na(weather_data_raw) | weather_data_raw == ""))

# Datetime object correction
weather_data_raw$time <- as.POSIXct(weather_data_raw$time, format="%Y-%m-%d %H:%M:%S")

# Check unique values for wdire (as factor)
unique(weather_data_raw$wdire)
weather_data_raw$wdire <- as.factor(weather_data_raw$wdire)

# Sanity check of variables
# -9999 - 2 are missing -> drop it
unique(weather_data_raw$wspdm)
sum(weather_data_raw$wspdm == -9999)

weather_data_raw <- weather_data_raw %>%
  filter(wspdm != -9999)

# This one is a factor
unique(weather_data_raw$wdird)
#weather_data_raw$wdird <- as.factor(weather_data_raw$wdird)

# -9999.0 - half of the data is missing -> drop column
unique(weather_data_raw$vism)
sum(weather_data_raw$vism == -9999)

# -999.0 - drop this one as well
unique(weather_data_raw$windchillm)
sum(weather_data_raw$windchillm == -999)

# No variability - only 0 velues - drop
unique(weather_data_raw$hail)

weather_data_raw <- weather_data_raw[, !names(weather_data_raw) %in% c("vism", "windchillm", "hail")]

################################################################################
# Explore how the weather affects lending.
################################################################################

## Regression analysis
# Create bins for analysis
weather_data_raw$time_upper <- as.POSIXct(weather_data_raw$time) + lubridate::minutes(29)

# Convert to data.table format for performance increase in left join
setDT(train_raw)
setDT(weather_data_raw)

joined_data <- train_raw[weather_data_raw, on = .(start_time >= time, start_time <= time_upper), nomatch = 0]

# Revert back to dplyr compatible solution
joined_data <- as.data.frame(joined_data)

joined_data <- joined_data %>%
  rename(time_slot = start_time.1)

result <- joined_data %>%
  group_by(time_slot) %>%
  summarise(number_of_observations = n())

result <- result %>%
  left_join(weather_data_raw, by = c("time_slot" = "time_upper"))

# Add hour variable and drop non needed column
result <- result %>%
  mutate(hour = hour(as.POSIXct(time, format = "%Y-%m-%d %H:%M:%S")))

# Remove the unwanted columns
result <- result[, !names(result) %in% c("time_slot", "time", "wdire", "wdird")]

# Run regression
model <- lm(number_of_observations ~ ., data = result)

# Print the summary of the regression model
summary(model)

#########

# Get coeffs
coefficients <- summary(model)$coefficients[, 1]

# Create a data frame for plotting -  exclude insignificant
exclude_vars <- c("(Intercept)", "pressurem", "thunder")
plot_data <- data.frame(
  Coefficient = names(coefficients)[!(names(coefficients) %in% exclude_vars)],
  Estimate = coefficients[!(names(coefficients) %in% exclude_vars)],
  stringsAsFactors = FALSE
)## Overall effects
train_raw$month <- format(train_raw$start_time, "%Y-%m")

# Aggregate the number of rides by month
rides_by_month <- train_raw %>%
  group_by(month) %>%
  summarise(total_rides = n())

# Create a line chart
ggplot(rides_by_month, aes(x = month, y = total_rides / 1000, group = 1)) +
  geom_line() +
  geom_point() +
  labs(x = "Month",
       y = "Total Rides (in thousands)") +
  theme_minimal() +
  ylim(0, NA)


# Order data by decreasing values
plot_data <- plot_data[order(-abs(plot_data$Estimate)), ]

# Create a horizontal bar chart
bar_chart <- ggplot(plot_data, aes(x = Estimate, y = reorder(Coefficient, Estimate))) +
  geom_bar(stat = "identity", fill = ifelse(plot_data$Coefficient %in% c("snow", "fog", "rain"), "pink", "lightblue")) +
  labs(
    x = "Estimate",
    y = "Coefficient"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10)) 

# Display the bar chart
print(bar_chart)

rm(bar_chart, plot_data, coefficients, exclude_vars, rides_by_month)

################################################################################
# von Mises diagram for hourly traffic 
################################################################################
# Convert start_time to hrs
train_raw$timestamps_hrs <- hour(train_raw$start_time) + minute(train_raw$start_time) / 60 + second(train_raw$start_time) / 3600

# Convert to class circular
train_raw$timestamps_hrs_circ <- circular(train_raw$timestamps_hrs, units = "hours", template = "clock24")

# Estimate the periodic mean and the concentration from the von Mises distribution
estimates <- mle.vonmises(train_raw$timestamps_hrs_circ)
periodic_mean <- estimates$mu %% 24
concentration <- estimates$kappa

# Circular histogram
clock_plot <- ggplot(train_raw, aes(x = timestamps_hrs_circ)) +
  geom_histogram(bins = 24, colour = "blue", fill = "lightblue") +
  coord_polar() + 
  scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), minor_breaks = NULL) +
  theme_light() +
  labs(y = "Number of Rides")  # Set y-axis label

# print the plot
print(clock_plot)

# Add the periodic_mean
clock_plot + 
  geom_vline(xintercept = as.numeric(periodic_mean), color = "red", linetype = 3, size = 1.25)

rm(clock_plot, estimates)
################################################################################
# Rute analysis
################################################################################
# Get unique names with ids from station data
unique_combinations <- unique(station_data_raw[, c("place_id", "place_name")])

# Join them to train data
# Merge with start_location
train_raw <- merge(train_raw, unique_combinations, by.x = "start_location", by.y = "place_id", all.x = TRUE)
names(train_raw)[names(train_raw) == "place_name"] <- "start_location_name"

# Merge with end_location
train_raw <- merge(train_raw, unique_combinations, by.x = "end_location", by.y = "place_id", all.x = TRUE)
names(train_raw)[names(train_raw) == "place_name"] <- "end_location_name"


# Standardized rutes (create unique 'from - to' routes and match them with the reverse direction)
# There were 2792 different rutes
train_raw$route <- ifelse(train_raw$start_location_name < train_raw$end_location_name,
                          paste("From", train_raw$start_location_name, "to", train_raw$end_location_name, "or backwards"),
                          paste("From", train_raw$end_location_name, "to", train_raw$start_location_name, "or backwards"))

unique(train_raw$route)

# Measure their frequency
route_counts <- train_raw %>%
  group_by(route) %>%
  summarise(total_rides = n()) %>%
  mutate(percentage_share = round((total_rides / nrow(train_raw)) * 100, 2)) %>%
  arrange(desc(total_rides))

# We have all routes
sum(route_counts$total_rides)


hist(as.numeric(names(table(route_counts$total_rides))), 
     breaks = 30, 
     col = "grey", 
     main = "Histogram of Frequencies", 
     xlab = "Total rides", 
     ylab = "Frequency")

# Show the top 10 routes
top_10_routes <- head(route_counts, 10)


################################################################################
# Popular start and end destinations
################################################################################
top_start_destinations <- train_raw %>%
  group_by(start_location_name) %>%
  summarise(total_rides = n()) %>%
  arrange(desc(total_rides)) %>%
  head(10)

# Print the result
print(top_start_destinations)

top_end_destinations <- train_raw %>%
  group_by(end_location_name) %>%
  summarise(total_rides = n()) %>%
  arrange(desc(total_rides)) %>%
  head(10)

# Print the result
print(top_end_destinations)

rm(route_counts, 
   top_10_routes, 
   top_end_destinations, 
   top_start_destinations, 
   unique_combinations)

################################################################################
# Effect of rack number
################################################################################
rack_rides_data <- train_raw %>%
  group_by(start_location_name) %>%
  summarise(total_rides = n())

rack_num_data <- station_data_raw %>%
  group_by(place_name) %>%
  summarise(racks = mean(num_of_rack))

rack_final <- left_join(rack_rides_data, rack_num_data, 
                        by = c("start_location_name" = "place_name")) %>%
  arrange(desc(racks))

rm(rack_rides_data, rack_num_data)


ggplot(rack_final, aes(x = racks, y = total_rides)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Racks",
       y = "Total Rides") +
  theme_minimal()

summary(lm(total_rides ~ racks, data = rack_final))

