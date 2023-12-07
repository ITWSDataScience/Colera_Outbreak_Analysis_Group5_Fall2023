# Load the necessary libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(zoo)
library(scales)
library(MASS)
library(randomForest) 
library(rnaturalearth)  
library(rnaturalearthdata) 
library(sf)  

# Define the custom theme
Rohan_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0, vjust = 1)
  )

# Read and clean the datasets
outbreak_data <- read.csv("outbreak_data.csv")
precipitation_data <- read.csv("precipitation_data.csv")

# Mapping of African country abbreviations to full names
african_country_mapping <- data.frame(
  code = c("DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF", "TCD", 
           "COM", "COD", "COG", "CIV", "DJI", "EGY", "GNQ", "ERI", "SWZ", "ETH", 
           "GAB", "GMB", "GHA", "GIN", "GNB", "KEN", "LSO", "LBR", "LBY", "MDG", 
           "MWI", "MLI", "MRT", "MUS", "MAR", "MOZ", "NAM", "NER", "NGA", "REU", 
           "RWA", "SHN", "STP", "SEN", "SYC", "SLE", "SOM", "ZAF", "SSD", "SDN", 
           "TZA", "TGO", "TUN", "UGA", "ZMB", "ZWE"),
  full_name = c("Algeria", "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi", "Cape Verde", "Cameroon", "Central African Republic", "Chad", 
                "Comoros", "Democratic Republic of the Congo", "Republic of the Congo", "Ivory Coast", "Djibouti", "Egypt", "Equatorial Guinea", "Eritrea", "Eswatini", "Ethiopia", 
                "Gabon", "The Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Libya", "Madagascar", 
                "Malawi", "Mali", "Mauritania", "Mauritius", "Morocco", "Mozambique", "Namibia", "Niger", "Nigeria", "Réunion", 
                "Rwanda", "Saint Helena", "São Tomé and Príncipe", "Senegal", "Seychelles", "Sierra Leone", "Somalia", "South Africa", "South Sudan", "Sudan", 
                "Tanzania", "Togo", "Tunisia", "Uganda", "Zambia", "Zimbabwe")
)

outbreak_data <- merge(outbreak_data, african_country_mapping, by.x = "country", by.y = "code", all.x = TRUE)

# Convert date columns to Date type
outbreak_data$start_date <- as.Date(outbreak_data$start_date, format="%m/%d/%Y")
outbreak_data$end_date <- as.Date(outbreak_data$end_date, format="%m/%d/%Y")

# Extract month and season from dates
outbreak_data$month <- format(outbreak_data$start_date, "%m")
outbreak_data$season <- ifelse(outbreak_data$month %in% c("12", "01", "02"), "Winter",
                               ifelse(outbreak_data$month %in% c("03", "04", "05"), "Spring",
                                      ifelse(outbreak_data$month %in% c("06", "07", "08"), "Summer",
                                             "Fall")))

# Calculate the total precipitation for each location
precipitation_data <- precipitation_data %>%
  mutate(total_precipitation = rowSums(dplyr::select(., starts_with('precipitation')), na.rm = TRUE))

# Load country borders
world <- ne_countries(scale = "medium", returnclass = "sf")
africa <- world[world$continent == "Africa", ]

# Define the longitude and latitude range for Africa
xlim_africa <- c(-25, 60)  # Approximate longitude range for Africa
ylim_africa <- c(-35, 40)  # Approximate latitude range for Africa

# Spatial plot for Cholera Outbreaks
ggplot(data = outbreak_data) +
  geom_sf(data = world, fill = NA, color = "gray50") +  # Plot country borders
  geom_sf(data = africa, fill = "gray95", color = "gray50") +  # Highlight the African continent
  geom_point(aes(x = longitude, y = latitude, size = total_suspected_cases, fill = total_suspected_cases), pch = 21, alpha = 0.7) +
  scale_size(name = "Total Suspected Cases", range = c(1, 10)) + 
  scale_fill_viridis_c(name = "Total Suspected Cases", option = "C", begin = 0.3, end = 0.9, direction = 1) +
  guides(fill = guide_legend(override.aes = list(size = 6))) +  # Combine legends
  coord_sf(xlim = xlim_africa, ylim = ylim_africa, expand = FALSE) +  # Zoom into African region
  labs(
    title = "Spatial Distribution of Cholera Cases in Africa",
    subtitle = "Size and color of points represent the number of suspected cases",
    x = "Longitude",
    y = "Latitude"
  ) +
  Rohan_theme +
  theme(
    legend.position = "right",
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

# Create the precipitation spatial plot
ggplot(data = precipitation_data) +
  geom_sf(data = world, fill = NA, color = "gray50") +  # Plot world borders
  geom_sf(data = africa, fill = "gray95", color = "gray50") +  # Highlight the African continent
  geom_point(aes(x = longitude, y = latitude, size = total_precipitation, fill = total_precipitation), pch = 21, alpha = 0.7) +
  scale_size(name = "Total Precipitation", range = c(1, 10)) +
  scale_fill_viridis_c(name = "Total Precipitation", option = "C", begin = 0.3, end = 0.9, direction = 1) +
  guides(fill = guide_legend(override.aes = list(size = 6))) +  # Use override.aes to match size in legend
  coord_sf(xlim = xlim_africa, ylim = ylim_africa, expand = FALSE) +  # Zoom into the African region
  labs(
    title = "Spatial Distribution of Precipitation in Africa",
    subtitle = "Size and fill of points represent total precipitation",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    text = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )



# Merge the datasets
merged_data <- merge(outbreak_data, precipitation_data, by = "location_period_id")

# Convert date columns to Date type if not already
merged_data$start_date <- as.Date(merged_data$start_date, format="%m/%d/%Y")
merged_data$end_date <- as.Date(merged_data$start_date, format="%m/%d/%Y")

# Aggregate data by month/year and calculate total precipitation and cholera cases
monthly_data <- merged_data %>%
  mutate(month = floor_date(start_date, "month")) %>%
  group_by(month) %>%
  summarize(total_cases = sum(total_suspected_cases, na.rm = TRUE),
            total_precipitation = sum(`precipitation.2010_1`, na.rm = TRUE)) 

# Used to scale the secondary axis below
factor <- max(monthly_data$total_cases) / max(monthly_data$total_precipitation)

# Create the time-series plot with a secondary axis
ggplot(monthly_data, aes(x = month)) +
  geom_line(aes(y = total_cases, colour = "Cholera Cases")) +
  geom_line(aes(y = total_precipitation * factor, colour = "Precipitation")) +
  scale_y_continuous(
    name = "Total Cholera Cases",
    limits = c(0, 80000), # Set limit for the primary axis (left side)
    sec.axis = sec_axis(~./factor, name="Total Precipitation (mm/day)", 
                        labels = scales::comma) # Adjust the secondary axis (right side)
  ) +
  labs(title = "Cholera Cases and Precipitation Over Time",
       x = "Time") +
  scale_colour_manual(
    "", 
    breaks = c("Cholera Cases", "Precipitation"),
    values = c("blue", "green")
  ) +
  theme_minimal() + 
  theme(legend.position="bottom") 

# Shapiro-Wilk normality test for total_cases
shapiro.test(monthly_data$total_cases)

# Shapiro-Wilk normality test for total_precipitation
shapiro.test(monthly_data$total_precipitation)

# Spearman correlation test - Data not normal
cor.test(monthly_data$total_cases, monthly_data$total_precipitation, method = "spearman")

# Robust Linear Model - Data not normal
lm_model <- rlm(total_cases ~ total_precipitation, data = monthly_data)

# Use the model to predict cholera cases for a range of precipitation values
future_precipitation_data <- data.frame(total_precipitation = seq(min(monthly_data$total_precipitation, na.rm = TRUE), 
                                                                  max(monthly_data$total_precipitation, na.rm = TRUE), length.out = 100))
predictions <- predict(lm_model, newdata = future_precipitation_data)

# Add predictions to the future precipitation data frame
future_precipitation_data$predicted_cases <- predictions

# Plot the predictions along with the original data
ggplot(monthly_data, aes(x = total_precipitation, y = total_cases)) +
  geom_point() +
  geom_line(data = future_precipitation_data, aes(x = total_precipitation, y = predicted_cases), color = 'red') +
  ylim(0, 80000) +
  xlim(0, 150) +
  labs(title = "Predicted Cholera Cases Based on Precipitation",
       x = "Total Precipitation (mm/day)",
       y = "Predicted Total Cholera Cases") +
  Rohan_theme

# Output the summary of the linear model
summary(lm_model)








