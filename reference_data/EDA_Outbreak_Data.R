library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(forcats) 

outbreak_data <- read.csv("outbreak_data.csv")

Rohan_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0, vjust = 1)
  )

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

# Histogram for Total Suspected Cases
# This histogram shows the distribution of total suspected cholera cases across different outbreaks.
# The x-axis represents the number of suspected cases, and the y-axis shows how frequently these case numbers occur.
histogram_total_cases <- ggplot(outbreak_data, aes(x=total_suspected_cases)) + 
  geom_histogram(binwidth = 10, fill = "#4d89f9", color = "#4d89f9") + 
  scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with commas for readability
  labs(
    title = "Distribution of Total Suspected Cholera Cases",
    subtitle = "Across Various Outbreaks",
    x = "Total Suspected Cases",
    y = "Frequency"
  ) +
  Rohan_theme

# Histogram for Duration of Outbreaks
# This histogram illustrates the distribution of the duration (in weeks) of cholera outbreaks.
# It helps in understanding how long outbreaks typically last.
histogram_duration <- ggplot(outbreak_data, aes(x=duration)) + 
  geom_histogram(binwidth=1, fill = "#4d89f9" , color = "black") + 
  labs(title="Duration of Outbreaks", x = "Duration (weeks)", y = "Frequency") +
  Rohan_theme

# Bar Plot for Country Distribution
# This bar plot shows the number of cholera outbreaks in different Counties.
# It is useful for identifying which countries have the highest number of reported outbreaks.
barplot_country <- ggplot(outbreak_data, aes(x=full_name)) + 
  geom_bar(fill = "#4d89f9", color = "black") + 
  labs(title="Distribution of Outbreaks by Country", x = "Country", y = "Number of Outbreaks") +
  Rohan_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 7))

# Bar Plot for Population Category Distribution
# This plot categorizes outbreaks by the population size of the affected area.
# It helps in understanding if certain population sizes are more prone to outbreaks.
# Reorder the pop_cat factor levels based on their frequency
outbreak_data$pop_cat <- fct_reorder(outbreak_data$pop_cat, outbreak_data$pop_cat, function(x) -length(x))

barplot_pop_cat <- ggplot(outbreak_data, aes(x = pop_cat)) + 
  geom_bar(fill = "#4d89f9", color = "black") + 
  labs(
    title = "Cholera Outnreak Distribution by Population Size",
    x = "Population Category",
    y = "Number of Outbreaks"
  ) +
  ylim(0,800) + 
  Rohan_theme

# Boxplot for Total Suspected Cases Distribution
# This boxplot visualizes the spread of the total suspected cases in different outbreaks.
# It highlights the median, quartiles, and any potential outliers in the suspected cases data.
boxplot_total_cases <- ggplot(outbreak_data, aes(y = total_suspected_cases)) +
  geom_boxplot(
    fill = "#4d89f9", color = "black", 
    outlier.color = "red", outlier.shape = 1
  ) +
  scale_y_continuous(trans = 'log10', labels = scales::comma) + # Use log10 transformation for the y-axis
  labs(
    title = "Distribution of Total Suspected Cholera Cases",
    x = "", 
    y = "Total Suspected Cases",
  ) +
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_blank(), 
    axis.title.y = element_text(size = 14),
    axis.text.x = element_blank(), 
    axis.text.y = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0, vjust = 1),
    axis.ticks.x = element_blank() 
  )


# Boxplot for Duration of Outbreaks Distribution
# This boxplot shows the distribution of the duration of outbreaks.
# It provides insights into the central tendency and variability in the duration of cholera outbreaks.
boxplot_duration <- ggplot(outbreak_data, aes(y = duration)) +
  geom_boxplot(
    fill = "#4d89f9", color = "black",
    outlier.color = "red", outlier.shape = 1
  ) +
  labs(
    title = "Duration of Cholera Outbreaks Distribution",
    x = "",
    y = "Duration (weeks)",
  ) +
  theme_minimal(base_size = 14) + # Use a minimal theme with a larger base font size for readability
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.title.x = element_blank(), # Hide the x-axis title
    axis.title.y = element_text(size = 14),
    axis.text.x = element_blank(), # Hide the x-axis text
    axis.text.y = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0, vjust = 1),
    axis.ticks.x = element_blank() # Hide the x-axis ticks
  )

histogram_total_cases
histogram_duration
barplot_country
barplot_pop_cat
boxplot_total_cases
boxplot_duration


# Summary Statistics
summary_stats <- outbreak_data %>% select(total_suspected_cases, duration, total_deaths) %>% summary()

# Correlation Analysis
correlation_analysis <- cor(outbreak_data$total_suspected_cases, outbreak_data$duration, use="complete.obs")

# Output the results
print(summary_stats)
print(correlation_analysis)









