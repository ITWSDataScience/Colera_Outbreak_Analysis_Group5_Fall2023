# Load necessary libraries
library(dplyr)
library(tidyverse)
library(tidyr)
library(factoextra)
library(cluster)
library(sf)
library(rnaturalearth)

# Load the merged cholera outbreak and precipitation data
merged_data <- read.csv("merged_cholera_precipitation_data.csv")

# Country code to country name mapping
country_code_name_mapping = c(
  AGO = 'Angola',
  BEN =  'Benin',
  CIV = "Cote d'Ivoire",
  CMR = 'Cameroon',
  COD = 'Congo, Dem. Rep.',
  COG = 'Congo, Rep.',
  ETH = 'Ethiopia',
  GHA = 'Ghana',
  GIN = 'Guinea',
  GNB = 'Guinea-Bissau',
  KEN = 'Kenya',
  MLI = 'Mali',
  MOZ = 'Mozambique',
  MWI = 'Malawi',
  NAM = 'Namibia',
  NER = 'Niger',
  NGA = 'Nigeria',
  SLE = 'Sierra Leone',
  SSD = 'South Sudan',
  TCD = 'Chad',
  TZA = 'Tanzania',
  TZA_zanzibar = 'Zanzibar',
  UGA = 'Uganda',
  ZMB = 'Zambia',
  SOM = 'Somalia',
  ZWE = 'Zimbabwe'
)

# Map country codes in the 'country' column to full country names
merged_data$country_name <- country_code_name_mapping[merged_data$country]

# Selecting relevant columns (total suspected cases and all precipitation columns)
precipitation_columns <- grep("precipitation", names(merged_data), value = TRUE)
selected_columns <- c('country_name', 'total_suspected_cases', precipitation_columns)
clustering_data <- merged_data[selected_columns]

# Handling missing values
clustering_data <- na.omit(clustering_data)

# Normalizing the data
clustering_data_normalized <- as.data.frame(scale(clustering_data[,-1]))

# K-Means clustering - Using the Elbow method to find the optimal number of clusters
fviz_nbclust(clustering_data_normalized, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow Method")

# Assuming the optimal number of clusters is 4 (adjust based on the Elbow plot)
set.seed(42)
kmeans_result <- kmeans(clustering_data_normalized, centers = 4, nstart = 25)
clustering_data$Cluster <- as.factor(kmeans_result$cluster)

# Geographical plotting
# Get country-level data
world_countries <- ne_countries(scale = "medium", returnclass = "sf")

# Filter for African countries
africa_countries <- world_countries[world_countries$continent == "Africa", ]

# Merging the cholera and precipitation data with the geographical data
geo_data <- left_join(africa_countries, clustering_data, by = c('name' = 'country_name'))

# Plotting the geographic distribution of Mean Cholera Cases and Mean Precipitation
# Enhanced geographical plotting of African countries
# Enhanced Geographical Plotting without the panel border
# Enhanced Geographical Plotting with non-data African countries in gray
ggplot() +
  geom_sf(data = geo_data, aes(fill = as.factor(Cluster)), color = "white", size = 0.2) + # Override color for data countries
  scale_fill_viridis_d(option = "D", name = "Cluster", na.value = "gray") + # Set NA values to white
  labs(title = 'Cholera Cases and Precipitation in Africa') +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank()) 



