#Maps


library(sf)
library(ggplot2)
library(data.table)

# Read spatial data 
loc_st<- st_read("data/analysis/loc_simple.gpkg")
#loc_sf <- st_read("data/analysis/sTWIST_Locations.shp")
names(loc_st)[names(loc_st) == "Location"] <- "location"

# Prepare data
fr_data <- fr_final_dataset
num_fr <- aggregate(firstRecordEvent ~ location, data = fr_data, FUN = length)


# Join both files
loc_joined <- merge(loc_st, num_fr, by = "location", all.x = TRUE)


# Plot map
ggplot(loc_joined) +
  geom_sf(aes(fill = firstRecordEvent), color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "cividis", na.value = "grey90") +
  labs(
    title = "Number of First Records per Location",
    fill = "First records"
  ) +
  theme_minimal()


# Plot map (log scale)
ggplot(loc_joined) +
  geom_sf(aes(fill = firstRecordEvent), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "plasma", #cividis is another color palette
    trans = "log10",          # log scale
    na.value = "grey90"
  ) +
  labs(
    title = "Number of First Records per Location (log scale)",
    fill = "First records\n(log10)"
  ) +
  theme_minimal()



# Pour faire un echantillon de la dataset
library(openxlsx)

# Lire le fichier
data <- read.xlsx("data/input/IntroData_raw_2026.xlsx")

# Sélectionner 200 lignes aléatoires
sample_2000 <- data[sample(nrow(data), 2000), ]

# Sauvegarder
write.xlsx(sample_2000, "data/input/sample_2000.xlsx")


# 1. Check unique locations in each dataset
print("Locations in shapefile:")
print(unique(loc_st$location))

print("Locations in fr_data:")
print(unique(fr_data$location))

# 2. Check which locations matched after merge
print("Locations with data after merge:")
print(loc_joined$location[!is.na(loc_joined$firstRecordEvent)])

# 3. Check for mismatches
locations_in_st <- unique(loc_st$location)
locations_in_data <- unique(num_fr$location)

print("In shapefile but not in data:")
print(setdiff(locations_in_st, locations_in_data))

print("In data but not in shapefile:")
print(setdiff(locations_in_data, locations_in_st))

