#Maps


library(sf)
library(ggplot2)

# Read spatial data 
loc_sf <- st_read("data/analysis/sTWIST_Locations.shp")
names(loc_sf)[names(loc_sf) == "Location"] <- "location"

# Prepare data
fr_data <- fr_final_dataset
num_fr <- aggregate(firstRecordEvent ~ location, data = fr_data, FUN = length)


# Join both files
loc_joined <- merge(loc_sf, num_fr, by = "location", all.x = TRUE)


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

