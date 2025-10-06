library(tidyverse)
library(sf)

#2023 SWFWMD LULC was clipped to drainage basins to each particular site according to upstream tracing in ArcGIS Utility Network Analyst
#FDEP Run 66 WBID boundaries upstream of the sample sites were used to clip the lulc layer
#Resulting drainage basin lulc shapefiles were dissolved by fluccscode, total hectares were calculated, and then imported into this repository

#Substitute appropriate shapefile for each site summary.

all_sites <- st_read("./data-raw/FIU_Sites.shp") %>% filter(SITE != 0)

site15_21_basin <- st_read("./data-raw/SITE_15thru21_LULC_DRAINAGE_DISS.shp") %>%
  mutate(category = case_when(LEV4 %in% c("1100", "1200", "1300", "1400", "1500", "1600", "1650",
                                          "1700", "1800", "1820", "1900", "7400", "8100", "8200",
                                          "8300") ~ "Urban / Open Lands",
                              LEV4 %in% c("1600") ~ "Mining",
                              LEV4 %in% c("1650") ~ "Reclaimed Mined Lands",
                              LEV4 %in% c("2100", "2140", "2200", "2300", "2400", "2500", "2550",
                                          "2600", "3000") ~ "Agriculture",
                              LEV4 %in% c("3100", "3200", "3300", "4100" ,"4110", "4120", "4200",
                                          "4300", "4340", "4400", "7100", "7200") ~ "Natural / Vegetated / Forested",
                              LEV4 %in% c("6100", "6110", "6120", "6150", "6200", "6210", "6300",
                                          "6400", "6410", "6420", "6430", "6440", "6460", "6500",
                                          "6510", "6520", "6530", "6600") ~ "Wetlands",
                              LEV4 %in% c("6540") ~ "Oysters",
                              LEV4 %in% c("9120", "6510", "7210", "9121", "9122") ~ "Tidal Flat",
                              LEV4 %in% c("9113", "9116") ~ "Seagrass",
                              LEV4 %in% c("5100", "5200", "5300", "5400", "5700", "5720") ~ "Water",
                              TRUE ~ NA))

lulc <- site15_21_basin %>%
             st_drop_geometry() %>%
             mutate(category = case_when(LEV4 %in% c("1100", "1200", "1300", "1400", "1500", "1600", "1650",
                                                     "1700", "1800", "1820", "1900", "7400", "8100", "8200",
                                                     "8300") ~ "Urban / Open Lands",
                                         LEV4 %in% c("1600") ~ "Mining",
                                         LEV4 %in% c("1650") ~ "Reclaimed Mined Lands",
                                         LEV4 %in% c("2100", "2140", "2200", "2300", "2400", "2500", "2550",
                                                     "2600", "3000") ~ "Agriculture",
                                         LEV4 %in% c("3100", "3200", "3300", "4100" ,"4110", "4120", "4200",
                                                     "4300", "4340", "4400", "7100", "7200") ~ "Natural / Vegetated / Forested",
                                         LEV4 %in% c("6100", "6110", "6120", "6150", "6200", "6210", "6300",
                                                     "6400", "6410", "6420", "6430", "6440", "6460", "6500",
                                                     "6510", "6520", "6530", "6600") ~ "Wetlands",
                                         LEV4 %in% c("6540") ~ "Oysters",
                                         LEV4 %in% c("9120", "6510", "7210", "9121", "9122") ~ "Tidal Flat",
                                         LEV4 %in% c("9113", "9116") ~ "Seagrass",
                                         LEV4 %in% c("5100", "5200", "5300", "5400", "5700", "5720") ~ "Water",
                                         TRUE ~ NA)) %>%
              mutate(total = sum(HECTARES),
                     pct = HECTARES / total) %>%
             group_by(category) %>%
             summarise(sum_area = sum(HECTARES),
                       percent = sum(pct)*100)

p <- ggplot() +
     geom_sf(data = site15_21_basin, aes(fill = category)) +
     geom_sf(data = all_sites) +
     theme_minimal()
p
