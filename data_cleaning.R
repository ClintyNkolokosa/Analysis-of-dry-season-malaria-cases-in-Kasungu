# Libraries -----------------------------------------------------------------------------------------
library(DataExplorer)
library(tidyverse)
library(raster)
library(sp)
library(sf)
library(rgdal)

# Tell R where the data is ---------------------------------------------------------------------------
here::here()

# Load 2020 NMCP confirmed cases data, remove unnecessary columns and rename -------------------------

# Filtering out dry season malaria cases and tidying the data
# Typically Malawi can be divided into the following seasons:
#  - a hot and rainy season from November to April 
#  - and a relatively cool and dry season from May to October
# Source: https://www.metmalawi.com/climate/climate.php

ku_dry_season_malaria_cases_2020 <- read.csv(here::here("data", "ku_2020_malaria_cases.csv")) %>% 
                                     dplyr::select(Names = `organisationunitname`,
                                                   May.2020, June.2020, July.2020,
                                                   August.2020, September.2020, October.2020) %>% 
                                     dplyr::filter(Names != "Fpam Clinic Kasungu", 
                                                   Names != "Kamuzu Academy Clinic") %>% 
                                     dplyr::rowwise() %>% 
                                     dplyr::mutate(dr_2020 = sum(May.2020, June.2020, July.2020,
                                                                 August.2020, September.2020, 
                                                                 October.2020, na.rm = TRUE))

# Export -------------------------------------------------------------------------------------------
write.csv(ku_dry_season_malaria_cases_2020, file = "data/dry_season_malaria_2020.csv")

# Eplore data --------------------------------------------------------------------------------------
ku_dry_season_malaria_cases_2020 %>% dplyr::glimpse() %>%  
  DataExplorer::introduce() %>% 
  DataExplorer::plot_intro()

ku_dry_season_malaria_cases_2020 %>% DataExplorer::plot_missing()

ku_dry_season_malaria_cases_2020 %>% DataExplorer::plot_bar() 

ku_dry_season_malaria_cases_2020 %>% 
  DataExplorer::plot_correlation(maxcat = 5)


# Load raster and shapefile data -------------------------------------------------------------
accessibility_raster <- raster::raster(here::here("data", "2015_friction_surface_v1.geotiff"))

accessibility_raster

ku_district <- shapefile(here::here("data", "kasungu_district"))

proj4string(accessibility_raster)

proj4string(ku_district)

ku_district <- spTransform(ku_district, proj4string(accessibility_raster)) 

# Health facility catchment boundary shapefile -----------------------------------------------
malire <- shapefile(here::here("data", "kasungu_health_facility_catchments")) 

malire <- spTransform(malire, proj4string(accessibility_raster))


# Plot map ----------------------------------------------------------------------------------
tm_shape(accessibility_raster)+
  tm_raster(palette = "Reds", style = "fisher", n = 5)+
  tm_layout(legend.position = c("left","bottom"),
            frame = FALSE)

# Crop and mask ------------------------------------------------------------------------------
clip_raster <- mask(crop(accessibility_raster, extent(ku_district)), ku_district) 

# raster_clip <- crop(accessibility_raster, extent(ku_district))
# ku_raster <- mask(raster_clip, ku_district)

# Check that clipping worked
plot(ku_raster)
plot(malire, add=TRUE, lwd=1)

# Write the resulting raster ------------------------------------------------------------------
writeRaster(ku_raster, "data/friction_surface_clip.tif")

# Plot clipped map ----------------------------------------------------------------------------
tm_shape(ku_raster)+
  tm_raster(palette = "Reds", style = "fisher", n = 5)+
  tm_layout(legend.position = c("right","bottom"),
            frame = FALSE)


