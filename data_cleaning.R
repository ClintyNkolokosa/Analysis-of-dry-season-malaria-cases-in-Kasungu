# Libraries -----------------------------------------------------------------------------------------
library(DataExplorer)
library(tidyverse)
library(raster)
library(sp)
library(sf)
library(rgdal)

# Tell R where the data is ---------------------------------------------------------------------------
here::here() # make sure you are in the "~/R/upscaled_2021_updated_May/upscaled_2021" folder

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

ku_malire <- spTransform(malire, proj4string(accessibility_raster))


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
plot(health_facility_sf, add=TRUE, lwd=1)

# Write the resulting raster ------------------------------------------------------------------
writeRaster(ku_raster, "data/friction_surface_clip.tif")

# Plot clipped map ----------------------------------------------------------------------------
tm_shape(ku_raster)+
  tm_raster(palette = "Greys", style = "fisher", n = 5)+
  tm_layout(legend.position = c("right","bottom"),
            frame = FALSE)

# Load zipatala data -------------------------------------------------------------------
# Master Health Facility Registry data http://zipatala.health.gov.mw/facilities
mhfr_fac <- readxl::read_xlsx(here::here("data/MHFR_Facilities.xlsx")) %>%  
            dplyr::select(NAME, `COMMON NAME`, OWNERSHIP, TYPE, STATUS, DISTRICT, LATITUDE, LONGITUDE) %>% 
            dplyr::filter(DISTRICT == "Kasungu",
                          STATUS == "Functional",
                          NAME != "St. Andrews Health Centre") # appears in Nkhotakota when mapped

# Using the is.na() function to remove the missing lon and lat points.
#  Missing values in coordinates not allowed in sf transformation
mhfr_fac <- mhfr_fac[!is.na(mhfr_fac$LATITUDE),]

mhfr_fac_sf <- sf::st_as_sf(mhfr_fac, coords = c("LONGITUDE", "LATITUDE"),
                        crs = 4326, agr = "constant") 

mapview::mapview(mhfr_fac_sf, zcol = "OWNERSHIP", legend.opacity = .5)

# Malawi disaster management health facility data -----------------------------------------------------------
masdap_health_fac <- read.csv(here::here("data/malawi_health_masdap_2013.csv")) %>%  # https://gis-malawi.com/
                     dplyr::select(fid, fac_name, district, facility, x, y, owner) %>% 
                     dplyr::filter(district == "KASUNGU")

masdap_health_fac_sf <- sf::st_as_sf(masdap_health_fac, coords = c("x", "y"),
                                     crs = 4326, agr = "constant")

mapview::mapview(masdap_health_fac_sf, zcol = "owner", legend.opacity = .5)

primary_health_fac <- read.csv(here::here("data/health_facilities_moh_primary_health_facilities.csv")) %>%  # https://gis-malawi.com/
                      dplyr::select(fid, geom, facility_name, district, facility_type, status) %>% 
                      dplyr::filter(district == "Kasungu",
                                    status == "Functional")

library(reshape2) # Split geom column into lat and long columns
geom <- colsplit(string = gsub(pattern = "\\(|\\)", replacement = "",
                               x = primary_health_fac$geom),
                 pattern = " ", names = c("type","geom"))

geom[,c("Longitude", "Latitude")] <- str_split_fixed(geom$geom, " ", 2)

geom <- geom %>% dplyr::select(type, Longitude, Latitude)

# Add splitted latitude and longitude coordinates to primary health facilities
primary_health_fac_sf <- cbind.data.frame(primary_health_fac, geom)

# Convert to sf object
primary_health_fac_sf <- sf::st_as_sf(primary_health_fac_sf, coords = c("Longitude", "Latitude"),
                                      crs = 4326, agr = "constant") 
mapview::mapview(primary_health_fac_sf, zcol = "facility_type", legend.opacity = .5)


