# Libraries -----------------------------------------------------------------------------------------
library(DataExplorer)
library(tidyverse)
library(raster)
library(sp)
library(sf)
library(rgdal)

# Tell R where the data is -----------------------------------------------------
here::here() # make sure you are in the folder where the data is

# Load 2020 NMCP confirmed cases data, remove unnecessary columns and rename 

# Filtering out dry season malaria cases and tidying the data
# Typically Malawi can be divided into the following seasons:
#  - a hot and rainy season from November to April 
#  - and a relatively cool and dry season from May to October
# Source: https://www.metmalawi.com/climate/climate.php.
# From an epidemiological perspective, there's a lag-effect in 
# rainy season malaria transmission that extends to May, which is
# why for this analysis, the dry season is starting from June to October

dry_season_malaria_2020 <- read.csv(here::here("data", "ku_2020_malaria_cases.csv")) %>% 
  dplyr::select(Names = `organisationunitname`,
                June.2020, July.2020, August.2020, 
                September.2020, October.2020) %>% 
  dplyr::filter(Names != "Fpam Clinic Kasungu", 
                Names != "Kamuzu Academy Clinic") %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dr_2020 = sum(June.2020, July.2020,
                              August.2020, September.2020, 
                              October.2020, na.rm = TRUE))

dry_season_malaria_2019 <- read.csv(here::here("data/kasungu_malaria_2019.csv")) %>% 
  dplyr::as_tibble() %>% 
  dplyr::rename(Names = organisationunitname,
                January = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.January.2019,
                February = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.February.2019,
                March = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.March.2019,
                April = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.April.2019,
                May = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.May.2019,
                June = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.June.2019,
                July = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.July.2019,
                August = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.August.2019,
                September = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.September.2019,
                October = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.October.2019,
                November = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.November.2019,
                December = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.December.2019) %>% 
  dplyr::select(-c(organisationunitid, organisationunitcode, organisationunitdescription,
                   January, February, March, April, May, November, December)) %>% 
  dplyr::filter(Names != "Fpam Clinic Kasungu",       # don't have any records
                Names != "Kamuzu Academy Clinic",
                Names != "Chilanga Health Centre",
                Names != "Kapyanga Health Centre",
                Names != "St Augustin Anglican Clinic (Shayona)") %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dr_2019 = sum(June, July, August, September, October, na.rm = TRUE))

dry_season_malaria_2018 <- read.csv(here::here("data/kasungu_malaria_2018.csv")) %>% 
  dplyr::as_tibble() %>% 
  dplyr::rename(Names = organisationunitname,
                January = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.January.2018,
                February = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.February.2018,
                March = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.March.2018,
                April = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.April.2018,
                May = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.May.2018,
                June = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.June.2018,
                July = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.July.2018,
                August = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.August.2018,
                September = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.September.2018,
                October = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.October.2018,
                November = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.November.2018,
                December = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.December.2018) %>% 
  dplyr::select(-c(organisationunitid, organisationunitcode, organisationunitdescription,
                   January, February, March, April, May, November, December)) %>% 
  dplyr::filter(Names != "Fpam Clinic Kasungu",       # don't have any records
                Names != "Kamuzu Academy Clinic",
                Names != "Chilanga Health Centre",
                Names != "Kapyanga Health Centre",
                Names != "St Augustin Anglican Clinic (Shayona)") %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dr_2018 = sum(June, July, August, September, October, na.rm = TRUE))

dry_season_malaria_2017 <- read.csv(here::here("data/kasungu_malaria_2017.csv"))%>% 
  dplyr::as_tibble() %>% 
  dplyr::rename(Names = organisationunitname,
                January = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.January.2017,
                February = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.February.2017,
                March = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.March.2017,
                April = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.April.2017,
                May = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.May.2017,
                June = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.June.2017,
                July = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.July.2017,
                August = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.August.2017,
                September = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.September.2017,
                October = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.October.2017,
                November = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.November.2017,
                December = WHO.NMCP.P.Confirmed.malaria.cases.NMCP.December.2017) %>% 
  dplyr::select(-c(organisationunitid, organisationunitcode, organisationunitdescription,
                   January, February, March, April, May, November, December)) %>% 
  dplyr::filter(Names != "Fpam Clinic Kasungu",       # don't have any records
                Names != "Kamuzu Academy Clinic",
                Names != "Chilanga Health Centre",
                Names != "Kapyanga Health Centre",
                Names != "St Augustin Anglican Clinic (Shayona)") %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dr_2017 = sum(June, July, August, September, October, na.rm = TRUE))

              

# 2015 - 2019 NMCP confirmed malaria cases by health facility
kasungu_monthly_malaria <- read.csv(here::here("data/Kasungu_Monthly_facility_ Malaria data.csv")) %>% 
  dplyr::select(-c(periodcode, perioddescription, 
                   nmcp.confirmed.malaria.cases.rdt_central.east.zone)) 

# Helper function to rename columns by removing "nmcp.confirmed.malaria.cases.rdt_"
foo <- function(x) gsub("^[^_]*_", "", x)

kasungu_monthly_malaria_filter <- kasungu_monthly_malaria%>% 
  dplyr::rename_all(foo) %>% 
  dplyr::filter(!stringr::str_detect(periodname, "15"), # Remove years 2015 and 2016
                !stringr::str_detect(periodname, "16"),
                !stringr::str_detect(periodname, "Jan"), # Remove rainy months except may
                !stringr::str_detect(periodname, "Feb"),
                !stringr::str_detect(periodname, "Mar"),
                !stringr::str_detect(periodname, "Apr"),
                !stringr::str_detect(periodname, "Nov"),
                !stringr::str_detect(periodname, "Dec"))

# Helper function to subtract May malaria cases from the dry season malaria dataframe
subtract.2017.may.cases <- function(dry.season.malaria.df, may.malaria.df, year =NA,
                                    health.centre1 = NA, health.centre2 = NA, month = NA){
  
  dry.season.malaria <- dry.season.malaria.df$year[which(
    dry.season.malaria.df$Names == health.centre1)] <- dry.season.malaria.df$year[which(
             dry.season.malaria.df$Names == health.centre1)] - may.malaria.df$health.centre2[which(
                 may.malaria.df$periodname == month)]
  
  return(dry.season.malaria)
}

# Invoking function 
dry_season_malaria <- subtract.2017.may.cases(dry_season_malaria_2017_2020, 
                                              kasungu_monthly_malaria_filter,
                                              health.centre1 = "Anchor Farm",
                                              health.centre2 = "anchor.farm.health.centre",
                                              year = "dr_2017", month = "17-May")
# Subtract May malaria cases
dry_season_malaria_2017_2020$dr_2017[which(
  dry_season_malaria_2017_2020$Names == "Anchor Farm")] <- dry_season_malaria_2017_2020$dr_2017[which(
    dry_season_malaria_2017_2020$Names == "Anchor Farm")] - kasungu_monthly_malaria_filter$anchor.farm.health.centre[which(
      kasungu_monthly_malaria_filter$periodname == "17-May")]

dry_season_malaria_2017_2020$dr_2018[which(
  dry_season_malaria_2017_2020$Names == "Anchor Farm")] <- dry_season_malaria_2017_2020$dr_2018[which(
    dry_season_malaria_2017_2020$Names == "Anchor Farm")] - kasungu_monthly_malaria_filter$anchor.farm.health.centre[which(
      kasungu_monthly_malaria_filter$periodname == "18-May")]

dry_season_malaria_2017_2020$dr_2019[which(
  dry_season_malaria_2017_2020$Names == "Anchor Farm")] <- dry_season_malaria_2017_2020$dr_2019[which(
    dry_season_malaria_2017_2020$Names == "Anchor Farm")] - kasungu_monthly_malaria_filter$anchor.farm.health.centre[which(
      kasungu_monthly_malaria_filter$periodname == "19-May")]

dry_season_malaria_2017_2020$dr_2017[which(
  dry_season_malaria_2017_2020$Names == "Bua Health Centre")] <- dry_season_malaria_2017_2020$dr_2017[which(
    dry_season_malaria_2017_2020$Names == "Bua Health Centre")] - kasungu_monthly_malaria_filter$bua.hc.kasungu[which(
      kasungu_monthly_malaria_filter$periodname == "17-May")]

dry_season_malaria_2017_2020$dr_2018[which(
  dry_season_malaria_2017_2020$Names == "Bua Health Centre")] <- dry_season_malaria_2017_2020$dr_2018[which(
    dry_season_malaria_2017_2020$Names == "Bua Health Centre")] - kasungu_monthly_malaria_filter$bua.hc.kasungu[which(
      kasungu_monthly_malaria_filter$periodname == "18-May")]

dry_season_malaria_2017_2020$dr_2019[which(
  dry_season_malaria_2017_2020$Names == "Bua Health Centre")] <- dry_season_malaria_2017_2020$dr_2019[which(
    dry_season_malaria_2017_2020$Names == "Bua Health Centre")] - kasungu_monthly_malaria_filter$bua.hc.kasungu[which(
      kasungu_monthly_malaria_filter$periodname == "19-May")]

dry_season_malaria_2017_2020$dr_2017[which(
  dry_season_malaria_2017_2020$Names == "Chamama Health Facility")] <- dry_season_malaria_2017_2020$dr_2017[which(
    dry_season_malaria_2017_2020$Names == "Chamama Health Facility")] - kasungu_monthly_malaria_filter$chamama.health.facility[which(
      kasungu_monthly_malaria_filter$periodname == "17-May")]

dry_season_malaria_2017_2020$dr_2018[which(
  dry_season_malaria_2017_2020$Names == "Chamama Health Facility")] <- dry_season_malaria_2017_2020$dr_2018[which(
    dry_season_malaria_2017_2020$Names == "Chamama Health Facility")] - kasungu_monthly_malaria_filter$chamama.health.facility[which(
      kasungu_monthly_malaria_filter$periodname == "18-May")]

dry_season_malaria_2017_2020$dr_2019[which(
  dry_season_malaria_2017_2020$Names == "Chamama Health Facility")] <- dry_season_malaria_2017_2020$dr_2019[which(
    dry_season_malaria_2017_2020$Names == "Chamama Health Facility")] - kasungu_monthly_malaria_filter$chamama.health.facility[which(
      kasungu_monthly_malaria_filter$periodname == "19-May")]


dry_season_malaria_2017_2020$dr_2017[which(
  dry_season_malaria_2017_2020$Names == "Chamwabvi Health Centre")] <- dry_season_malaria_2017_2020$dr_2017[which(
    dry_season_malaria_2017_2020$Names == "Chamwabvi Health Centre")] - kasungu_monthly_malaria_filter$chamwabvi.disp[which(
      kasungu_monthly_malaria_filter$periodname == "17-May")]

dry_season_malaria_2017_2020$dr_2018[which(
  dry_season_malaria_2017_2020$Names == "Chamwabvi Health Centre")] <- dry_season_malaria_2017_2020$dr_2018[which(
    dry_season_malaria_2017_2020$Names == "Chamwabvi Health Centre")] - kasungu_monthly_malaria_filter$chamwabvi.disp[which(
      kasungu_monthly_malaria_filter$periodname == "18-May")]
dry_season_malaria_2017_2020$dr_2017[which(
  dry_season_malaria_2017_2020$Names == "Chamwabvi Health Centre")] <- dry_season_malaria_2017_2020$dr_2017[which(
    dry_season_malaria_2017_2020$Names == "Chamwabvi Health Centre")] - kasungu_monthly_malaria_filter$chamwabvi.disp[which(
      kasungu_monthly_malaria_filter$periodname == "17-May")]



kasungu_monthly_malaria_long <- kasungu_monthly_malaria_filter %>%
  tidyr::pivot_longer(cols = wimbe.hc:anchor.farm.health.centre,
                      names_to = "Names",
                      values_to = "malaria_cases")

kasungu_monthly_malaria_wide <- kasungu_monthly_malaria_long %>% 
  pivot_wider(names_from = periodname, values_from = malaria_cases) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(dry_2017 = sum(`17-Jun`, `17-Jul`, `17-Aug`, `17-Sep`, `17-Oct`, na.rm = TRUE))



# %>% 
#   tidyr::pivot_wider(names_from = periodname, 
#                      values_from = c(wimbe.hc, st.andrews.ch, simlemba.hc,
#                                      santhe.hc, ofesi.hc, nkhamenya.hosp,
#                                      newa..mpasazi.hc, mziza.health.centre,
#                                      mtunthama.hc, mpepa..chisinga.hc,
#                                      mnyanja.health.centre, mkhota.r.growth.hc,
#                                      mdunga.hc, lodjwa.hc, livwezi.health.centre,
#                                      linyangwa.hc, khola.hc, kawamba.hc,
#                                      kasungu.distr.hosp, kasalika.disp, kapelula.hc,
#                                      kamboni.hc, kaluluma.rh, gogode.hc, dwangwa.hc,
#                                      chulu.hc, chinyama.dispensary, chamwabvi.disp,
#                                      chamama.health.facility, bua.hc.kasungu, 
#                                      anchor.farm.health.centre)) %>% 
  tidyr::pivot_longer()

    
  


# Export -----------------------------------------------------------------------
write.csv(dry_season_malaria_2020, file = "data/dry_season_malaria_2020.csv")

# Eplore data ------------------------------------------------------------------
dry_season_malaria_2020 %>% dplyr::glimpse() %>%  
  DataExplorer::introduce() %>% 
  DataExplorer::plot_intro()

dry_season_malaria_2020 %>% DataExplorer::plot_missing()

dry_season_malaria_2020 %>% DataExplorer::plot_bar() 

dry_season_malaria_2020 %>% 
  DataExplorer::plot_correlation(maxcat = 5)


# Load raster and shapefile data -----------------------------------------------
accessibility_raster <- raster::raster(here::here("data/2015_friction_surface_v1.geotiff"))

accessibility_raster

ku_district <- shapefile(here::here("data", "kasungu_excluding_national_park"))

proj4string(accessibility_raster)

proj4string(ku_district)

ku_district <- spTransform(ku_district, proj4string(accessibility_raster)) 

# Health facility catchment boundary shapefile ---------------------------------
malire <- shapefile(here::here("data/kasungu_health_facility_catchments")) 

ku_malire <- spTransform(malire, proj4string(accessibility_raster))


# Plot map ---------------------------------------------------------------------
tm_shape(accessibility_raster)+
  tm_raster(palette = "Reds", style = "fisher", n = 5)+
  tm_layout(legend.position = c("left","bottom"),
            frame = FALSE)

# Crop and mask ----------------------------------------------------------------
clip_raster <- mask(crop(accessibility_raster, extent(ku_district)), ku_district) 

# raster_clip <- crop(accessibility_raster, extent(ku_district))
# ku_raster <- mask(raster_clip, ku_district)

# Check that clipping worked
plot(clip_raster)
plot(health_facility_sf, add=TRUE, lwd=1)

# Write the resulting raster ---------------------------------------------------
writeRaster(clip_raster, "data/friction_surface_clip.tif", overwrite=TRUE)

# Plot clipped map -------------------------------------------------------------
tm_shape(clip_raster)+
  tm_raster(palette = "Greys", style = "fisher", n = 5)+
  tm_layout(legend.position = c("right","bottom"),
            frame = FALSE)

# Load zipatala data -----------------------------------------------------------
# Master Health Facility Registry data http://zipatala.health.gov.mw/facilities
mhfr_fac <- readxl::read_xlsx(here::here("data/MHFR_Facilities.xlsx")) %>%  
            dplyr::select(NAME, `COMMON NAME`, OWNERSHIP, TYPE, STATUS, DISTRICT, 
                          LATITUDE, LONGITUDE) %>% 
            dplyr::filter(DISTRICT == "Kasungu",
                          STATUS == "Functional",
                          NAME != "St. Andrews Health Centre") # appears in Nkhotakota when mapped

# Using the is.na() function to remove the missing lon and lat coordinates
# Missing values in coordinates not allowed in sf transformation
mhfr_fac <- mhfr_fac[!is.na(mhfr_fac$LATITUDE),]

mhfr_fac_sf <- sf::st_as_sf(mhfr_fac, coords = c("LONGITUDE", "LATITUDE"),
                        crs = 4326, agr = "constant") 

mapview::mapview(mhfr_fac_sf, zcol = "OWNERSHIP", legend.opacity = .5)

# Malawi disaster management health facility data ------------------------------
# https://gis-malawi.com/
masdap_health_fac <- read.csv(here::here("data/malawi_health_masdap_2013.csv")) %>%  
                     dplyr::select(fid, fac_name, district, facility, x, y, owner) %>% 
                     dplyr::filter(district == "KASUNGU")

masdap_health_fac_sf <- sf::st_as_sf(masdap_health_fac, coords = c("x", "y"),
                                     crs = 4326, agr = "constant")

mapview::mapview(masdap_health_fac_sf, zcol = "owner", legend.opacity = .5)

primary_health_fac <- read.csv(here::here("data/health_facilities_moh_primary_health_facilities.csv")) %>%  
                      dplyr::select(fid, geom, facility_name, district, facility_type, status) %>% 
                      dplyr::filter(district == "Kasungu",
                                    status == "Functional",
                                    facility_type != "Outreach")

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

# Crop Kasungu National Park from the new health facility catchments -----------
# Read in new health facility catchment boundaries shapefile generated from accessibility mapping
new_health_fac_catchment <- shapefile(here::here("data", "new_health_facility_catchments"))
crs(new_health_fac_catchment)

mapview::mapview(new_health_fac_catchment)

# Read in old catchments excluding Kasungu National Park boundary 
old_health_fac_catchments <- shapefile(here::here("data", "old_catchments_excl_knp"))
crs(old_health_fac_catchments)

mapview::mapview(old_health_fac_catchments)

# Match projection
new_health_fac_catchment <- spTransform(new_health_fac_catchment, crs(kasungu_np))
crs(new_health_catchment)

# Save reprojected shapefile
# raster::shapefile(new_health_catchment, filename='data/reprojected_new_catchments.shp')

# Clip and mask
new_health_fac_catchment_clip <- raster::crop(new_health_fac_catchment, old_health_fac_catchments)

# Check if clipping worked
mapview::mapview(new_health_fac_catchment_clip)

# Save clipped new health facility catchment boundaries i.e. Kasungu NP excluded
raster::shapefile(new_health_catchment_clip, filename = "data/new_health_fac_catchment_clip.shp", overwrite = TRUE)
