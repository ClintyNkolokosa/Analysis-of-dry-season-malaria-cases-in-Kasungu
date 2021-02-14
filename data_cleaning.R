# tell R where the data is
here::here()

# Load 2020 NMCP confirmed cases data, remove unnecessary columns and rename
kasungu_nmcp_2020 <- read.csv(here::here("data", "ku_nmcp_confirmed_cases_2020.csv")) %>% 
                     dplyr::select(FACILITY_N = `organisationunitname`,
                                   dr_2020 = `X2020`) %>% 
                     dplyr::filter(FACILITY_N != "Fpam Clinic Kasungu", 
                                   FACILITY_N != "K2 Taso Clinic",
                                   FACILITY_N != "Kamuzu Academy Clinic")

View(kasungu_nmcp_2020)
glimpse(kasungu_nmcp_2020)


# Filtering out dry season malaria cases and tidying the data
# Typically Malawi can be divided into the following seasons:
#  - a hot and rainy season from November to April 
#  - and a relatively cool and dry season from May to October
# Source: https://www.metmalawi.com/climate/climate.php

ku_dry_season_malaria_cases_2020 <- read.csv(here::here("data", "ku_2020_malaria_cases.csv")) %>% 
                                     dplyr::select(FACILITY_N = `organisationunitname`,
                                                   May.2020, June.2020, July.2020,
                                                   August.2020, September.2020, October.2020) %>% 
                                     dplyr::rowwise() %>% 
                                     dplyr::mutate(dr_2020 = sum(dplyr::c_across(May.2020:October.2020)))
ku_2020_malaria_cases

