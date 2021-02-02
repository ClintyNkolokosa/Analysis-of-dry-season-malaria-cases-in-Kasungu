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

