#############################################################################################
#           Analysis of Role of Dams on Recorded Dry Season Malaria Incidences in Kasungu  ##
#############################################################################################

#################
# Set path name #
#################

pathname <- "C:/GIS Folder/2020/Vector/finalized_data/finalized.shp"




####################
# Read in packages #
####################


library(dplyr)
library(ggplot2)
library(sf)
library(sp)


########################
# Load the Data files  #
########################


finalized_data <- st_read(paste0(pathname,"finalized.shp"))

finalized_data$prop_0_5km_2018 <- finalized_data$pop_0_5ksu/finalized_data$X__2018
finalized_data$prop_1km_2018 <- finalized_data$pop_1ksum/finalized_data$X__2018
finalized_data$prop_2km_2018 <- finalized_data$pop_2ksum/finalized_data$X__2018
finalized_data$prop_3km_2018 <- finalized_data$pop_3ksum/finalized_data$X__2018

summary(finalized_data$prop_0_5km_2018)
summary(finalized_data$prop_1km_2018)
summary(finalized_data$prop_2km_2018)
summary(finalized_data$prop_3km_2018)

##############################
# Model Fitting            ###
##############################


######
mod.0_5k <- glm(dr_2018~1+prop_0_5km_2018+offset(log(ex_2018)), data=finalized_data, family="poisson")
summary(mod.0_5k) #AIC = 38503


mod.1k <- glm(dr_2018~1+prop_1km_2018+offset(log(ex_2018)), data=finalized_data, family="poisson")
summary(mod.1k) #AIC = 37047


mod.2k <- glm(dr_2018~1+prop_2km_2018+offset(log(ex_2018)), data=finalized_data, family="poisson")
summary(mod.2k) #AIC = 36736

mod.3k <- glm(dr_2018~1+prop_3km_2018+offset(log(ex_2018)), data=finalized_data, family="poisson")
summary(mod.3k) #AIC = 39157


# Lowest AIC found when distance  = 2km

exp(coef(mod.2k))

# This tells us that living within 2km increases risk of dry season malaria as there's a significant positive relationship
# with prop of population living within 2km and risk. If all lived within 2km of water, risk of malaria would be 3.19 times
# higher compared to noone living close to water.

exp(confint(mod.2k))

# Confidence interval 3.09 - 3.29
