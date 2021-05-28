#############################################################################################
#           Analysis of Role of Dams on Recorded Dry Season Malaria Incidences in Kasungu  ##
#############################################################################################

# The aim is to determine the standardised mortality/morbidity ratio (SMR). 
# SMR compares the risk of morbidity/mortality in a population of interest with 
# that of a standard population. In this case, interest in understanding whether
# the number of dry season malaria cases in each health facility catchment are 
# greater than we would expect given the rate of malaria in the whole of Kasungu district.

############################
# Tell R where the data is#
##########################

# Make sure you're in the directory where there's data and scripts for this project

here::here()  


#############################
# Loading required packages #
############################


library(dplyr)
library(ggplot2)
library(sf)
library(sp)
library(lme4)
library(sjPlot)


########################
# Load the Data files  #
########################

kasungu_model <- read.csv(here::here("data", "Kasungu_hfc_malaria_pop_2017_2020.csv"))


##############################################
# Defining model parameters and their values #
##############################################

# dr_2017, dr_2018, dr_2019 and dr_2020 are recorded dry season malaria cases in Kasungu
# malariaProp_1km, malariaProp_2km and malariaProp_3km are the malaria proportion 
# (total number of malaria cases divided by total population) within 1km, 2km and 3km buffers respectively
# expectedMalaria is the number of malaria cases we would expect given the rate of malaria, and is 
# calculated by dividing total malaria cases with total population and multiplied
# by the number of people in each health facility.


##############################
# Model Fitting            ###
##############################

# Modelling the observed outcomes using `Poisson regression` with an offset of 
# the log of the expected counts `expectedMalaria`. `Poisson regression` framework 
# has been used as it is suitable for modeling count outcomes. 


# 2017 -----------------------------------------------------------------------------------
model_1km_2017 <- glm(dr_2017~malariaProp_1km_2017, offset = log(expectedMalaria_1km_2017),
                      data = kasungu_model_df, family = 'poisson', na.action = na.omit)
# alias(model_1km_2017)
# sjPlot::tab_model(model_1km_2017)

summary(model_1km_2017) # AIC: 69758

model_2km_2017 <- glm(dr_2017~malariaProp_2km_2017, offset = log(expectedMalaria_2km_2017),
                      data = kasungu_model_df, family = 'poisson', na.action = na.omit)

summary(model_2km_2017) # AIC: 46655

model_3km_2017 <- glm(dr_2017~malariaProp_3km_2017, offset = log(expectedMalaria_3km_2017),
                      data = kasungu_model_df, family = 'poisson', na.action = na.omit)

summary(model_3km_2017) # AIC: 56808

# 2018 -----------------------------------------------------------------------------------
model_1km_2018 <- glm(dr_2018~malariaProp_1km_2018, offset = log(expectedMalaria_1km_2018),
                      data = kasungu_model_df, family = 'poisson', na.action = na.omit)

summary(model_1km_2018) # AIC: 69024

model_2km_2018 <- glm(dr_2018~malariaProp_2km_2018, offset = log(expectedMalaria_2km_2018),
                      data = kasungu_model_df, family = 'poisson', na.action = na.omit)

summary(model_2km_2018) # AIC: 47856

model_3km_2018 <- glm(dr_2018~malariaProp_3km_2018, offset = log(expectedMalaria_3km_2018),
                      data = kasungu_model_df, family = 'poisson', na.action = na.omit)

summary(model_3km_2018) # AIC: 38164

# 2019 ----------------------------------------------------------------------------------
model_1km_2019 <- glm(dr_2019~malariaProp_1km_2019, offset = log(expectedMalaria_1km_2019),
                      data = kasungu_model_df, family = 'poisson', na.action = na.omit)

summary(model_1km_2019) # AIC: 60170

ggplot() + geom_point(aes(model_1km_2019$fitted.values, kasungu_model_df$dr_2019)) # scatter plot

model_2km_2019 <- glm(dr_2019~malariaProp_2km_2019, offset = log(expectedMalaria_2km_2019),
                      data = kasungu_model_df, family = 'poisson', na.action = na.omit)

summary(model_2km_2019) # AIC: 60123 

model_3km_2019 <- glm(dr_2019~malariaProp_3km_2019, offset = log(expectedMalaria_3km_2019),
                      data = kasungu_model_df, family = 'poisson', na.action = na.omit)

summary(model_3km_2019) # AIC: 50372

# 2020 ----------------------------------------------------------------------------------
model_1km_2020 <- glm(dr_2020~malariaProp_1km_2020, offset = log(expectedMalaria_1km_2020),
                      data = kasungu_model_df, family = 'poisson', na.action = na.omit)

summary(model_1km_2020) # AIC: 112351

model_2km_2020 <- glm(dr_2020~malariaProp_2km_2020, offset = log(expectedMalaria_2km_2020),
                      data = kasungu_model_df, family = 'poisson', na.action = na.omit)

summary(model_2km_2020) # AIC: 118612

model_3km_2020 <- glm(dr_2020~malariaProp_3km_2020, offset = log(expectedMalaria_3km_2020),
                      data = kasungu_model_df, family = 'poisson', na.action = na.omit)

summary(model_3km_2020) # AIC: 99461


# Lowest AIC found when distance  = 3km

exp(coef(model_3km_2017))

exp(coef(model_3km_2018))

exp(coef(model_3km_2019))

exp(coef(model_3km_2020))

# Confidence interval

exp(confint(model_3km_2017))

exp(confint(model_3km_2018))

exp(confint(model_3km_2019))

exp(confint(model_3km_2020))
