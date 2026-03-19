##############################################
#
# FlexER Scale - 10 Item Version
#
# Timo Schweikert, 
# Christoph Scheffel
#
# Exploratory CFA
##############################################

# load packages

library(renv)
library(here)
here::i_am("FlexERScale.Rproj")

library(dplyr)
library(tidyverse)

library(lavaan)    # CFA/SEM
library(semTools)  # Additional functions for reliability, validity, etc.
library(semPlot)
library(psych)     

### DATA IMPORT

# import questionnaire data from ICER-SV Study

data_icersv <- read.csv(here("datasheets", "10item", "ICER_SV_Survey.csv"),
                        stringsAsFactors = FALSE, header = TRUE,
                        na.strings = c("", "NA"))
colnames(data_icersv)[1] <- "set" # rename the first column
# tidy up data frame:
# fill subject id in each row and collapse arms into one row per participant

data_icersv <- data_icersv %>% 
  data.table::as.data.table() %>% 
  replace(is.na(data_icersv), "")

data_icersv <- data_icersv[, lapply(.SD, paste0 , collapse=""), by=set]

data_icersv <- as.data.frame(data_icersv)

data_icersv <- data_icersv %>%
  subset(!is.na(set), select = c(set, age, gender, edu,  grep("flexer", colnames(data_icersv))))

data_icersv[, c("age", "gender", "edu")] <- sapply(data_icersv[, c("age", "gender", "edu")], as.numeric)

#### Flexible Emotion Regulation Scale - FlexER

# store flexer items in separate df
# only keep rows with complete flexer questionnaire
data_CFA <- data_icersv %>% 
  subset(select = c(set,
                    grep("flexer", colnames(data_icersv)))) %>% 
  dplyr::filter((flexer_scale_complete == 2))

data_CFA[, c("flexer_01", "flexer_02", "flexer_03", "flexer_04", "flexer_05", "flexer_06", "flexer_07", "flexer_08", "flexer_09", "flexer_10")] <- sapply(data_CFA[, c("flexer_01", "flexer_02", "flexer_03", "flexer_04", "flexer_05", "flexer_06", "flexer_07", "flexer_08", "flexer_09", "flexer_10")], as.numeric)

# compute ERF mean score --> lower values reflect higher flexibility
data_CFA$ERF_mean <- rowMeans(data_CFA[c("flexer_01", "flexer_02", "flexer_03", "flexer_04", "flexer_05", "flexer_06", "flexer_07", "flexer_08", "flexer_09", "flexer_10")])

# recode mean score, because we want higher values to reflect higher flexibility

data_CFA$ERF <- 5 - data_CFA$ERF_mean

### CONFIRMATORY FACTORY ANALYSES

# model with one factor

model_1factor <- 'ERF1 =~ flexer_01 + flexer_02 + flexer_03 + flexer_04 + flexer_05 + flexer_06 + flexer_07 + flexer_08 + flexer_09+ flexer_10'
model_1factor <- cfa(model_1factor, data = data_CFA, estimator ="ml")
semPlot::semPaths(model_1factor, whatLabels="stand")
summary(model_1factor, sta=T,fit.measure =TRUE)


# model with two factors

model_2factor <- '
  ERF1 =~ flexer_01 + flexer_04 + flexer_05 + flexer_06 + flexer_07 + flexer_10
  ERF2 =~ flexer_02 + flexer_03 + flexer_08 + flexer_09
'
model_2factor <- cfa(model_2factor, data = data_CFA, estimator ="ml")
semPlot::semPaths(model_2factor, whatLabels="stand")
summary(model_2factor, sta=T,fit.measure =TRUE)

# model comparison

# Modellvergleich (Standard-Schätzer)
anova(model_1factor, model_2factor)
# oder
lavTestLRT(model_1factor, model_2factor)

