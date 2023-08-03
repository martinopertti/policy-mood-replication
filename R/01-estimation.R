
## **************************************************************************
## 01-Estimation 
## Script to estimate the Policy Mood export and plot it
# **************************************************************************

library(openxlsx)
library(tidyverse)

rm(list = ls())

source("R/Extract.r") # Policy Mood function (www.stimson.web.unc.edu)


## 1. Load and prepare data   ==============================================

joined_data <- read.xlsx("Data/raw-data.xlsx") 

joined_data <- joined_data %>% 
  mutate(date = as.Date(paste(joined_data$fecha, 6, 6, sep = "-")),
         score = as.numeric(score))  
  

## 2. Descriptive statistics   =============================================

length(unique(joined_data$variable)) # Number of questions
nrow(joined_data) # Number of administrations

# Administrations per source
joined_data %>% 
  group_by(fuente) %>% 
  summarise(n = n()) %>% 
  mutate(per = n / sum(n) * 100)


## 3. Estimation    =======================================================

output <- extract(joined_data$variable, # varname
                  joined_data$date, # date
                  joined_data$score, # score
                  joined_data$n_casos, # ncases
                  unit = "A", # Units (A = anual)
                  begindt = ISOdate(1993, 1, 1), # Starting date
                  enddt = ISOdate(2020, 12, 12), # End date
                  smoothing = TRUE) # Exponential smoothing applied

display(output) # Series
summary(output) # Loadings



## 4. Results    ===========================================================

# Extract series
year <- output$period
pmood <- output$latent1
dim_latente <- data.frame(cbind(year, pmood))

# Extract loadings
vars <- output$varname
loads <- as.numeric(output$loadings1)
n_cases <- as.numeric(output$N)
loadings <- data.frame(cbind(vars, loads, n_cases))
loadings <- loadings %>% 
  arrange(desc(loads))

