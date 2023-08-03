
## ***************************************************************************
## 02-figures
## Script to create paper's figures 
## ***************************************************************************

library(openxlsx)
library(tidyverse)

rm(list = ls())

source("R/Extract.r") # Policy Mood function (www.stimson.web.unc.edu)


##  Figure  1. (Policy Mood series)  =======================================

# Read data
data <- readxl::read_excel("Data/pmood.xlsx")

elections <- c("1994","1999", "2004", "2009", "2014", "2019")

data %>% 
  ggplot(aes(x = year, y = pmood)) +
  geom_point(size = 3, colour = "black") +
  geom_line(size = 1.5, colour = "black") +
  theme_minimal() +
  geom_hline(yintercept=50,linetype="solid") +
  geom_vline(xintercept = as.numeric(elections), linetype="dashed", size=1, color="grey30") +
  labs(x = "", 
       y = "Percent left"
       # ,
       # title = "Policy mood in Uruguay"
  ) +
  theme(legend.position = "bottom") +
  ylim(30, 70) +
  scale_x_continuous(breaks=c(seq(1994, 2019, 5))) 



##  Figure  2. (Policy Mood & symbolic ideology)  ==========================

data %>% 
  pivot_longer(cols = pmood:pvs_autid_coef,
               names_to = "indicador",
               values_to = "valor") %>% 
  ggplot(aes(x = year, y = valor, colour = indicador, linetype = indicador)) +
  geom_point(size = 3) +
  geom_line(size =1.5) +
  theme_minimal() +
  labs(x = "", 
       y = "") +
  scale_color_manual(name = "",
                     labels = c("pmood" = "Policy Mood",
                                "pvs_autid_coef" = "Symbolic Idelogy"),
                     values = c("pmood" =  "black", # dodgerblue4
                                "pvs_autid_coef" =  "#99A3A4")) + # chocolate2
  scale_linetype_manual(name = "",
                        labels = c("pmood" = "Policy Mood",
                                   "pvs_autid_coef" = "Symbolic Idelogy"),
                        values = c("solid", "dashed")) +
  theme(legend.position = "bottom",
        legend.key.width = unit(3,"cm")) 



#  Figure  3. (Variation in Policy Mood by government)  ===================

## Changes in policy mood by government 
pmood_data <- data %>%
  rename(valor = pmood) %>% 
  select(year, valor) 

var9499 <- round((filter(pmood_data, year == 1999) %>% pull(valor) - 
                    filter(pmood_data, year == 1994) %>% pull(valor)) / 
                   filter(pmood_data, year == 1994) %>% pull(valor) * 100,
                 digits = 2)
var9904 <- round((filter(pmood_data, year == 2004) %>% pull(valor) - 
                    filter(pmood_data, year == 1999) %>% pull(valor)) / 
                   filter(pmood_data, year == 1999) %>% pull(valor) * 100,
                 digits = 2)
var0409 <- round((filter(pmood_data, year == 2009) %>% pull(valor) - 
                    filter(pmood_data, year == 2004) %>% pull(valor))/ 
                   filter(pmood_data, year == 2004) %>% pull(valor) * 100,
                 digits = 2)
var0914 <- round((filter(pmood_data, year == 2014) %>% pull(valor) - 
                    filter(pmood_data, year == 2009) %>% pull(valor))/ 
                   filter(pmood_data, year == 2009) %>% pull(valor) * 100,
                 digits = 2)
var1419 <- round((filter(pmood_data, year == 2019) %>% pull(valor) - 
                    filter(pmood_data, year == 2014) %>% pull(valor))/ 
                   filter(pmood_data, year == 2014) %>% pull(valor) * 100,
                 digits = 2)

var_pmood <- c(var9499, var9904, var0409, var0914, var1419)
periodo <- c("1994-99", "1999-2004", "2004-09", "2009-14", "2014-19")

data_var <- tibble(periodo = periodo,
                   pmood = var_pmood) 

## Load PELA data
presidentes <- readxl::read_excel("data/pela_data.xlsx")

presidentes <- presidentes %>% 
  mutate(mean_ideo_lag = lag(mean_ideo)) %>% 
  filter(pte != "Lacalle") %>% 
  mutate(valor = ((mean_ideo - mean_ideo_lag) / mean_ideo_lag) * 100) %>% 
  select(valor) 

data_var$var_presi <- presidentes$valor

data_var <- data_var %>% 
  pivot_longer(cols = pmood:var_presi)

# Figure
ggplot(data_var,
       aes(x = fct_rev(periodo), y = value, fill = name)) +
  geom_col(color = "black", position = "dodge") +
  theme_minimal() +
  labs(x = "", 
       y = "",
       title = "") +
  scale_x_discrete(labels = c("1994-99" = "J.M. Sanguinetti (1994-1999) \n (Partido Colorado)",
                              "1999-2004" = "J. Batlle (1999-2004) \n (Partido Colorado)",
                              "2004-09" = "T. Vázquez (2004-2009) \n (Frente Amplio)",
                              "2009-14" = "J. Mujica (2009-2014) \n (Frente Amplio)",
                              "2014-19" = "T. Vázquez (2014-2019) \n (Frente Amplio)")) +
  theme(legend.position = "bottom") +
  coord_flip() +
  annotate("segment", x = 5, y = 100, xend = 5, yend = 110,
           arrow = arrow(type = "closed", length = unit(0.03, "npc"))) +
  annotate("text",
           label = "more leftist government",
           x = 5.25, 
           y = 100,
           fontface = "bold") +
  annotate("text",
           label = "more leftist mood",
           x = 4.75, 
           y = 90,
           fontface = "bold") +
  scale_fill_manual(name = "",
                    values = c("black", 
                               "#99A3A4"),
                    labels = c("Change in Policy Mood", "Change in President's ideology")) +
  ylim(-50, 140)

