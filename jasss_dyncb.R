library(dplyr)
library(readxl)
library(readr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(plotly)
library(ggpubr)
library(grid)
library(writexl)



setwd("C:/Users/LENOVO/Desktop/MIE/SOCMULT_JASSS/")

folders <- c("childbirth_0op","childbirth_1op","childbirth_2op",
             "childbirth_3op","childbirth_4op","childbirth_5op")

filesind <- unlist(
  lapply(folders, function(x)
    list.files(paste0("indiv_selection/", x, "/"), full.names = TRUE))
)

df_sel <- bind_rows(
  lapply(filesind, read.csv, sep = ",")
)

df_sel <- df_sel %>% mutate(
  correctmatch = case_when(
    selectedhospital == selectedhospitalemp ~ 1,
    selectedhospital != selectedhospitalemp ~ 0
  )
)

dfgroup <- df_sel %>%
  group_by(weight_distance_hospital,
           weight_opinion,
           social_multiplier,
           weight_experience) %>%
  summarise(
    sumcr = sum(correctmatch == 1, na.rm = TRUE),
    total = n(),
    accuracy = sumcr / total,
    .groups = "drop"
  )

dfgroup %>%
  ggplot(aes(x = as.factor(weight_distance_hospital),
             y = as.factor(weight_opinion),
             fill = accuracy)) +
  geom_tile() +
  facet_grid(
    rows = vars(weight_experience),
    cols = vars(social_multiplier),
    labeller = labeller(
      weight_experience = label_both,
      social_multiplier = label_both
    )
  ) +
  geom_text(aes(label = round(accuracy, 2)), size = 3, color = "white") +
  labs(
    x = "weight_distance_hospital",
    y = "weight_opinion"
  ) + 
  scale_fill_viridis_c(limits = c(0, 1)) +
  theme_bw()




# for the longitudinal dynamics

# back
# dft0 <- read.csv("long_format/childbnet op0-table.csv",sep = ";",skip = 8)
# dft1 <- read.csv("long_format/childbnet op1-table.csv",sep = ";",skip = 8)
# dft2 <- read.csv("long_format/childbnet op2-table.csv",sep = ";",skip = 8)
# dft3 <- read.csv("long_format/childbnet op3-table.csv",sep = ";",skip = 8)
# dft4 <- read.csv("long_format/childbnet op4-table.csv",sep = ";",skip = 8)
# dft5 <- read.csv("long_format/childbnet op5-table.csv",sep = ";",skip = 8)

names_recode <- read_xlsx("names_recode.xlsx")

df_all <- list()

files <- list.files("long_format", full.names = TRUE)

for (i in seq_along(files)) {
  
  df <- read.csv(files[i], sep = ";", skip = 8)

  df_all[[i]] <- df
}

df_all <- bind_rows(df_all)

