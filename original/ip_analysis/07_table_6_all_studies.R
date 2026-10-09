"
Table 6
"

# set dir
setwd(dropboxdir)
setwd("turk/replication_public/")

# Load lib
library(tidyverse)
library(readxl)
library(magrittr)
library(dplyr)
library(ggplot2)
library(xtable)
library(goji)

# Read in the data
t1   <- read.csv("data/turk_08_17_2018/turk_recoded_public.csv")
t2   <- read.csv("data/turk_06_29_2020/merged_survey_ip_06_29_2020_final_public.csv")
t3   <- read.csv("data/turk_07_12_2020/merged_survey_ip_07_12_2020_final_public.csv")
# ssi_turk   <- read.csv("data/turk_ssi_data/turk_ssi_recoded_public.csv")
# lucid <- read.csv( "data/busby_lucid/lucid_ip_recoded.csv") 

columns <- c("ip_index", "pollname", "platform", "blacklisted", "duplicated", "foreign_ip", "missing_ip", "maxmindcitynamesen", "maxmindcountrynamesen", "apivoidanonymityis_vpn", "funny_ip")

t1_sub  <- t1 %>% select(all_of(columns))
t2_sub  <- t2 %>% select(all_of(columns))
t3_sub  <- t3 %>% select(all_of(columns))
# ssi_turk_sub <- ssi_turk %>% select(all_of(columns))
# lucid_sub <- lucid %>% select(all_of(columns))

# all_polls <- rbind(t1_sub, t2_sub, t3_sub, ssi_turk_sub, lucid_sub)
all_polls <- rbind(t1_sub, t2_sub, t3_sub)

# Analysis
# ----------------

# Duplicated
all_polls %>%
  group_by(pollname) %>%
  summarize(dups = sum(duplicated, na.rm = T),
            n = n())

# Foreign or Duplicated
all_polls %>%
  group_by(pollname) %>%
  summarize(foreign_or_dup = sum(foreign_ip | duplicated, na.rm = T))

## Table 6
# -----------------------

tab6_si <- all_polls %>%
    group_by(pollname) %>%
    summarize(perc_missing = sum(missing_ip)*100/n(),
              perc_blacklisted = sum(blacklisted, na.rm = T)*100/n(),
              perc_duplicated = sum(duplicated, na.rm = T)*100/n(),
              perc_foreign = sum(foreign_ip, na.rm = T)*100/n(),
              perc_any = sum(funny_ip, na.rm = T)*100/n(),
              n = n(),
              platform = unique(platform)) %>%
    mutate(pollname = fct_relevel(pollname, "June 2020 Study", after = 1)) %>%
    arrange(pollname) %>%
    mutate_at(vars(perc_missing:perc_any), list(~ paste0(round(., 1), "%")))

tab6_si_sub <- tab6_si %>% select(-platform)

names(tab6_si_sub) <- c("Survey", "Missing", "Blacklisted", "Duplicated", "Foreign", "Any", "N")


### Compressed Version of Table 6 for Main Text
tab6 <- tab6_si %>% select(pollname, platform, perc_any)
names(tab6) <- c("Survey", "Platform", "% Suspicious IP Addresses")

# For the rest of Table 6, please get in touch with us as it uses data that is not ours. 