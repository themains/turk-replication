"
Table 3: Incidence of Suspicious IP Addresses Across Three Studies | CDG Studies
Figure 1/SI: Troll Qs. Histogram by Study | CDG Studies
Table SI 1.1: How many times an IP address appears in the Data
Table SI 1.2: Country
Table SI 1.3: City
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

columns <- c("ip_index", "pollname", "prosthetic", "blind", "deaf", "gang_resp", "gang_fam", "sleep", "counts", "troll_count_2", "blacklisted", "foreign_ip", "duplicated", "missing_ip", "funny_ip", "untrustworthy", "maxmindcitynamesen", "maxmindcountrynamesen")
t1_sub <- t1 %>% select(all_of(columns))
t2_sub <- t2 %>% select(all_of(columns))
t3_sub <- t3 %>% select(all_of(columns))

all_polls <- rbind(t1_sub, t2_sub, t3_sub)

# Analysis
# ----------------

# Foreign or Duplicated
all_polls %>%
  group_by(pollname) %>%
  summarize(foreign_or_dup = sum(foreign_ip | duplicated, na.rm = T))

## Table 3
# -----------------------

tab3 <- all_polls %>%
    group_by(pollname) %>%
    summarize(perc_missing = sum(missing_ip)*100/n(),
              perc_blacklisted = sum(blacklisted, na.rm = T)*100/n(),
              perc_duplicated = sum(duplicated, na.rm = T)*100/n(),
              perc_foreign = sum(foreign_ip, na.rm = T)*100/n(),
              perc_any = sum(funny_ip, na.rm = T)*100/n(),
              n = n()) %>%
    mutate(pollname = fct_relevel(pollname, "June 2020 Study", after = 1)) %>%
    arrange(pollname) %>%
    mutate_at(vars(perc_missing:perc_any), list(~ paste0(round(., 1), "%")))

names(tab3) <- c("Survey", "Missing", "Blacklisted", "Duplicated", "Foreign", "Any", "N")

print(xtable(tab3,
  caption = "Incidence of Suspicious IP Addresses Across Three Studies",
  label   = "tab:tab3",
  align   = "llcccccc",
  digits  = 0),
  include.rownames = F
)

## Table SI 1.1: How many times an IP address appears in the Data

tab_si_11 <- all_polls %>%
    group_by(pollname, ip_index) %>%
    tally() %>%
    select(-ip_index) %>%
    group_by(pollname, n) %>%
    tally() %>%
    spread(n, nn) %>%
    replace(is.na(.), 0) %>%
    ungroup()  %>%
    mutate(pollname = fct_relevel(pollname, "June 2020 Study", after = 1)) %>%
    arrange(pollname)

print(xtable(tab_si_11,
      caption = "Number of Times an IP Address Appears in the Data",
      label   = "tab:tab_si_11",
      align   = "llccccccc",
      digits  = 0),
      include.rownames = F
    )


## Table SI 1.2: Countries
# ------------------------

tab_si_12 <- all_polls %>%
    filter(!is.na(maxmindcountrynamesen)) %>%
    group_by(pollname, maxmindcountrynamesen) %>%
    tally() %>%
    mutate(maxmindcountrynamesen = if_else(n <= 5, "Other", maxmindcountrynamesen)) %>%
    group_by(pollname, maxmindcountrynamesen)  %>%
    summarize(no = sum(n), .groups = 'drop') %>%
    spread(maxmindcountrynamesen, no) %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    mutate(pollname = fct_relevel(pollname, "June 2020 Study", after = 1)) %>%
    arrange(pollname)

print(xtable(tab_si_12,
          caption = "Country of Origin",
          label   = "tab:tab_si_12",
          align   = "llccccc",
          digits  = 0),
          include.rownames = F
)

## Table SI 1.3: Cities
# ----------------------------------

tab_si_13 <- all_polls %>%
    filter(!is.na(maxmindcitynamesen)) %>%
    group_by(pollname, maxmindcitynamesen) %>%
    tally() %>%
    mutate(maxmindcitynamesen = if_else(n <= 25, "Other", maxmindcitynamesen)) %>%
    group_by(pollname, maxmindcitynamesen)  %>%
    summarize(no = sum(n), .groups = 'drop') %>%
    spread(maxmind.city.names.en, no) %>%
    replace(is.na(.), 0) %>%
    ungroup() %>%
    mutate(pollname = fct_relevel(pollname, "June 2020 Study", after = 1)) %>%
    arrange(pollname)

print(xtable(tab_si_13,
          caption = "City of Origin",
          label   = "tab:tab_si_13",
          align   = "llcccccccc",
          digits  = 0),
          include.rownames = F
)

## Fig SI 1
# ----------------------

# Custom ggplot theme
cust_theme <-
   theme_minimal() +
   theme(panel.grid.major = element_line(color = "#e1e1e1",  linetype = "dotted"),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.key       = element_blank(),
      legend.key.width = unit(1, "cm"),
      axis.title   = element_text(size = 12, color = "#555555"),
      axis.text    = element_text(size = 12, color = "#555555"),
      axis.title.x = element_text(vjust = 1, margin = margin(12, 0, 0, 0)),
      axis.title.y = element_text(vjust = 1),
      axis.ticks   = element_line(color = "#e1e1e1", linetype = "dotted", size = .2),
      axis.text.x  = element_text(vjust = .3),
      plot.margin = unit(c(.5, .75, .5, .5), "cm"))

ggplot(all_polls, aes(x = counts)) +
  geom_histogram(colour = "grey", position = "identity", binwidth = 1, alpha = 0.45) +
  ylab("Number of Responses") +
  xlab("Number of Affirmative Answers to Infrequent Things") +
  #scale_x_continuous(breaks = 0:5, expand = c(0, 0), limits = c(-1, 6)) +
  cust_theme +
  facet_wrap(~pollname, ncol = 1)
