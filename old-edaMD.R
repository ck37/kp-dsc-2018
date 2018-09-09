library(knitr)
library(readr)
library(tidyverse)
training <- read.csv("I:/DSC/kp-dsc-2018/data-raw/train.csv")
names(training) <- tolower(names(training))


#install.packages("GGally")
library(GGally)                             # requires cran version of ggplot2
set.seed(830015)                             #use your anniv as a seed and never forget it
sample <- training %>% sample_n(500)
g <- ggpairs(training, progress = FALSE, alpha = .25)
g

  install.packages("devtools")
library(devtools)
install_github("hafen/trelliscopejs")

#
install.packages("trelliscopejs")
install.packages("magrittr")
library(magrittr)
library(trelliscopejs)
temp <- sample %>% ungroup() %>% select(-eventid) %>%  gather(-pri_jet_num, key = "var", value = "value")
# get names of installed packages
packs <- installed.packages()
exc <- names(packs[,'Package'])

# get available package names
av <- names(available.packages()[,1])

# create loooong string
ins <- av[!av %in% exc]
install.packages(ins)

ggplot(training, aes(value, group = factor(pri_jet_num), fill = factor(pri_jet_num))) +
  geom_density(alpha=.35) +
  theme_bw() +
  scale_fill_viridis_d()+
  facet_trelliscope(~ var, scales = "free", nrow = 3, ncol = 5, self_contained = TRUE)
