---
title: "Clean raw data"
output: html_document
---

```{r setup, include=TRUE}
knitr::opts_chunk$set(echo = TRUE)
if (FALSE) {
# Load an initial pair of startup functions.
source("R/_startup.R")
# Load necessary libraries; set auto_install = TRUE to try to install any needed packages.
startup(auto_install = FALSE, verbose = FALSE)
# Load all .R files in the R/ subdirectory.
ck37r::load_all_code("R", verbose = TRUE)
```
}
```{r import_data}

# Import as a data.frame rather than a data.table - avoids extra complication.
#need to install packages data.table first
data = data.table::fread("data-raw/train.csv", data.table = FALSE)
dim(data)

# examine the structure of the data
names(data) = tolower(names(data))
str(data)

# Review classes in our dataset.
table(sapply(data, class))
sapply(data, class)

# Review the number of unique values in each column.
sapply(data, function(col) { length(unique(col)) })

# pri_jet_num has only 4 so let's convert it to a factor.
data$pri_jet_num = as.factor(data$pri_jet_num)
pri_jet_num_table<- table(data$pri_jet_num)
prop.table(pri_jet_num_table)

#       0        1        2        3
#0.399652 0.310176 0.201516 0.088656
```

```{r data_prep}
# Define our outcome variable.
var_outcome = "label"

label_table
# b      s
#164333  85667
#b        s
#0.657332 0.342668



# Specify which variables we don't want to use for prediction.
remove_cols = c(var_outcome,
                # Simply notes that this is all training data.
                "kaggleset",
                # TODO: figure out what these columns mean.
                "eventid", "kaggleweight", "weight")

# Create a list to save our outcome, data, and covariates.
# This can then be used for EDA, modeling, etc.
task = list(
  # Save our primary dataset.
  data = data,
  # Specify which covariates we can use for predicting the outcome
  # (these are only the column names, which can be indexed into task$data).
  covariates = names(data)[!names(data) %in% remove_cols],
  # Specify the outcome vector.
  outcome = as.integer(data[[var_outcome]] == "s")
)

# Review covariates.
task$covariates

# Review outcome distribution.
table(task$outcome, useNA = "ifany")
prop.table(table(task$outcome, useNA = "ifany"))
```

## Missing values

```{r missing_values}
sapply(data, class)
table(is.na(data))
head(data)

colMeans(is.na(data))

# Replace -999 values with NA for all of our covariates.
data[, task$covariates] <- lapply(data[, task$covariates], function(col) {
  col[col == -999] <- NA
  col})
table(is.na(data))
colMeans(is.na(data[, task$covariates]))
head(data)
class(data)
sapply(data, class)

# need to check the proportion of missing for each column

 prop_miss<- apply(data, 2, function(col)sum(is.na(col))/length(col))
 factor(prop_miss)
# der_mass_mmc 0.152456
 # pri_jet_leading_eta pri_jet_leading_eta pri_jet_leading_phi  0.399652
 # der_lep_eta_centrality  der_deltaeta_jet_jet  der_mass_jet_jet  der_prodeta_jet_jet 0.709828
  # pri_jet_subleading_eta  pri_jet_subleading_pt pri_jet_subleading_pt  0.709828
 # my guess is the variables in the 2 rows above are highly corelated, they have same percentage of missing.
# corrrelation matrix
 str(task$covariates)
# correlation matrix

 install.packages("psych")
library(psych)
 require(psych)

# corelation matrix
 corel <- cor ( data [c(
      "der_mass_mmc", "der_mass_transverse_met_lep",
     "der_mass_vis", "der_pt_h",  "der_deltaeta_jet_jet", "der_mass_jet_jet",
"der_prodeta_jet_jet", "der_deltar_tau_lep", "der_pt_tot","der_sum_pt",
"der_pt_ratio_lep_tau", "der_met_phi_centrality", "der_lep_eta_centrality","pri_tau_pt",
"pri_tau_eta", "pri_tau_phi","pri_lep_pt", "pri_lep_eta","pri_lep_phi", "pri_met",

"pri_met_phi", "pri_met_sumet", "pri_jet_num", "pri_jet_leading_pt",
"pri_jet_leading_eta", "pri_jet_leading_phi", "pri_jet_subleading_pt",

  "pri_jet_subleading_eta", "pri_jet_subleading_phi", "pri_jet_all_pt" )],  use = "complete.obs")

 round(corel, 2)

 save(data,file="dataMD.Rda")
 save(corel, file="corel.rda")
 save(task,"task.rda")
 write.table(corel, file = "corel.csv", sep = ",")

 #Then load it with:
   load("data.Rda")

 # removing "pri_jet_all_pt"
 corel1 <- cor ( data [c(
   "der_mass_mmc", "der_mass_transverse_met_lep",
   "der_mass_vis", "der_pt_h",  "der_deltaeta_jet_jet", "der_mass_jet_jet",
   "der_prodeta_jet_jet", "der_deltar_tau_lep", "der_pt_tot","der_sum_pt",
   "der_pt_ratio_lep_tau", "der_met_phi_centrality", "der_lep_eta_centrality","pri_tau_pt",
   "pri_tau_eta", "pri_tau_phi","pri_lep_pt", "pri_lep_eta","pri_lep_phi", "pri_met",

   "pri_met_phi", "pri_met_sumet", "pri_jet_num", "pri_jet_leading_pt",
   "pri_jet_leading_eta", "pri_jet_leading_phi", "pri_jet_subleading_pt",

   "pri_jet_subleading_eta", "pri_jet_subleading_phi")],  use = "complete.obs")

 round(corel1, 2)
 save(corel1, file="corel1.rda")
 write.table(corel1, file = "corel1.csv", sep = ",")


 # removing "pri_jet_leading_pt pri_jet_all_pt"
 corel2 <- cor ( data [c(
   "der_mass_mmc", "der_mass_transverse_met_lep",
   "der_mass_vis", "der_pt_h",  "der_deltaeta_jet_jet", "der_mass_jet_jet",
   "der_prodeta_jet_jet", "der_deltar_tau_lep", "der_pt_tot","der_sum_pt",
   "der_pt_ratio_lep_tau", "der_met_phi_centrality", "der_lep_eta_centrality","pri_tau_pt",
   "pri_tau_eta", "pri_tau_phi","pri_lep_pt", "pri_lep_eta","pri_lep_phi", "pri_met",

   "pri_met_phi", "pri_met_sumet", "pri_jet_num",
   "pri_jet_leading_eta", "pri_jet_leading_phi", "pri_jet_subleading_pt",

   "pri_jet_subleading_eta", "pri_jet_subleading_phi")],  use = "complete.obs")

 round(corel2, 2)
 save(corel2, file="corel2.rda")
 write.table(corel2, file = "corel2.csv", sep = ",")

# very high corelated variables
 library(psych)
 pairs.panels(data[c(  "der_deltaeta_jet_jet","der_prodeta_jet_jet",
                       "pri_met_sumet", "der_sum_pt", "der_mass_mmc",
                "der_mass_vis", "der_prodeta_jet_jet", "pri_met_sumet" )])


# add log "pri_met_sumet", "der_sum_pt", "der_mass_mmc", "der_mass_vis" shape are very skewed
 data$log_pri_met_sumet<- log10( data$pri_met_sumet)
 data$log_der_sum_pt<- log10( data$der_sum_pt)

 data$der_mass_mmc<- log10( data$der_mass_mmc)

 data$log_pri_met_sumet<- log10( data$pri_met_sumet)
# coming back to this later,section above

 d <- density(data$pri_met_sumet)
 plot(d, main="Kernel Density of Miles Per Gallon")
 polygon(d, col="red", border="blue")

 library(ggplot2)
 library(tidyr)
 getwd()
# removing id,
 newdata <- data[c(-1,-32,-34,-35)]
 newdata$outcome<- ifelse(newdata$label=="s",1,0)  # look at newdata first make sure variable outcome is created correctly
 newdata<- newdata[c(-30)]
 save(newdata, file="newdata.rda")
 load("newdata.Rda")
 newdata1<-newdata # will use newdata1 to add new variable

 library(dlookr)
 library(dplyr)
 diagnose(newdata)
 newdata %>%
diagnose() %>%
select(-unique_count, -unique_rate) %>%
   filter(missing_count > 0) %>%
   arrange(desc(missing_count))

 #missing value proportion:
 #install.packages("DataExplorer")
 library(DataExplorer)
 introduce(newdata)
 plot_missing(newdata)
#creating indicator for missingess for highly missing data

 newdata1$ind_der_deltaeta_jet_jet <- ifelse(!is.na(newdata1$der_deltaeta_jet_jet),1,0)
 newdata1$ind_der_mass_jet_jet <- ifelse(!is.na(newdata1$der_mass_jet_jet),1,0)
 newdata1$ind_der_prodeta_jet_jet<- ifelse(!is.na(newdata1$der_prodeta_jet_jet),1,0)
 newdata1$ind_der_lep_eta_centrality<- ifelse(!is.na(newdata1$der_lep_eta_centrality),1,0)
 newdata1$ind_pri_jet_subleading_pt<- ifelse(!is.na(newdata1$pri_jet_subleading_pt),1,0)
 newdata1$ind_pri_jet_subleading_eta<- ifelse(!is.na(newdata1$pri_jet_subleading_eta),1,0)
 newdata1$ind_pri_jet_subleading_phi<- ifelse(!is.na(newdata1$pri_jet_subleading_phi),1,0)

 # Keep only numeric columns
 newdata %>%
   gather() %>%                             # Convert to key-value pairs
   ggplot(aes(value)) +                     # Plot the values
   facet_wrap(~ key, scales = "free")    # In separate panels
   geom_density()

#we start by “melting” our data from a wide format into a long format.
    require(reshape2)
    melt.boston <- melt(newdata)
    head(melt.boston)

    #Small Multiple Chart( density plot)
 ggplot(data = melt.boston, aes(x = value, )) +   stat_density() +
      facet_wrap(~variable, scales = "free")

    # boxplot

 boxplot(newdata) +facet_wrap(~variable, scales = "free")





#outliers
    #install.packages("dlookr")
    library(dlookr)
    outliers<-diagnose_outlier(newdata)
    write.table(outliers, file = "outliers.csv", sep = ",")
    # diagosed outliers and filter out any with more than 3% outliers
    diagnose_outlier(newdata) %>%
      filter(outliers_ratio > 3) %>%
      mutate(rate = outliers_mean / with_mean) %>%
      arrange(desc(rate)) %>%
      select(-outliers_cnt)

    #visualize anomaly values of all numeric variables with an outlier ratio of 0.5% or more.:

    library( dplyr )
    newdata %>%
      plot_outlier(diagnose_outlier(newdata) %>%
                     filter(outliers_ratio >= 0.5) %>%
                     select(variables) %>%
                     unlist())
#imputing variable with a lot of outliers

    newdata1$imputed_der_mass_vis<- imputate_outlier(newdata,der_mass_vis, method = "capping")
    summary( newdata1$imputed_der_mass_vis)
    plot( newdata1$imputed_der_mass_vis)

    newdata1$imputed_pri_tau_pt<- imputate_outlier(newdata,pri_tau_pt, method = "capping")
    summary( newdata1$imputed_pri_tau_pt)
    plot( newdata1$imputed_pri_tau_pt)

    newdata1$imputed_der_pt_h<- imputate_outlier(newdata,der_pt_h, method = "capping")
    summary( newdata1$imputed_der_pt_h)
    plot( newdata1$imputed_der_pt_h)

    newdata1$imputed_der_mass_mmc<- imputate_outlier(newdata,der_mass_mmc, method = "capping")
    summary( newdata1$imputed_der_mass_mmc)
    plot( newdata1$imputed_der_mass_mmc)

    newdata1$imputed_pri_met <- imputate_outlier(newdata,pri_met , method = "capping")
    summary( newdata1$imputed_pri_met )
    plot( newdata1$imputed_pri_met )

    newdata1$imputed_pri_lep_pt<- imputate_outlier(newdata,pri_lep_pt, method = "capping")
    summary( newdata1$imputed_pri_lep_pt)
    plot( newdata1$imputed_pri_lep_pt)

#imputed missing
    der_mass_vis
    pri_tau_pt
    der_pt_h
    der_mass_mmc
    pri_met
    pri_lep_pt
#highly missing

    der_deltaeta_jet_jet
   der_mass_jet_jet
    der_prodeta_jet_jet
    der_lep_eta_centrality
    pri_jet_subleading_pt
  pri_jet_subleading_eta
    pri_jet_subleading_phi

    #highly corelated
    pri_jet_leading_pt
    pri_jet_all_pt

    drops<- c("pri_jet_leading_pt","pri_jet_all_pt",  "der_deltaeta_jet_jet",  "der_mass_jet_jet", "der_prodeta_jet_jet",
                       "der_lep_eta_centrality", "pri_jet_subleading_pt", "pri_jet_subleading_eta","pri_jet_subleading_phi",
              "der_mass_vis",
              "pri_tau_pt",
              "der_pt_h",
              "der_mass_mmc",
              "pri_met",
              "pri_lep_pt")

    completedataMD1<- newdata1[ , !(names(newdata1) %in% drops)]
completedataMD <-newdata1

#saving newdata:
    save(newdata1, file="newdata1.rda")
    load("newdata1.Rda")
    completedataMD <-newdata1

    save(completedataMD, file="completedataMD.rda")
    save(completedataMD1, file="completedata1MD.rda")

    #cleaning test data
getwd()
setwd("C:/Users/makdi/Desktop/kpdor-dsc/kp-dsc-2018/testdata")






getwd()

data <- read.csv("test.csv")

dim(data)

# examine the structure of the data
names(data) = tolower(names(data))
str(data)

# Review classes in our dataset.
table(sapply(data, class))
sapply(data, class)

# Review the number of unique values in each column.
sapply(data, function(col) { length(unique(col)) })


#       0        1        2        3
#0.399652 0.310176 0.201516 0.088656
```



## Missing values

```{r missing_values}
sapply(data, class)
table(is.na(data))
head(data)

colMeans(is.na(data))

# Replace -999 values with NA for all of our covariates.

data[data==-999] <- NA



# need to check the proportion of missing for each column


# correlation matrix

install.packages("psych")
library(psych)
require(psych)

# corelation matrix
corel <- cor ( data [c(
  "der_mass_mmc", "der_mass_transverse_met_lep",
  "der_mass_vis", "der_pt_h",  "der_deltaeta_jet_jet", "der_mass_jet_jet",
  "der_prodeta_jet_jet", "der_deltar_tau_lep", "der_pt_tot","der_sum_pt",
  "der_pt_ratio_lep_tau", "der_met_phi_centrality", "der_lep_eta_centrality","pri_tau_pt",
  "pri_tau_eta", "pri_tau_phi","pri_lep_pt", "pri_lep_eta","pri_lep_phi", "pri_met",

  "pri_met_phi", "pri_met_sumet", "pri_jet_num", "pri_jet_leading_pt",
  "pri_jet_leading_eta", "pri_jet_leading_phi", "pri_jet_subleading_pt",

  "pri_jet_subleading_eta", "pri_jet_subleading_phi", "pri_jet_all_pt" )],  use = "complete.obs")

round(corel, 2)

save(data,file="dataMDtest.Rda")
save(corel, file="corel.rda")

write.table(corel, file = "corel.csv", sep = ",")

#Then load it with:
load("data.Rda")

# removing "pri_jet_all_pt"
corel1 <- cor ( data [c(
  "der_mass_mmc", "der_mass_transverse_met_lep",
  "der_mass_vis", "der_pt_h",  "der_deltaeta_jet_jet", "der_mass_jet_jet",
  "der_prodeta_jet_jet", "der_deltar_tau_lep", "der_pt_tot","der_sum_pt",
  "der_pt_ratio_lep_tau", "der_met_phi_centrality", "der_lep_eta_centrality","pri_tau_pt",
  "pri_tau_eta", "pri_tau_phi","pri_lep_pt", "pri_lep_eta","pri_lep_phi", "pri_met",

  "pri_met_phi", "pri_met_sumet", "pri_jet_num", "pri_jet_leading_pt",
  "pri_jet_leading_eta", "pri_jet_leading_phi", "pri_jet_subleading_pt",

  "pri_jet_subleading_eta", "pri_jet_subleading_phi")],  use = "complete.obs")

round(corel1, 2)
save(corel1, file="corel1.rda")
write.table(corel1, file = "corel1.csv", sep = ",")


# removing "pri_jet_leading_pt pri_jet_all_pt"
corel2 <- cor ( data [c(
  "der_mass_mmc", "der_mass_transverse_met_lep",
  "der_mass_vis", "der_pt_h",  "der_deltaeta_jet_jet", "der_mass_jet_jet",
  "der_prodeta_jet_jet", "der_deltar_tau_lep", "der_pt_tot","der_sum_pt",
  "der_pt_ratio_lep_tau", "der_met_phi_centrality", "der_lep_eta_centrality","pri_tau_pt",
  "pri_tau_eta", "pri_tau_phi","pri_lep_pt", "pri_lep_eta","pri_lep_phi", "pri_met",

  "pri_met_phi", "pri_met_sumet", "pri_jet_num",
  "pri_jet_leading_eta", "pri_jet_leading_phi", "pri_jet_subleading_pt",

  "pri_jet_subleading_eta", "pri_jet_subleading_phi")],  use = "complete.obs")

round(corel2, 2)
save(corel2, file="corel2.rda")
write.table(corel2, file = "corel2.csv", sep = ",")

# very high corelated variables
library(psych)
pairs.panels(data[c(  "der_deltaeta_jet_jet","der_prodeta_jet_jet",
                      "pri_met_sumet", "der_sum_pt", "der_mass_mmc",
                      "der_mass_vis", "der_prodeta_jet_jet", "pri_met_sumet" )])


# add log "pri_met_sumet", "der_sum_pt", "der_mass_mmc", "der_mass_vis" shape are very skewed
data$log_pri_met_sumet<- log10( data$pri_met_sumet)
data$log_der_sum_pt<- log10( data$der_sum_pt)

data$der_mass_mmc<- log10( data$der_mass_mmc)

data$log_pri_met_sumet<- log10( data$pri_met_sumet)
# coming back to this later,section above
# cannt ru this on my computer, will plit the data
d <- density(data$pri_met_sumet)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")

library(ggplot2)
library(tidyr)
getwd()
# removing id,
newdata <- data[c(-32,-34,-35)]
# no outcome newdata$outcome<- ifelse(newdata$label=="s",1,0)  # look at newdata first make sure variable outcome is created correctly

save(newdata, file="newdata.rda")
load("newdata.Rda")
newdata1<-newdata # will use newdata1 to add new variable

library(dlookr)
library(dplyr)
diagnose(newdata)
newdata %>%
  diagnose() %>%
  select(-unique_count, -unique_rate) %>%
  filter(missing_count > 0) %>%
  arrange(desc(missing_count))

#missing value proportion:
#install.packages("DataExplorer")
library(DataExplorer)
introduce(newdata)
plot_missing(newdata)
#creating indicator for missingess for highly missing data

newdata1$ind_der_deltaeta_jet_jet <- ifelse(!is.na(newdata1$der_deltaeta_jet_jet),1,0)
newdata1$ind_der_mass_jet_jet <- ifelse(!is.na(newdata1$der_mass_jet_jet),1,0)
newdata1$ind_der_prodeta_jet_jet<- ifelse(!is.na(newdata1$der_prodeta_jet_jet),1,0)
newdata1$ind_der_lep_eta_centrality<- ifelse(!is.na(newdata1$der_lep_eta_centrality),1,0)
newdata1$ind_pri_jet_subleading_pt<- ifelse(!is.na(newdata1$pri_jet_subleading_pt),1,0)
newdata1$ind_pri_jet_subleading_eta<- ifelse(!is.na(newdata1$pri_jet_subleading_eta),1,0)
newdata1$ind_pri_jet_subleading_phi<- ifelse(!is.na(newdata1$pri_jet_subleading_phi),1,0)

# Keep only numeric columns
newdata %>%
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free")    # In separate panels
geom_density()

#we start by “melting” our data from a wide format into a long format.
require(reshape2)
melt.boston <- melt(newdata)
head(melt.boston)

#Small Multiple Chart( density plot)
ggplot(data = melt.boston, aes(x = value, )) +   stat_density() +
  facet_wrap(~variable, scales = "free")

# boxplot

boxplot(newdata) +facet_wrap(~variable, scales = "free")





#outliers
#install.packages("dlookr")
library(dlookr)
outliers<-diagnose_outlier(newdata)
write.table(outliers, file = "outliers.csv", sep = ",")
# diagosed outliers and filter out any with more than 3% outliers
diagnose_outlier(newdata) %>%
  filter(outliers_ratio > 3) %>%
  mutate(rate = outliers_mean / with_mean) %>%
  arrange(desc(rate)) %>%
  select(-outliers_cnt)

#visualize anomaly values of all numeric variables with an outlier ratio of 0.5% or more.:

library( dplyr )
newdata %>%
  plot_outlier(diagnose_outlier(newdata) %>%
                 filter(outliers_ratio >= 0.5) %>%
                 select(variables) %>%
                 unlist())
#imputing variable with a lot of outliers

newdata1$imputed_der_mass_vis<- imputate_outlier(newdata,der_mass_vis, method = "capping")
summary( newdata1$imputed_der_mass_vis)
plot( newdata1$imputed_der_mass_vis)

newdata1$imputed_pri_tau_pt<- imputate_outlier(newdata,pri_tau_pt, method = "capping")
summary( newdata1$imputed_pri_tau_pt)
plot( newdata1$imputed_pri_tau_pt)

newdata1$imputed_der_pt_h<- imputate_outlier(newdata,der_pt_h, method = "capping")
summary( newdata1$imputed_der_pt_h)
plot( newdata1$imputed_der_pt_h)

newdata1$imputed_der_mass_mmc<- imputate_outlier(newdata,der_mass_mmc, method = "capping")
summary( newdata1$imputed_der_mass_mmc)
plot( newdata1$imputed_der_mass_mmc)

newdata1$imputed_pri_met <- imputate_outlier(newdata,pri_met , method = "capping")
summary( newdata1$imputed_pri_met )
plot( newdata1$imputed_pri_met )

newdata1$imputed_pri_lep_pt<- imputate_outlier(newdata,pri_lep_pt, method = "capping")
summary( newdata1$imputed_pri_lep_pt)
plot( newdata1$imputed_pri_lep_pt)

#imputed missing
der_mass_vis
pri_tau_pt
der_pt_h
der_mass_mmc
pri_met
pri_lep_pt
#highly missing

der_deltaeta_jet_jet
der_mass_jet_jet
der_prodeta_jet_jet
der_lep_eta_centrality
pri_jet_subleading_pt
pri_jet_subleading_eta
pri_jet_subleading_phi

#highly corelated
pri_jet_leading_pt
pri_jet_all_pt

drops<- c("pri_jet_leading_pt","pri_jet_all_pt",  "der_deltaeta_jet_jet",  "der_mass_jet_jet", "der_prodeta_jet_jet",
          "der_lep_eta_centrality", "pri_jet_subleading_pt", "pri_jet_subleading_eta","pri_jet_subleading_phi",
          "der_mass_vis",
          "pri_tau_pt",
          "der_pt_h",
          "der_mass_mmc",
          "pri_met",
          "pri_lep_pt")

completedataMD1<- newdata1[ , !(names(newdata1) %in% drops)]
completedataMD <-newdata1

#saving newdata:
save(newdata1, file="newdata1TEST.rda")
load("newdata1TEST.Rda")
completedataMD <-newdata1

save(completedataMD, file="completedataMD.rda")
save(completedataMD1, file="completedata1MD.rda")











































    newdata %>%
    diagnose_report(output_format = "html")
    transformation_report(target = outcome, output_format = "html",
                          output_file = "transformation.html")







 # Create a shareable link to your chart
 # Set up API credentials: https://plot.ly/r/getting-started
 chart_link = api_create(p, filename="box-multiple")
 chart_link
# Add missingness indicators and impute missing values to median.
impute <- ck37r::impute_missing_values(data, type = "standard", verbose = TRUE)

task$data <- impute$data

# Clear out this $data element so that we don't have two copies of the data.
impute$data = NULL

# Add missingness indicators to task$covariates
(task$covariates <- c(task$covariates, impute$indicators_added))

# Confirm that all of our covariates exist in the data.
all(task$covariates %in% names(task$data))

```

## Convert factors to indicators

```{r}

sapply(task$data, class)

# This will only apply to $pri_jet_num
# We need to do the same step on the testing data.
factors = ck37r::factors_to_indicators(data, predictors = task$covariates, verbose = TRUE)
names(factors)

# This is dropping the missingness indicators for some reason - appears to be
# a bug in factors_to_indicators - need to investigate.
names(factors$data)
factors$factor_names

# TODO: this as.vector shouldn't be needed but $factor_names is a column matrix
# (erroneously).
task$data = cbind(task$data, factors$data[, as.vector(factors$factor_names)])
names(task$data)

# Update our covariate list to include the 3 new indicator columns.
(task$covariates = c(task$covariates, as.vector(factors$factor_names)))

# Clear out this extra copy of the dataframe to save memory & storage size.
factors$data = NULL

# Remove $pri_jet_num now that it exists as indicators.
task$data$pri_jet_num = NULL
(task$covariates = setdiff(task$covariates, "pri_jet_num"))
```

## Save results

```{r save_results}

# Confirm that all of our covariates exist in our data.
stopifnot(all(task$covariates %in% names(task$data)))

# Save imputation and factor objects for future reference and use on test dataset.
save(task, impute, factors,
     file = "data/clean.RData")
```
