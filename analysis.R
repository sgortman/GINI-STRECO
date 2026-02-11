#####
# This code based on that producing the figures and analyses for PNAS https://doi.org/10.1073/pnas.2400691122
# revised for paper presented by Kohler at UNU-WIDER (Helsinki) in Fall '24
# and here used to produce figures and analyses for STRECO paper with Green and Ortman
# Figure 3 code originally by Ortman, modified slightly here; other code by Kohler

# It has three sections:
# Part 1: Data ingestion, imputations, manipulations
# Part 2: Produces Figures 1 and 3
# Part 3: Produces Regression Analyses and Figures 2 and 4
#
#
##### Part 1: Data ingestion and manipulations

if (!requireNamespace("renv", quietly = TRUE)) {
  stop("renv is not installed. Install it with install.packages('renv') then run renv::restore().")
}

library(tidyverse)
library(simputation)
library(visdat)
library(ggrepel)
library(ggpubr)
library(cowplot)
library(DescTools)
library(RColorBrewer)
library(moments)
options(brms.backend = "cmdstanr")

if (!requireNamespace("cmdstanr", quietly = TRUE)) {
  stop("cmdstanr is required. Install it, then run cmdstanr::install_cmdstan().")
}

v <- cmdstanr::cmdstan_version(error_on_NA = FALSE)
if (is.na(v)) {
  stop("CmdStan is not installed/configured. Run cmdstanr::install_cmdstan() and try again.")
}

library(brms)
library(posterior)

library(bayestestR)
library(mgcv)    # load mgcv first
library(gratia)  # then gratia
library(patchwork)
library(future)
library(openxlsx)
library(here)
library(ggforce)
library(grid)

# Project folders (create if missing)
dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)

# test for resolution of Excel issues
# file.info(here::here("data","gini_database_all_records_20251011r.xlsx"))$size
# openxlsx::read.xlsx(here::here("data","gini_database_all_records_20251011r.xlsx"))


# This paper uses 3 data files, SiteGiniLevel.csv (everything except Fig 3)
# and gini_database_all_records_20251011r.xlsx & tlkp Phase.csv (for Fig. 3)
# All are in github public repository https://github.com/sgortman/GINI-STRECO/data

SiteGiniLevel <- read.csv(here("data", "SiteGiniLevel.csv"), header=TRUE, fill=TRUE, strip.white=TRUE)
cfas <- openxlsx::read.xlsx(here("data", "gini_database_all_records_20251011r.xlsx"))
order <- read.csv(here("data", "tlkp Phase.csv"), header=TRUE, fill=TRUE, strip.white=TRUE)

# optional data exploration
# How many houses in dataset?
total_households <- sum(SiteGiniLevel$CountHH)
print(total_households)

# Distribution of sites by Bigregion, Region, and Subregion
observation_counts <- SiteGiniLevel %>%
  count(Bigregion)
print(observation_counts)
observation_counts <- SiteGiniLevel %>%
  count(Region)
print(observation_counts)
observation_counts <- SiteGiniLevel %>%
  count(Subregion)
print(observation_counts)

# stomp some missing values
SiteGiniLevel <- SiteGiniLevel %>% 
  mutate(
    Plant.cultivation...earliest = if_else(Subregion == 'Egypt' & is.na(Plant.cultivation...earliest), -6200, Plant.cultivation...earliest),
    Plant.cultivation...common = if_else(Subregion == 'Egypt' & is.na(Plant.cultivation...common), -6000, Plant.cultivation...common),
    Bronze...earliest = if_else(Subregion == 'Egypt' & is.na(Bronze...earliest), -3150, Bronze...earliest),
    Bronze...common = if_else(Subregion == 'Egypt' & is.na(Bronze...common), -3100, Bronze...common),
    Iron..smelted....earliest = if_else(Subregion == 'Egypt' & is.na(Iron..smelted....earliest), -1100, Iron..smelted....earliest),
    Iron..smelted....common = if_else(Subregion == 'Egypt' & is.na(Iron..smelted....common), -700, Iron..smelted....common),
    Animal.management...common = if_else(Subregion == 'Egypt' & is.na(Animal.management...common), -8000, Animal.management...common),
    Copper..smelted....earliest = if_else(Region == 'Egypt' & is.na(Copper..smelted....earliest), -4400, Copper..smelted....earliest),
    Copper..smelted....common = if_else(Region == 'Egypt' & is.na(Copper..smelted....common), -4200, Copper..smelted....common)
  )
SiteGiniLevel <- SiteGiniLevel %>% 
  mutate(
    Plant.cultivation...earliest = if_else(Subregion == 'Korea' & is.na(Plant.cultivation...earliest), -1600, Plant.cultivation...earliest),
    Plant.cultivation...common = if_else(Subregion == 'Korea' & is.na(Plant.cultivation...common), -1500, Plant.cultivation...common),
    Bronze...earliest = if_else(Subregion == 'Korea' & is.na(Bronze...earliest), -800, Bronze...earliest),
    Iron..smelted....earliest = if_else(Subregion == 'Korea' & is.na(Iron..smelted....earliest), -300, Iron..smelted....earliest),
    Iron..smelted....common = if_else(Subregion == 'Korea' & is.na(Iron..smelted....common), -100, Iron..smelted....common),
    Animal.management...common = if_else(Subregion == 'Korea' & is.na(Animal.management...common), 100, Animal.management...common)
    
  )
SiteGiniLevel <- SiteGiniLevel %>% 
  mutate(
    Bronze...earliest = if_else(Region == 'Great Plains' & is.na(Bronze...earliest), 1600, Bronze...earliest),
    Iron..smelted....earliest = if_else(Region == 'Great Plains' & is.na(Iron..smelted....earliest), 1600, Iron..smelted....earliest),
    Iron..smelted....common = if_else(Region == 'Great Plains' & is.na(Iron..smelted....common), 1600, Iron..smelted....common),
    Animal.management...common = if_else(Region == 'Great Plains' & is.na(Animal.management...common), 1600, Animal.management...common),
    Portage...common = if_else(Region == 'Great Plains' & is.na(Portage...common), 1600, Portage...common),
    Riding...common = if_else(Region == 'Great Plains' & is.na(Riding...common), 1600, Riding...common),
    Enslavement...earliest = if_else(Region == 'Great Plains' & is.na(Enslavement...earliest), 1600, Enslavement...earliest),
    Copper..smelted....earliest = if_else(Region == 'Great Plains' & is.na(Copper..smelted....earliest), 1600, Copper..smelted....earliest)
  )
SiteGiniLevel <- SiteGiniLevel %>% 
  mutate(
    Bronze...earliest = if_else(Subregion == 'Midwest' & is.na(Bronze...earliest), 1600, Bronze...earliest),
    Iron..smelted....earliest = if_else(Subregion == 'Midwest' & is.na(Iron..smelted....earliest), 1600, Iron..smelted....earliest),
    Iron..smelted....common = if_else(Subregion == 'Midwest' & is.na(Iron..smelted....common), 1600, Iron..smelted....common),
    Animal.management...common = if_else(Subregion == 'Midwest' & is.na(Animal.management...common), 1600, Animal.management...common),
    Portage...common = if_else(Subregion == 'Midwest' & is.na(Portage...common), 1600, Portage...common),
    Riding...common = if_else(Subregion == 'Midwest' & is.na(Riding...common), 1600, Riding...common),
    Enslavement...earliest = if_else(Subregion == 'Midwest' & is.na(Enslavement...earliest), 1600, Enslavement...earliest),
    Copper..smelted....earliest = if_else(Subregion == 'Midwest' & is.na(Copper..smelted....earliest), 1600, Copper..smelted....earliest)
  )
SiteGiniLevel <- SiteGiniLevel %>% 
  mutate(
    Plant.cultivation...earliest = if_else(HousePhase == 'Fremont' & is.na(Plant.cultivation...earliest), 200, Plant.cultivation...earliest),
    Plant.cultivation...common = if_else(HousePhase == 'Fremont' & is.na(Plant.cultivation...common), 800, Plant.cultivation...common),
    Animal.management...common = if_else(HousePhase == 'Fremont' & is.na(Animal.management...common), 1600, Animal.management...common),
    Bronze...earliest = if_else(HousePhase == 'Fremont' & is.na(Bronze...earliest), 1600, Bronze...earliest),
    Iron..smelted....earliest = if_else(HousePhase == 'Fremont' & is.na(Iron..smelted....earliest), 1600, Iron..smelted....earliest),
    Iron..smelted....common = if_else(HousePhase == 'Fremont' & is.na(Iron..smelted....common), 1600, Iron..smelted....common),
    Animal.management...common = if_else(HousePhase == 'Fremont' & is.na(Animal.management...common), 1600, Animal.management...common),
    Portage...common = if_else(HousePhase == 'Fremont' & is.na(Portage...common), 1600, Portage...common),
    Riding...common = if_else(HousePhase == 'Fremont' & is.na(Riding...common), 1600, Riding...common),
    Enslavement...earliest = if_else(HousePhase == 'Fremont' & is.na(Enslavement...earliest), 1600, Enslavement...earliest),
    Copper..smelted....earliest = if_else(HousePhase == 'Fremont' & is.na(Copper..smelted....earliest), 1600, Copper..smelted....earliest)
  )
SiteGiniLevel <- SiteGiniLevel %>% 
  mutate(
    Animal.management...common = if_else(Region == 'Southeast NA' & is.na(Animal.management...common), 1600, Animal.management...common),
    Bronze...earliest = if_else(Region == 'Southeast NA' & is.na(Bronze...earliest), 1600, Bronze...earliest),
    Iron..smelted....earliest = if_else(Region == 'Southeast NA' & is.na(Iron..smelted....earliest), 1600, Iron..smelted....earliest),
    Iron..smelted....common = if_else(Region == 'Southeast NA' & is.na(Iron..smelted....common), 1600, Iron..smelted....common),
    Animal.management...common = if_else(Region == 'Southeast NA' & is.na(Animal.management...common), 1600, Animal.management...common),
    Portage...common = if_else(Region == 'Southeast NA' & is.na(Portage...common), 1600, Portage...common),
    Riding...common = if_else(Region == 'Southeast NA' & is.na(Riding...common), 1600, Riding...common),
    Enslavement...earliest = if_else(Region == 'Southeast NA' & is.na(Enslavement...earliest), 1600, Enslavement...earliest),
    Copper..smelted....earliest = if_else(Region == 'Southeast NA' & is.na(Copper..smelted....earliest), 1600, Copper..smelted....earliest)
  )
SiteGiniLevel <- SiteGiniLevel %>% 
  mutate(
    Plant.cultivation...earliest = if_else(Subregion == 'Turkmenistan' & is.na(Plant.cultivation...earliest), -4500, Plant.cultivation...earliest),
    Plant.cultivation...common = if_else(Subregion == 'Turkmenistan' & is.na(Plant.cultivation...common), -4000, Plant.cultivation...common),
    Animal.management...common = if_else(Subregion == 'Turkmenistan' & is.na(Animal.management...common), -2400, Animal.management...common),
    Bronze...earliest = if_else(Subregion == 'Turkmenistan' & is.na(Bronze...earliest), -2500, Bronze...earliest),
    Iron..smelted....earliest = if_else(Subregion == 'Turkmenistan' & is.na(Iron..smelted....earliest), -1500, Iron..smelted....earliest),
    Iron..smelted....common = if_else(Subregion == 'Turkmenistan' & is.na(Iron..smelted....common), -1300, Iron..smelted....common),
    Portage...common = if_else(is.na(Portage...common), -2300, Portage...common),
    Riding...common = if_else(Subregion == "Turkmenistan" & is.na(Riding...common), -2300, Riding...common),
    Enslavement...earliest = if_else(Subregion == 'Turkmenistan' & is.na(Enslavement...earliest), -400, Enslavement...earliest),
    Copper..smelted....earliest = if_else(Subregion == 'Turkmenistan' & is.na(Copper..smelted....earliest), -4500, Copper..smelted....earliest)
  )
# get ready for Reg: calculate Delta vars
SiteGiniLevel$DeltaCult <- SiteGiniLevel$Date - SiteGiniLevel$`Plant.cultivation...common`
SiteGiniLevel$DeltaAnMan <- SiteGiniLevel$Date - SiteGiniLevel$`Animal.management...common`
SiteGiniLevel$DeltaPort <- SiteGiniLevel$Date - SiteGiniLevel$`Portage...common`
SiteGiniLevel$DeltaRiding <- SiteGiniLevel$Date - SiteGiniLevel$`Riding...common`
SiteGiniLevel$DeltaTraction <- SiteGiniLevel$Date - SiteGiniLevel$`Traction...common`
SiteGiniLevel$DeltaEnslavement <- SiteGiniLevel$Date - SiteGiniLevel$`Enslavement...common`
SiteGiniLevel$DeltaCopperSm <- SiteGiniLevel$Date - SiteGiniLevel$`Copper..smelted....common`
SiteGiniLevel$DeltaBronze <- SiteGiniLevel$Date - SiteGiniLevel$`Bronze...common`
SiteGiniLevel$DeltaWriting <- SiteGiniLevel$Date - SiteGiniLevel$writing
SiteGiniLevel$DeltaMassacre <- SiteGiniLevel$Date - SiteGiniLevel$`Earliest.massacre`
SiteGiniLevel$DeltaWandM <- SiteGiniLevel$Date - SiteGiniLevel$`weights.and.measures`
SiteGiniLevel$DeltaMarketPl <- SiteGiniLevel$Date - SiteGiniLevel$`earliest.marketplaces`
SiteGiniLevel$DeltaCurrency <- SiteGiniLevel$Date - SiteGiniLevel$Currency
SiteGiniLevel$DeltaOstBur <- SiteGiniLevel$Date - SiteGiniLevel$'earliest.ostentatious.individual.burials'
SiteGiniLevel$DeltaPubBldgs <- SiteGiniLevel$Date - SiteGiniLevel$'intra.site.public.buildings'
SiteGiniLevel$DeltaSpecProd <- SiteGiniLevel$Date - SiteGiniLevel$'specialized.production.of.goods'
SiteGiniLevel$DeltaIronSm <- SiteGiniLevel$Date - SiteGiniLevel$'Iron..smelted....common'
SiteGiniLevel$DeltaCompBow <- SiteGiniLevel$Date - SiteGiniLevel$'Complex.bow...earliest'
SiteGiniLevel$DeltaCult_E <- SiteGiniLevel$Date - SiteGiniLevel$'Plant.cultivation...earliest'

# Convert 'Landlabor' to a factor 
if(is.character(SiteGiniLevel$Landlabor)) {
  SiteGiniLevel$Landlabor <- factor(SiteGiniLevel$Landlabor)
}

# flip Gov where 3 natively represents maximally collective
# High values for Gov_I (inverted) will now represent maximally autocratic/despotic power relations
SiteGiniLevel$Gov_I <- 3 - SiteGiniLevel$Gov
# get rid of original
SiteGiniLevel <- SiteGiniLevel %>% dplyr::select(-Gov)

# remove a few vars not needed for following
SiteGiniLevel <- SiteGiniLevel %>% 
  dplyr::select(-X, -SiteType, -SiteGroup, -PopMethod, -Popmethdesc, -FortificationComment, -Politytypdesc)


# Make Fig. S2, SA on log(PolityPop) before removing PolityPop
# make SA
SiteGiniLevel <- SiteGiniLevel %>%
  filter(!is.na(WhichLevel) & !is.na(NOfLevels))
SiteGiniLevel$SA <- SiteGiniLevel$WhichLevel + SiteGiniLevel$NOfLevels

## Check for missing and do imputations

vis_miss(SiteGiniLevel)
# 18.2% missing overall, remove worst vars

# Calculate the percentage of missing values for each variable
missing_percentage <- sapply(SiteGiniLevel, function(x) mean(is.na(x)))

# Identify variables with more than 25% missing values
vars_to_remove <- names(missing_percentage[missing_percentage > 0.25])

# Remove these variables from the dataset: not needed in the following plaots and analyses
SiteGiniLevel_clean <- SiteGiniLevel[, !(names(SiteGiniLevel) %in% vars_to_remove)]
# removes 24 vars

# Create the vis_miss plot
miss_plot <- vis_miss(SiteGiniLevel_clean)
# Add the theme_bw()
miss_plot <- miss_plot + theme_bw()
# Display the plot
print(miss_plot)
# now 2.4% missing values

# Impute missing values using CART for specified variables needed for regressions
# Create a list of predictor variables excluding 'Site' etc.
predictors <- setdiff(names(SiteGiniLevel_clean), c("Bigregion", "Site", "Region", "Subregion", "Subarea", "HousePhase", "DeltaCult", "Gov_I",
                                                    "DeltaAnMan", "DeltaPort", "DeltaRiding", "DeltaTraction", "DeltaCopperSm", 
                                                    "DeltaBronze", "DeltaIronSm", 
                                                    "DeltaEnslavement", "DeltaCult_E"))

# Create a formula string, one var at a time, not automated
formula_str <- paste("Gov_I ~", paste(predictors, collapse = " + "))
# Convert the string to a formula
formula <- as.formula(formula_str)
SiteGiniLevel_imputed <- impute_cart(SiteGiniLevel_clean, formula)

# on second time through impute DeltaCult (etc.)
formula_str <- paste("DeltaCult ~", paste(predictors, collapse = " + "))
# Convert the string to a formula
formula <- as.formula(formula_str)
# Perform the imputation, first time through use SiteGiniLevel_clean on rhs
SiteGiniLevel_imputed <- impute_cart(SiteGiniLevel_imputed, formula)

# (etc.)
formula_str <- paste("DeltaAnMan ~", paste(predictors, collapse = " + "))
# Convert the string to a formula
formula <- as.formula(formula_str)
# Perform the imputation, first time through use SiteGiniLevel_clean on rhs
SiteGiniLevel_imputed <- impute_cart(SiteGiniLevel_imputed, formula)

# (etc.)
formula_str <- paste("DeltaPort ~", paste(predictors, collapse = " + "))
# Convert the string to a formula
formula <- as.formula(formula_str)
# Perform the imputation, first time through use SiteGiniLevel_clean on rhs
SiteGiniLevel_imputed <- impute_cart(SiteGiniLevel_imputed, formula)

# (etc.)
formula_str <- paste("DeltaRiding ~", paste(predictors, collapse = " + "))
# Convert the string to a formula
formula <- as.formula(formula_str)
# Perform the imputation, first time through use SiteGiniLevel_clean on rhs
SiteGiniLevel_imputed <- impute_cart(SiteGiniLevel_imputed, formula)

# (etc.)
formula_str <- paste("DeltaTraction ~", paste(predictors, collapse = " + "))
# Convert the string to a formula
formula <- as.formula(formula_str)
# Perform the imputation, first time through use SiteGiniLevel_clean on rhs
SiteGiniLevel_imputed <- impute_cart(SiteGiniLevel_imputed, formula)

# (etc.)
formula_str <- paste("DeltaCopperSm ~", paste(predictors, collapse = " + "))
# Convert the string to a formula
formula <- as.formula(formula_str)
# Perform the imputation, first time through use SiteGiniLevel_clean on rhs
SiteGiniLevel_imputed <- impute_cart(SiteGiniLevel_imputed, formula)

# (etc.)
formula_str <- paste("DeltaBronze ~", paste(predictors, collapse = " + "))
# Convert the string to a formula
formula <- as.formula(formula_str)
# Perform the imputation, first time through use SiteGiniLevel_clean on rhs
SiteGiniLevel_imputed <- impute_cart(SiteGiniLevel_imputed, formula)

# (etc.)
formula_str <- paste("DeltaIronSm ~", paste(predictors, collapse = " + "))
# Convert the string to a formula
formula <- as.formula(formula_str)
# Perform the imputation, first time through use SiteGiniLevel_clean on rhs
SiteGiniLevel_imputed <- impute_cart(SiteGiniLevel_imputed, formula)

# (etc.)
formula_str <- paste("DeltaEnslavement ~", paste(predictors, collapse = " + "))
# Convert the string to a formula
formula <- as.formula(formula_str)
# Perform the imputation, first time through use SiteGiniLevel_clean on rhs
SiteGiniLevel_imputed <- impute_cart(SiteGiniLevel_imputed, formula)

# (etc.)
formula_str <- paste("DeltaCult_E ~", paste(predictors, collapse = " + "))
# Convert the string to a formula
formula <- as.formula(formula_str)
# Perform the imputation, first time through use SiteGiniLevel_clean on rhs
SiteGiniLevel_imputed <- impute_cart(SiteGiniLevel_imputed, formula)

# put SA in both SiteGiniLevel & SiteGiniLevel_imputed
SiteGiniLevel_imputed <- SiteGiniLevel_imputed %>%
  filter(!is.na(WhichLevel) & !is.na(NOfLevels))
SiteGiniLevel_imputed$SA <- SiteGiniLevel_imputed$WhichLevel + SiteGiniLevel_imputed$NOfLevels

SiteGiniLevel <- SiteGiniLevel %>%
  filter(!is.na(WhichLevel) & !is.na(NOfLevels))
SiteGiniLevel$SA <- SiteGiniLevel$WhichLevel + SiteGiniLevel$NOfLevels

# so that I can use SiteGiniLevel below, make that the imputed form
SiteGiniLevel <- SiteGiniLevel_imputed

##### End Part 1


##### Begin Part 2 STRECO Figure 1 upper and lower, Fig. 3

# --- build the combined LIH table cleanly ---
Basal_sites <- SiteGiniLevel %>% filter(WhichLevel == 1) %>% mutate(LIH = "Basal")

Apex_sites_region <- SiteGiniLevel %>%
  group_by(Region) %>%
  filter(WhichLevel == max(WhichLevel, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(LIH = "Apex")

Apex_sites_global <- SiteGiniLevel %>%
  filter(WhichLevel == NOfLevels) %>%
  mutate(LIH = "Apex")

Other_sites <- SiteGiniLevel %>% mutate(LIH = "Other sites")

Apex_sites_combined <- bind_rows(Apex_sites_region, Apex_sites_global) %>% distinct()

# (If you still need Ids) — fix the Id/id typo
Basal_sites$Id <- seq_len(nrow(Basal_sites))
Apex_sites_combined$Id <- seq_len(nrow(Apex_sites_combined)) + max(Basal_sites$Id, na.rm = TRUE)
Other_sites$Id <- seq_len(nrow(Other_sites)) + max(Apex_sites_combined$Id, na.rm = TRUE)

Combined_sites <- bind_rows(Basal_sites, Apex_sites_combined, Other_sites)

# De-dup on all but Id and LIH

# Columns to consider when de-duplicating (everything except Id and LIH)
cols <- setdiff(names(Combined_sites), c("Id", "LIH"))

Combined_sites_unique <- Combined_sites %>%
  dplyr::distinct(dplyr::across(dplyr::all_of(cols)), .keep_all = TRUE)


# Stable join key
SiteGiniLevel <- SiteGiniLevel %>%
  mutate(SiteDateWLevel = paste0(Site, Date, WhichLevel))

Combined_sites_unique <- Combined_sites_unique %>%
  mutate(SiteDateWLevel = paste0(Site, Date, WhichLevel)) %>%
  dplyr::select(SiteDateWLevel, LIH)

# Attach the LIH labels from Combined_sites_unique to the matching rows in SiteGiniLevel, 
# keeping all the original site data intact.

# search for dups:
SiteGiniLevel %>%
  count(SiteDateWLevel) %>%
  filter(n > 1)

# get rid of them!
SiteGiniLevel <- SiteGiniLevel %>%
  dplyr::distinct(SiteDateWLevel, .keep_all = TRUE)

SiteGiniLevel <- SiteGiniLevel %>%
  left_join(Combined_sites_unique, by = "SiteDateWLevel")

# quick sanity check
stopifnot("LIH" %in% names(SiteGiniLevel))



# some stuff that won't change across plots:
# Define the span for loess smoothing
loess_span <- 0.9  # Adjust this value as needed
# group some levels of SA
SiteGiniLevel$SA_Modified <- as.factor(ifelse(SiteGiniLevel$SA %in% c(3, 4, 5), "3-5",
                                              ifelse(SiteGiniLevel$SA %in% c(6, 7), "6-7",
                                                     ifelse(SiteGiniLevel$SA %in% c(8, 9, 10), "8-10", as.character(SiteGiniLevel$SA)))))
# Replace 'W Asia and Cyprus' with 'W Asia/Cyprus' in the Region column
SiteGiniLevel$Region <- sub("W Asia and Cyprus", "W Asia/Cyprus", SiteGiniLevel$Region)
# this fixes alignment problem in graphs

# Define plotting range (excluding missing values) (not currently used)
plotting_range <- quantile(SiteGiniLevel$DeltaCult, c(0.00, 1.00), na.rm = TRUE)  # originally 2nd to 98th percentile, excluding NAs

# For some graphs eliminate early outlier(s) 
# SiteGiniLevel <- filter(SiteGiniLevel, DeltaCult > -5000)

# For some graphs need to display site names: clean them up

# Function to clean non-ASCII characters
clean_site <- function(site) {
  iconv(site, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
}

# Clean the Site variable in the SiteGiniLevel data frame
SiteGiniLevel$Site <- sapply(SiteGiniLevel$Site, clean_site)


# Fit a loess model for each level of SA_Modified
predictions <- SiteGiniLevel %>%
  group_by(SA_Modified) %>%
  do({
    model <- loess(Gini ~ DeltaCult, data = ., span = loess_span)
    # Generate predictions only within the plotting range
    new_data <- data.frame(DeltaCult = seq(from = plotting_range[1], to = plotting_range[2], length.out = 100))
    data.frame(DeltaCult = new_data$DeltaCult, Gini_fitted = predict(model, newdata = new_data), SA_Modified = unique(.$SA_Modified))
  }) %>%
  ungroup()  # Ungroup to prevent issues in the plotting


# Now make  predictions for each of the 'Basal' and 'Apex' groups in LIH while ignoring the 'Other sites'
predictions1_W <- SiteGiniLevel %>%
  filter(LIH %in% c('Basal', 'Apex')) %>%  # Exclude 'Other sites'
  group_by(LIH) %>%
  do({
    model <- loess(Gini ~ DeltaCult, data = ., span = loess_span)
    # Generate predictions only within the plotting range
    new_data <- data.frame(DeltaCult = seq(from = plotting_range[1], to = plotting_range[2], length.out = 100))
    data.frame(DeltaCult = new_data$DeltaCult, Gini_fitted = predict(model, newdata = new_data), LIH = unique(.$LIH))
  }) %>%
  ungroup()  # Ungroup to prevent issues in the plotting

# For World there are more Bigregions than allowed by the default for shape codes, so add one:
# Assuming you have n unique Bigregions
unique_regions <- sort(unique(SiteGiniLevel$Bigregion))
shape_codes <- 0:(length(unique_regions) - 1)

Fig1a <- ggplot(SiteGiniLevel, aes(x = DeltaCult, y = Gini)) +
  # geom_point(aes(color = SA_Modified, shape = Bigregion), size = 0.6, alpha = 0.6) +
  # geom_line(data = predictions, aes(y = Gini_fitted, color = SA_Modified)) +
  # geom_line(data = predictions1_W, aes(y = Gini_fitted, color = LIH), linetype = "dashed", size = 1.0, show.legend = TRUE) +
  geom_point(aes(shape = Bigregion), size = 0.6, alpha = 0.2, show.legend = TRUE) + # show.legend = TRUE for shape
  geom_point(aes(color = SA_Modified), size = 0.6, alpha = 0.2, show.legend = FALSE) + # show.legend = FALSE for color
  geom_line(data = predictions, aes(y = Gini_fitted, color = SA_Modified), show.legend = FALSE) + # show.legend = FALSE for lines
  geom_line(data = predictions1_W, aes(y = Gini_fitted, color = LIH), linetype = "dashed", size = 1.0, show.legend = TRUE) +
  scale_color_brewer(palette = "Set2") + # Use Set2 color palette
  scale_shape_manual(values = shape_codes) +
  labs(color = "SA Level", shape = "Macroregion") +
  theme_bw() +
  theme(legend.title = element_text(size = 6),   # Smaller legend titles
        legend.text = element_text(size = 6),    # Smaller text in legends
        axis.text = element_text(size = 7),     # Smaller axis text
        axis.title = element_text(size = 10),   # Smaller axis titles
        axis.text.y = element_text(size = 10),  # Larger y-axis text
        legend.key.size = unit(c(0.5, 0.2), "lines")) +   # Adjust key height and width
  coord_cartesian(xlim = c(-5000, 8000), ylim = c(0.0, 0.9)) +
  scale_x_continuous(breaks = seq(-5000, 8000, by = 1000)) +
  theme(axis.text.x = element_blank(), # suppress x-axis elements
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank()) +
  theme(panel.border = element_blank()) + # Remove the plot frame
  annotate("text", x = -5000, y = 0.9,
           label = "World",
           hjust = 0, vjust = 1,  # Left align, vertically top
           fontface = "plain", size = 4) +  # Customize font
  # annotate("text", x = 8000, y = 0.02, 
  #          label = "SiteGiniLevel\n07082024", 
  #          hjust = 1, vjust = 1,  # Right align, vertically top
  #          fontface = "plain", size = 1.4) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue", size = 0.3) +
  guides(shape = guide_legend(override.aes = list(size = 1.5, linetype = 0)),
         color = guide_legend(override.aes = list(size = 0.3)))  # Adjust legend lines
Fig1a


# now make Fig1 lower for STRECO, like Fig1 upper but with log mean house size on x-axis


SiteGiniLevel <- SiteGiniLevel[SiteGiniLevel$MED_TAH > 0, ]
# does not currently remove any sites

SiteGiniLevel <- SiteGiniLevel %>%
  mutate(
    MED_TAH_log = log(MED_TAH), # Take log of MED_TAH
    MED_TAH_log_std = scale(MED_TAH_log),
    ML_TAH_std = scale(ML_TAH),
    Gini_std = scale(Gini)) # for use in regressions and phase plots

# Fit a loess model for each level of SA_Modified but now fitting for ML_TAH_log instead of Gini
predictions <- SiteGiniLevel %>%
  group_by(SA_Modified) %>%
  do({
    model <- loess(ML_TAH ~ DeltaCult, data = ., span = loess_span)
    # Generate predictions only within the plotting range
    new_data <- data.frame(DeltaCult = seq(from = plotting_range[1], to = plotting_range[2], length.out = 100))
    data.frame(DeltaCult = new_data$DeltaCult, ML_TAH_fitted = predict(model, newdata = new_data), SA_Modified = unique(.$SA_Modified))
  }) %>%
  ungroup()  # Ungroup to prevent issues in the plotting


# Now make  predictions for each of the 'Basal' and 'Apex' groups in LIH while ignoring the 'Other sites'
predictions1_W <- SiteGiniLevel %>%
  filter(LIH %in% c('Basal', 'Apex')) %>%  # Exclude 'Other sites'
  group_by(LIH) %>%
  do({
    model <- loess(ML_TAH ~ DeltaCult, data = ., span = loess_span)
    # Generate predictions only within the plotting range
    new_data <- data.frame(DeltaCult = seq(from = plotting_range[1], to = plotting_range[2], length.out = 100))
    data.frame(DeltaCult = new_data$DeltaCult, ML_TAH_fitted = predict(model, newdata = new_data), LIH = unique(.$LIH))
  }) %>%
  ungroup()  # Ungroup to prevent issues in the plotting

# For World there are more Bigregions than allowed by the default for shape codes, so add one:
# Assuming you have n unique Bigregions
unique_regions <- sort(unique(SiteGiniLevel$Bigregion))
shape_codes <- 0:(length(unique_regions) - 1)

Fig1b <- ggplot(SiteGiniLevel, aes(x = DeltaCult, y = ML_TAH)) +
  geom_point(aes(shape = Bigregion), size = 0.6, alpha = 0.2, show.legend = TRUE) +
  geom_point(aes(color = SA_Modified), size = 0.6, alpha = 0.2, show.legend = FALSE) +
  geom_line(data = predictions, aes(y = ML_TAH_fitted, color = SA_Modified), show.legend = FALSE) +
  geom_line(data = predictions1_W, aes(y = ML_TAH_fitted, color = LIH), linetype = "dashed", size = 1.0, show.legend = TRUE) +
  scale_color_brewer(palette = "Set2") +
  scale_shape_manual(values = shape_codes) +
  labs(
    color = "SA Level",
    shape = "Macroregion",
    y = "Mean log Total Area Household",
    x = "Years Before/After Plant Cultivation") + 
  theme_bw() +
  theme(
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 8),    # Larger x-axis text
    legend.key.size = unit(c(0.5, 0.2), "lines")
  ) +
  coord_cartesian(xlim = c(-5000, 8000), ylim = c(1, 10)) +
  scale_x_continuous(breaks = seq(-5000, 8000, by = 2000)) +
  theme(
    axis.text.x = element_text(size = 9),  # Customize size for x-axis text
    axis.ticks.x = element_line(),        # Enable x-axis ticks
    axis.title.x = element_text(size = 10) # Add x-axis title
  ) +
  theme(panel.border = element_blank()) +
  annotate(
    "text", x = -5000, y = 9.8,
    label = "World",
    hjust = 0, vjust = 1,
    fontface = "plain", size = 4
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue", size = 0.3) +
  guides(
    shape = guide_legend(override.aes = list(size = 1.5, linetype = 0)),
    color = guide_legend(override.aes = list(size = 0.3))
  )

Fig1b


Fig1a <- Fig1a +
  theme(
    legend.position = "right",  # Move legend to the right
    legend.key.size = unit(0.2, "lines"),  # Shrink legend keys
    legend.text = element_text(size = 5),  # Smaller legend text
    legend.title = element_text(size = 6)  # Smaller legend title
  )

Fig1b <- Fig1b +
  theme(
    legend.position = "right",  
    legend.key.size = unit(0.2, "lines"),
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 6)
  )

aligned_plots <- cowplot::plot_grid(
  Fig1a + theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")),  # Tighten margins
  Fig1b + theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")),
  ncol = 1, align = 'hv', axis = 'l', rel_heights = c(1, 1), vjust = -0.5
)


aligned_plots

# Save the combined plot in output/
ggsave(here("output", "Fig_1a_b.pdf"), aligned_plots, width = 7, height = 9)

# Fig 3 Examine covariance of productivity and growth for inequality

old.theme <- theme_set(theme_bw())

#Read in data in step 1

order$HousePhase<-as.factor(order$HousePhase)

#Remove cases where house area is missing
cfas<-cfas%>%filter(TotalAreaHouse>0)
cfas$HouseType[is.na(cfas$HouseType)] <- 0
cfas<-cfas%>%filter(HouseType!=15) #Public Building
cfas<-cfas%>%filter(HouseType!=8) #Ceremonial

#Recode HousePhase groups
cfas<-cfas%>%mutate(HousePhase=ifelse(cfas$Region=="W Asia and Cyprus",dplyr::recode(HousePhase,"Iron Age I" = "Iron Age","Iron Age II"="Iron Age","Bronze Age"="Late Bronze Age","Middle Bronze Age"="Late Bronze Age","Aceramic Neolithic"="Early Neolithic","Ceramic Neolithic"="Late Neolithic","Neolithic"="Late Neolithic","Late Neolithic/Early Chalcolithic"="Chalcolithic","Early Chalcolithic"="Chalcolithic","Middle Chalcolithic"="Chalcolithic","Middle and Late Chalcolithic"="Chalcolithic","Late Chalcolithic"="Chalcolithic","Middle-Final Chalcolithic"="Chalcolithic"),HousePhase))
cfas<-cfas%>%mutate(HousePhase=ifelse(cfas$Region=="Central Mexico",dplyr::recode(HousePhase,"Early Classic" = "Classic","Late Classic"="Classic"),HousePhase))
cfas<-cfas%>%mutate(HousePhase=ifelse(cfas$Region=="Southwest NA",dplyr::recode(HousePhase,"Early Fremont" = "Fremont","Late Fremont"="Fremont"),HousePhase))


#Set up factors and ordering for plots

cfas$Bigregion<-factor(cfas$Bigregion)
cfas$Region<-factor(cfas$Region)
cfas$Subregion<-factor(cfas$Subregion)
cfas$Subarea<-factor(cfas$Subarea)
cfas$PolityEcon<-as.numeric(cfas$PolityEcon)
cfas$PolityGov<-as.numeric(cfas$PolityGov)
cfas$PolityArch<-as.numeric(cfas$PolityArch)
cfas<-cfas%>%mutate(Gov=PolityEcon + PolityGov + PolityArch)


#Ordering for plots

cfas$SitePeriod <- factor(cfas$SitePeriod,levels=unique(cfas$SitePeriod[order(cfas$Region,cfas$tlkp.Period.Order)]))
cfas$SitePhase <- factor(cfas$SitePhase,levels=unique(cfas$SitePhase[order(cfas$Region,cfas$tlkp.Phase.Order)]))
cfas$HousePeriod <- factor(cfas$HousePeriod,levels=unique(cfas$HousePeriod[order(cfas$Region,cfas$tlkp.Period_1.Order)]))
cfas$HousePhase <- factor(cfas$HousePhase,levels=unique(cfas$HousePhase[order(cfas$Region,cfas$tlkp.Phase_1.Order)]))
cfas$Politytypdesc <- factor(cfas$Politytypdesc,levels=unique(cfas$Politytypdesc[order(cfas$TypeOfPolity)]))


#Add decile rank to each house, by site and House Phase

cfas<-cfas%>%group_by(Site,HousePeriod,HousePhase,NOfLevels,WhichLevel,PolityArch,PolityEcon,PolityGov,Gov)%>%mutate(decile=ntile(TotalAreaHouse, 10))
SiteDecile<-cfas%>%dplyr::group_by_at(vars(3,5:10,13:14,28:33,36:38,53:56,79:114))%>%dplyr::summarise(Latitude=mean(Latitude),Longitude=mean(Longitude),TotalArea=mean(TotalArea,na.rm=TRUE),WindowArea=(sum(WindowArea)/n()), WindowHH=(sum(WindowHH)/n()),MinHH=mean(MinHH,na.rm=TRUE),MaxHH=mean(MaxHH,na.rm=TRUE),CountHH=n(),BeginDate=min(BeginHouse),EndDate=max(EndHouse),PolityPop=mean(PolityPop,na.rm=TRUE),PolityArea=mean(PolityArea,na.rm=TRUE),ML_TAH=mean(log(TotalAreaHouse),na.rm=TRUE),SDL_TAH=sd(log(TotalAreaHouse),na.rm=TRUE),MED_TAH=median(TotalAreaHouse))%>%ungroup
SiteDecile10<-SiteDecile%>%filter(CountHH>9)

cfas.cov<-cfas
cfas.cov<-cfas.cov%>%filter(HousePhase!="")

#Combine Initial and Extended Coalescent from the Great Plains.

cfas.cov$HousePhase<-dplyr::recode(cfas.cov$HousePhase,'Initial Coalescent'="Extended Coalescent")
cfas.cov$HousePhase<-dplyr::recode(cfas.cov$HousePhase,'Late Neolithic/Early Chalcolithic'="Late Neolithic")

levels(cfas.cov$HousePhase)

#Distinguish NW Coast and North Pacific, and Desert SW from the rest of Southwest NA

recode_if <- function(x, condition, ...) {
  if_else(condition, dplyr::recode(x, ...), x)
}

#Distinguish Midwest from Deep South as a region

# cfas.cov <- cfas.cov %>%mutate(Region = if_else(Subregion == "Deep South", 'Deep South', Region))
# cfas.cov <- cfas.cov %>%mutate(Region = if_else(Subregion == "Midwest", 'Midwest', Region))
cfas.cov$HousePhase<-dplyr::recode(cfas.cov$HousePhase,'Lohman'="Early Mississippian")

cfas.cov<-cfas.cov %>% mutate(Region = recode_if(Region, Subregion == "North Pacific", "Northwest NA" = "North Pacific"))
cfas.cov<-cfas.cov %>% mutate(Region = recode_if(Region, Subregion == "Desert SW", "Southwest NA" = "Desert SW"))
cfas.cov<-cfas.cov%>%filter(Site!="Yautepec")

#More groupings

cfas.cov$Region<-dplyr::recode(cfas.cov$Region,'SE Europe'="BAIZ",'W Asia and Cyprus'="BAIZ",'North Africa'="BAIZ")
#cfas.cov$HousePhase<-dplyr::recode(cfas.cov$HousePhase,'Early Chalcolithic'="Chalcolithic",'Middle Chalcolithic'="Chalcolithic",'Middle-Final Chalcolithic'="Chalcolithic")

#Count number of measured houses by Site and House Phase (levels included to deal with a couple funky cases)

cfas.cov<-cfas.cov%>%group_by(Region,HousePhase)%>%mutate(BeginDate=max(BeginHouse,na.rm=TRUE),EndDate=min(EndHouse,na.rm=TRUE),CountHH=n(),SD=sd(TotalAreaHouse))%>%ungroup
cfas.cov$Midpoint<-(cfas.cov$EndDate+cfas.cov$BeginDate)/2

#Remove groups with fewer than 10 houses or with no variation in house area

cfas.cov<-cfas.cov%>%filter(CountHH>9)
cfas.cov<-cfas.cov%>%filter(SD>0)
cfas.cov<-cfas.cov%>%group_by(Region)%>%mutate(NPhase=n_distinct(HousePhase))
cfas.cov<-cfas.cov%>%filter(NPhase!=1)
check<-cfas.cov%>%dplyr::select(Site,Region,HousePhase,BeginDate,EndDate,BeginHouse,EndHouse,Midpoint)

#Determine decile groups by region and phase

cov<-cfas.cov%>%group_by(Region,HousePhase)%>%mutate(mlnH=mean(log(TotalAreaHouse),na.rm=TRUE),sdlnH=sd(log(TotalAreaHouse)),Gini=DescTools::Gini(TotalAreaHouse),Decile=ntile(TotalAreaHouse, 10))
cov$Hemisphere <- "Western"
cov[cov$Bigregion %in% c('Africa',
                         'Asia','Europe','Oceania'),]$Hemisphere <- "Eastern"

cov2<-cov%>%dplyr::group_by(Hemisphere,Region,HousePhase,CountHH,mlnH,sdlnH,Gini,Decile,Midpoint)%>%dplyr::summarise(ML_TAH=mean(log(TotalAreaHouse),na.rm=TRUE))%>%ungroup

#Remove problematic groups

cov2<-cov2%>%filter(HousePhase!="MD",Region!="S Africa",HousePhase!="Fremont",HousePhase!="Gobernador",HousePhase!="Dinetah")
cov2<-cov2%>%filter(HousePhase!="Chimu")

#Number phases within region

sort<-cov2%>%group_by(Region,Midpoint,HousePhase)%>%summarize()%>%ungroup
sort<-sort%>%group_by(Region)%>%mutate(Phase=row_number())%>%ungroup
cov2<-left_join(cov2,sort,by= c("Region", "HousePhase","Midpoint"))

#calculate difference between ML_TAH by region, phase and decile

cov2<-cov2%>%arrange(Region,Phase,HousePhase,Decile)

#using lead, calculate the change from a given phase to the next phase

cov2<-cov2%>%group_by(Region,Decile)%>%mutate(DT=abs(lead(Midpoint)-Midpoint),DHD = -1*(ML_TAH - lead(ML_TAH)),Dsd=-1*((sdlnH-lead(sdlnH))/DT),DG=-1*(Gini-lead(Gini)))%>%ungroup
cov3<-cov2%>%group_by(Hemisphere,Region,Phase, HousePhase,CountHH,mlnH,sdlnH,Gini,Dsd,DG,DT,Midpoint)%>%summarize(Cov = cov(ML_TAH,DHD/DT))%>%ungroup
cov3<-cov3%>%group_by(Region)%>%mutate(DH=-1*(mlnH-lead(mlnH))/DT)%>%ungroup

# Fig 3 STRECO

cov3<-cov3%>%arrange(Region,Phase)
cov3$Midpoint <- floor(cov3$Midpoint)
cov3$NewPhase <- paste(cov3$Phase, cov3$HousePhase, sep = "-")
cov3$NewPhase<-fct_reorder(cov3$NewPhase,cov3$Phase)

cov3_gb <- cov3 %>%
  filter(Region == "Great Britain") %>%
  arrange(Phase)


GB <- ggplot(
  data = cov3[cov3$Region == "Great Britain", ],
  aes(x = mlnH, y = Gini)
) +
  ggforce::geom_link(
    aes(
      xend = dplyr::lead(mlnH),
      yend = dplyr::lead(Gini)
    ),
    arrow = arrow(type = "closed", length = unit(0.15, "inches")),
    color = "gray",
    na.rm = TRUE
  ) +
  geom_point(aes(size = CountHH, color = NewPhase)) +
  ggrepel::geom_text_repel(aes(label = Midpoint)) +
  xlab(expression("Mean-log of Residence Area (m"^2*")")) +
  ylab("Gini Coefficient") +
  ggtitle("Great Britain") +
  guides(color = guide_legend(ncol = 2), size = "none") +
  scale_color_discrete(name = "Phase") +
  scale_size_continuous(name = "Residence Count")

GB


SE <- ggplot(
  data = cov3[cov3$Region == "Southeast NA", ],
  aes(x = mlnH, y = Gini)
) +
  ggforce::geom_link(
    aes(
      xend = dplyr::lead(mlnH),
      yend = dplyr::lead(Gini)
    ),
    arrow = arrow(type = "closed", length = unit(0.15, "inches")),
    color = "gray",
    na.rm = TRUE
  ) +
  geom_point(aes(size = CountHH, color = NewPhase)) +
  ggrepel::geom_text_repel(aes(label = Midpoint)) +
  xlab(expression("Mean-log of Residence Area (m"^2*")")) +
  ylab("Gini Coefficient") +
  ggtitle("Southeastern US") +
  guides(color = guide_legend(ncol = 2), size = "none") +
  scale_color_discrete(name = "Phase") +
  scale_size_continuous(name = "Residence Count")

SE


cov3_gb <- cov3 %>%
  filter(Region == "BAIZ") %>%
  arrange(Phase)


BAIZ <- ggplot(
  data = cov3[cov3$Region == "BAIZ", ],
  aes(x = mlnH, y = Gini)
) +
  ggforce::geom_link(
    aes(
      xend = dplyr::lead(mlnH),
      yend = dplyr::lead(Gini)
    ),
    arrow = arrow(type = "closed", length = unit(0.15, "inches")),
    color = "gray",
    na.rm = TRUE
  ) +
  geom_point(aes(size = CountHH, color = NewPhase)) +
  ggrepel::geom_text_repel(aes(label = Midpoint)) +
  xlab(expression("Mean-log of Residence Area (m"^2*")")) +
  ylab("Gini Coefficient") +
  ggtitle("SE Europe & W Asia") +
  guides(color = guide_legend(ncol = 2), size = "none") +
  scale_color_discrete(name = "Phase") +
  scale_size_continuous(name = "Residence Count")

BAIZ


Fig3_repcheck<-ggarrange(GB,BAIZ,SE,ncol=1,labels=c("A","B","C"))


# Fig 3 STRECO
ggsave(here("output", "Fig_3.pdf"), Fig3_repcheck, width = 7, height = 9)
####################### end Figures Part 2



##### Begin Part 3 regressions for Figure 2 & 4

# For regressions, recode neg values in the Delta vars to -1; others to 1
# But first make a version of DeltaCult that will retain its original values
SiteGiniLevel$DCult <- SiteGiniLevel$DeltaCult
# All variables starting with "Delta" need to be processed
delta_vars <- grep("^Delta", names(SiteGiniLevel), value = TRUE)

SiteGiniLevel <- SiteGiniLevel %>%
  mutate(across(all_of(delta_vars), sign))


# Create a vector of variable names to standardize
vars_to_standardize <- c("SA", "Gov_I", "Fourscale")

SiteGiniLevel_standardized <- SiteGiniLevel %>%
  mutate(across(all_of(vars_to_standardize), ~ scale(.x, center = TRUE, scale = TRUE)[, 1]))

# Check the structure and summary of the modified data frame
summary(SiteGiniLevel_standardized)
str(SiteGiniLevel_standardized)


# Check column names for any unusual characters: change for brm which is very picky

SiteGiniLevel_standardized <- SiteGiniLevel_standardized %>%
  mutate(
    Ginistd = as.numeric(Gini_std),
    MLTAHstd = as.numeric(ML_TAH_std),
    GovI = as.numeric(Gov_I)
  )

# Asia <- SiteGiniLevel_standardized %>% filter(Bigregion == 'Asia') 
# Europe <- SiteGiniLevel_standardized %>% filter(Bigregion == 'Europe') 
# Americas <- subset(SiteGiniLevel_standardized, Bigregion == 'South America' | Bigregion == 'North America'
#                    | Bigregion == 'Mesoamerica') 


# Look at log of mean house size to choose correct family
ggplot(SiteGiniLevel, aes(x = ML_TAH_std)) + 
  geom_histogram(binwidth = 0.1, fill = "blue", color = "white") + 
  ggtitle("Histogram of ML_TAH_std") +
  xlab("ML_TAH_std") + 
  ylab("Frequency") +
  theme_minimal()


###  moments
ML_TAH_std <-  SiteGiniLevel$ML_TAH_std  
# Calculate mean and variance
mean_ML_TAH_std <- mean(ML_TAH_std)
variance_ML_TAH_std <- var(ML_TAH_std)
# Calculate skewness and kurtosis
skewness_ML_TAH_std <- skewness(ML_TAH_std)
kurtosis_ML_TAH_std <- kurtosis(ML_TAH_std)  # Note: this returns excess kurtosis

# Print results
print(paste("Mean: ", mean_ML_TAH_std))
print(paste("Variance: ", variance_ML_TAH_std))
print(paste("Skewness: ", skewness_ML_TAH_std))
print(paste("Kurtosis (Excess): ", kurtosis_ML_TAH_std))

# Let's look at Gini to choose right family for brms

ggplot(SiteGiniLevel, aes(x = Gini_std[,1])) + 
  geom_histogram(binwidth = 0.03, fill = "blue", color = "blue") + 
  ggtitle("Histogram of Gini Coefficient") +
  xlab("Gini Coefficient") + 
  ylab("Frequency") +
  theme_minimal()

###  moments
gini_coefficients <-  SiteGiniLevel$Gini_std  
# Calculate mean and variance
mean_gini <- mean(gini_coefficients)
variance_gini <- var(gini_coefficients)
# Calculate skewness and kurtosis
skewness_gini <- skewness(gini_coefficients)
kurtosis_gini <- kurtosis(gini_coefficients)  # Note: this returns excess kurtosis

# Print results
print(paste("Mean: ", mean_gini))
print(paste("Variance: ", variance_gini))
print(paste("Skewness: ", skewness_gini))
print(paste("Kurtosis (Excess): ", kurtosis_gini))

# OK start brm: this can take a few minutes depending on computer…
# 
future::plan(multisession, workers = 4)  # Adjust 'workers' based on your number of cores

### WORLD
# Note: the code that produced figures in the publication did not have a seed set
# for brm, so expect slightly different results each time you run this code. If you want to set seed explicitly 
# call World_fit <- brm(..., seed = 12345)

priors <- c(
  prior(normal(0, 1), class = "b"),           # Priors for coefficients
  prior(normal(0, 0.5), class = "Intercept"), # Prior for intercept
  prior(gamma(2, 0.1), class = "nu")          # Prior for degrees of freedom
)

World_fit <- brm(
  formula = bf(Ginistd ~ DeltaCult + SA + Fourscale + DeltaCopperSm + DeltaBronze +
                 DeltaPort + DeltaRiding + DeltaAnMan +
                 DeltaTraction + DeltaIronSm + GovI +
                 (1 | Region)),
  family = student(link = "identity"), # Gini is std
  data = SiteGiniLevel_standardized,
  prior = priors,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  cores = 4,  # Use one core per chain
  control = list(adapt_delta = 0.99)
)

# Summarize the model
summary(World_fit)

# Plot the model diagnostics
plot(World_fit)

# Posterior predictive checks
pp_check(World_fit)

# Compute Bayesian R²
bayes_R2(World_fit)


# Extract posterior samples in a data-frame-like format (plays nicely with dplyr/tidyr)
posterior_samples_df <- posterior::as_draws_df(World_fit)

# Calculate 50% and 90% HDIs
hdi_50 <- bayestestR::hdi(posterior_samples_df, ci = 0.50)
hdi_90 <- bayestestR::hdi(posterior_samples_df, ci = 0.90)

# Add suffixes to differentiate between intervals
hdi_50 <- hdi_50 %>% dplyr::rename_with(~paste0(.x, "_50"), -Parameter)
hdi_90 <- hdi_90 %>% dplyr::rename_with(~paste0(.x, "_90"), -Parameter)

# Combine the HDI results
hdi_combined <- dplyr::full_join(hdi_50, hdi_90, by = "Parameter")

# Calculate medians (fixed effects only)
medians <- posterior_samples_df %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with("b_"), median)) %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "Parameter",
                      values_to = "Median")


# Merge medians with HDIs
hdi_combined <- hdi_combined %>%
  left_join(medians, by = "Parameter")

# Filter out the intercept and phi (if you don't want them in the plot)
hdi_combined <- hdi_combined %>%
  filter(grepl("^b_", Parameter) & Parameter != "b_Intercept")

# Sort the coefficients by median values
hdi_combined <- hdi_combined %>%
  mutate(Parameter = gsub("^b_", "", Parameter)) %>%
  arrange(desc(Median)) %>%
  mutate(Parameter = factor(Parameter, levels = unique(Parameter)))

# Modify the Parameter names for plotting
hdi_combined <- hdi_combined %>%
  mutate(Parameter = gsub("^b_", "", Parameter)) %>%
  mutate(Parameter = factor(Parameter, levels = rev(unique(Parameter))))

# Get Set2 colors from RColorBrewer
set2_colors <- RColorBrewer::brewer.pal(n = 5, name = "Set2")

# Create the plot with "Set2" colors and a vertical line at 0
World_plot_G <- ggplot(hdi_combined, aes(y = Parameter)) +
  geom_vline(xintercept = 0, color = set2_colors[1], size = 1.25) +  # Add vertical line at 0 with second color from Set2
  geom_errorbarh(aes(xmin = CI_low_50, xmax = CI_high_50), height = 0.0, color = set2_colors[3]) +
  geom_errorbarh(aes(xmin = CI_low_90, xmax = CI_high_90), height = 0.0, color = set2_colors[4]) +
  geom_segment(aes(x = CI_low_50, xend = CI_high_50, y = Parameter, yend = Parameter), color = set2_colors[3], 
               linewidth = 1.75) +  # Line for 50% HDIs
  geom_point(aes(x = Median), color = "white", shape = 18) + # diamond
  theme_bw() +
  labs(
    title = expression("Worldwide Influences on Wealth Inequality (Gini_std)"),
    x = "Estimate",
    y = "Fixed Coefficients"
  )  +
  scale_x_continuous(limits = c(-0.75, 0.75)) +  # c(-0.50, 0.50))
  theme(
    plot.title = element_text(size = 8.5),
    axis.text.y = element_text(size = 6.5, margin = margin(r = 5)),  
    axis.title.y = element_text(size = 6.5, margin = margin(r = 10)),  
    plot.margin = margin(6, 6, 6, 6),
    panel.spacing = unit(0.1, "lines"),
    axis.title.x = element_blank(),   
    axis.text.x = element_blank(),    
    axis.ticks.x = element_blank(),   
    panel.border = element_blank()    
  )
World_plot_G

# Extract the posterior samples for the fixed effects and random intercepts using as_draws_matrix
posterior_samples <- posterior::as_draws_matrix(World_fit)

# Convert the posterior samples to a data frame for easier manipulation
posterior_samples_df <- as.data.frame(posterior_samples)

# Inspect the column names to verify the patterns
print(names(posterior_samples_df))

# Adjust patterns based on inspected column names
fixed_effects_cols <- grep("^b_", names(posterior_samples_df), value = TRUE)
random_intercepts_cols <- grep("^sd_", names(posterior_samples_df), value = TRUE)
random_effects_cols <- grep("^r_region", names(posterior_samples_df), value = TRUE)

# Ensure we have the correct columns
print(fixed_effects_cols)
print(random_intercepts_cols)
print(random_effects_cols)

# Calculate the variance of the fixed effects using base R
fixed_effects_variance <- sum(apply(posterior_samples_df[, fixed_effects_cols], 2, var))

# Calculate the variance of the random intercepts using base R
if (length(random_intercepts_cols) > 0) {
  if (length(random_intercepts_cols) == 1) {
    # Handle single column case
    random_intercepts_variance <- mean(posterior_samples_df[[random_intercepts_cols[1]]]^2)
  } else {
    random_intercepts_variance <- sum(apply(posterior_samples_df[, random_intercepts_cols], 2, function(x) mean(x^2)))
  }
} else {
  random_intercepts_variance <- 0
}

# Calculate the variance of the random effects by region using base R
if (length(random_effects_cols) > 0) {
  random_effects_variance <- sum(apply(posterior_samples_df[, random_effects_cols], 2, var))
} else {
  random_effects_variance <- 0
}

# Compute the residual variance (phi parameter in the beta regression model)
residual_variance <- mean(posterior_samples_df$sigma^2, na.rm = TRUE)


# Calculate the total variance
total_variance <- fixed_effects_variance + random_intercepts_variance + random_effects_variance + residual_variance

# Proportion of variance explained by fixed effects
prop_fixed_effects <- fixed_effects_variance / total_variance

# Proportion of variance explained by random intercepts
prop_random_intercepts <- (random_intercepts_variance + random_effects_variance) / total_variance

# Print results using Bayesian-appropriate terminology
cat("Contribution of fixed effects to the total posterior variance:", prop_fixed_effects, "\n")
cat("Contribution of random intercepts to the total posterior variance:", prop_random_intercepts, "\n")


### now repeat WORLD for growth as measured by ML_TAH_std
# as above except switch brmsfamily and make change to priors (phi but now called shape)

World_fit <- brm(
  formula = bf(MLTAHstd ~ DeltaCult + SA + Fourscale + DeltaCopperSm + DeltaBronze +
                 DeltaPort + DeltaRiding + DeltaAnMan +
                 DeltaTraction + DeltaIronSm + GovI +
                 (1 | Region)),
  family = student(link = "identity"), # dependent is std
  data = SiteGiniLevel_standardized,
  # prior = c(
  #   set_prior("normal(0, 1)", class = "b"),          # Priors for coefficients
  #   set_prior("exponential(0.5)", class = "shape")   # Prior for the shape parameter
  # ),
  prior = priors,
  warmup = 1000,
  iter = 2000,
  chains = 4,
  cores = 4,  # Use one core per chain
  control = list(adapt_delta = 0.99)
)

# Summarize the model
summary(World_fit)

# Plot the model diagnostics
plot(World_fit)

# Posterior predictive checks
pp_check(World_fit)

# Compute Bayesian R²
bayes_R2(World_fit)

# Extract posterior samples in a data-frame-like format (plays nicely with dplyr/tidyr)
posterior_samples_df <- posterior::as_draws_df(World_fit)

# Calculate 50% and 90% HDIs
hdi_50 <- bayestestR::hdi(posterior_samples_df, ci = 0.50)
hdi_90 <- bayestestR::hdi(posterior_samples_df, ci = 0.90)

# Add suffixes to differentiate between intervals
hdi_50 <- hdi_50 %>% dplyr::rename_with(~paste0(.x, "_50"), -Parameter)
hdi_90 <- hdi_90 %>% dplyr::rename_with(~paste0(.x, "_90"), -Parameter)

# Combine the HDI results
hdi_combined <- dplyr::full_join(hdi_50, hdi_90, by = "Parameter")

# Calculate medians (fixed effects only)
medians <- posterior_samples_df %>%
  dplyr::summarise(dplyr::across(dplyr::starts_with("b_"), median)) %>%
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "Parameter",
                      values_to = "Median")


# Merge medians with HDIs
hdi_combined <- hdi_combined %>%
  left_join(medians, by = "Parameter")

# Filter out the intercept and phi (if you don't want them in the plot)
hdi_combined <- hdi_combined %>%
  filter(grepl("^b_", Parameter) & Parameter != "b_Intercept")

# Sort the coefficients by median values
hdi_combined <- hdi_combined %>%
  mutate(Parameter = gsub("^b_", "", Parameter)) %>%
  arrange(desc(Median)) %>%
  mutate(Parameter = factor(Parameter, levels = unique(Parameter)))

# Modify the Parameter names for plotting
hdi_combined <- hdi_combined %>%
  mutate(Parameter = gsub("^b_", "", Parameter)) %>%
  mutate(Parameter = factor(Parameter, levels = rev(unique(Parameter))))

# Get Set2 colors from RColorBrewer
set2_colors <- RColorBrewer::brewer.pal(n = 5, name = "Set2")

# Create the plot with "Set2" colors and a vertical line at 0
World_plot_W <- ggplot(hdi_combined, aes(y = Parameter)) +
  geom_vline(xintercept = 0, color = set2_colors[1], size = 1.25) +  # Add vertical line at 0 with second color from Set2
  geom_errorbarh(aes(xmin = CI_low_50, xmax = CI_high_50), height = 0.0, color = set2_colors[3]) +
  geom_errorbarh(aes(xmin = CI_low_90, xmax = CI_high_90), height = 0.0, color = set2_colors[4]) +
  geom_segment(aes(x = CI_low_50, xend = CI_high_50, y = Parameter, yend = Parameter), color = set2_colors[3], 
               linewidth = 1.75) +  # Line for 50% HDIs
  geom_point(aes(x = Median), color = "white", shape = 18) + # diamond
  theme_bw() +
  labs(
    title = expression("Worldwide Influences on Wealth (mean log(Total House Area))"),
    x = "Estimate",
    y = "Fixed Coefficients"
  )  +
  scale_x_continuous(limits = c(-0.75, 0.75)) +
  theme(
    plot.title = element_text(size = 8.5),
    axis.text.y = element_text(size = 6.5, margin = margin(r = 5)),  
    axis.title.y = element_text(size = 6.5, margin = margin(r = 10)),  
    plot.margin = margin(6, 6, 6, 6),
    panel.spacing = unit(0.1, "lines"),
    panel.border = element_blank()    
  )
World_plot_W # for Wealth

# Align plots using cowplot
aligned_plots <- plot_grid(World_plot_G, World_plot_W,
                           ncol = 1, align = 'hv', axis = 'l')
aligned_plots

# Save the combined plot
ggsave(here("output", "Fig_2.pdf"), aligned_plots, width = 7, height = 9)



# Extract the posterior samples for the fixed effects and random intercepts using as_draws_matrix
posterior_samples <- posterior::as_draws_matrix(World_fit)

# Convert the posterior samples to a data frame for easier manipulation
posterior_samples_df <- as.data.frame(posterior_samples)

# Inspect the column names to verify the patterns
print(names(posterior_samples_df))

# Adjust patterns based on inspected column names
fixed_effects_cols <- grep("^b_", names(posterior_samples_df), value = TRUE)
random_intercepts_cols <- grep("^sd_", names(posterior_samples_df), value = TRUE)
random_effects_cols <- grep("^r_region", names(posterior_samples_df), value = TRUE)

# Ensure we have the correct columns
print(fixed_effects_cols)
print(random_intercepts_cols)
print(random_effects_cols)

# Calculate the variance of the fixed effects using base R
fixed_effects_variance <- sum(apply(posterior_samples_df[, fixed_effects_cols], 2, var))

# Calculate the variance of the random intercepts using base R
if (length(random_intercepts_cols) > 0) {
  if (length(random_intercepts_cols) == 1) {
    # Handle single column case
    random_intercepts_variance <- mean(posterior_samples_df[[random_intercepts_cols[1]]]^2)
  } else {
    random_intercepts_variance <- sum(apply(posterior_samples_df[, random_intercepts_cols], 2, function(x) mean(x^2)))
  }
} else {
  random_intercepts_variance <- 0
}

# Calculate the variance of the random effects by region using base R
if (length(random_effects_cols) > 0) {
  random_effects_variance <- sum(apply(posterior_samples_df[, random_effects_cols], 2, var))
} else {
  random_effects_variance <- 0
}

# Compute the residual variance (phi parameter in the beta regression model)
residual_variance <- mean(posterior_samples_df$sigma^2, na.rm = TRUE)


# Calculate the total variance
total_variance <- fixed_effects_variance + random_intercepts_variance + random_effects_variance + residual_variance

# Proportion of variance explained by fixed effects
prop_fixed_effects <- fixed_effects_variance / total_variance

# Proportion of variance explained by random intercepts
prop_random_intercepts <- (random_intercepts_variance + random_effects_variance) / total_variance

# Print results using Bayesian-appropriate terminology
cat("Contribution of fixed effects to the total posterior variance:", prop_fixed_effects, "\n")
cat("Contribution of random intercepts to the total posterior variance:", prop_random_intercepts, "\n")




# produce Fig 4 using time as a latent variable in a joint model 
# tensor plots
# Fit the model first for WAC + SEE
# Load required libraries
library(mgcv)
library(gratia)
library(ggplot2)
library(MASS)
library(dplyr)


# check some values first
summary(SiteGiniLevel$MED_TAH_log)
summary(SiteGiniLevel$Gini)
any(is.na(SiteGiniLevel$MED_TAH_log))  # Check for missing values
any(is.na(SiteGiniLevel$Gini))
any(is.infinite(SiteGiniLevel$MED_TAH_log))  # Check for infinite values
any(is.infinite(SiteGiniLevel$Gini))
# Ok no missing or infinite values
# check for small groups that may cause splines to fail
SiteGiniLevel %>%
  group_by(Region) %>%
  summarise(n = n()) %>%
  arrange(n)

# remove regions with fewer than 40 sites
regions_to_keep <- SiteGiniLevel %>%
  group_by(Region) %>%
  summarise(n = n()) %>%
  filter(n >= 40) %>%
  pull(Region)


# Filter out regions with fewer than n sites 
SiteGiniLevel_filtered <- SiteGiniLevel %>%
  filter(Region %in% regions_to_keep) 

# Get rid of extraneous columns
# Remove columns by range
SiteGiniLevel_filtered <- SiteGiniLevel_filtered[, -(14:32)]
# Remove individual columns 
SiteGiniLevel_filtered <- SiteGiniLevel_filtered[, -c(1, 13, 18, 23, 24, 25, 28, 31, 32)]

glimpse(SiteGiniLevel_filtered)

SiteGiniLevel_filtered <- SiteGiniLevel_filtered %>%
  mutate(
    ML_TAH_std = as.vector(ML_TAH_std),
    Gini_std = as.vector(Gini_std),
    MED_TAH_log_std = as.vector(MED_TAH_log_std)
  )

# Create the Old_World data frame from the filtered world
Old_World <- SiteGiniLevel_filtered %>%
  filter(Bigregion %in% c("Africa", "Asia", "Europe", "Oceania"))

Old_World %>%
  group_by(Region) %>%
  summarise(n = n()) %>%
  arrange(n)

# Create the New_World data frame
New_World <- SiteGiniLevel_filtered %>%
  filter(!Bigregion %in% c("Africa", "Asia", "Europe", "Oceania"))

# temporarily needs this
WAC  <- Old_World  %>%
  filter(Region %in% c("W Asia/Cyprus"))

# temporarily needs this
WACSEE_dat  <- Old_World  %>%
  filter(Region %in% c("W Asia/Cyprus", "SE Europe"))

# and this
GB_dat  <- Old_World  %>%
  filter(Region %in% c("Great Britain"))

gini_gam_main <- gam(Gini ~ s(ML_TAH_std) + s(Date) + te(ML_TAH_std, Date), 
                     data = WACSEE_dat, method = "REML")



# WACSEE
gini_gam_main_WACSEE <- gini_gam_main

capture.output(summary(gini_gam_main_WACSEE),
               file = here("output", "WACSEE_model_summary.txt"))


# plot main effect of wealth
draw(gini_gam_main, select = "s(ML_TAH_std)") +
  labs(
    title = "Main Effect of Wealth on Gini",
    x = "Mean Wealth or Productivity (ML_TAH_std)",
    y = "Effect on Gini"
  )

# Plot main effect of date
draw(gini_gam_main, select = "s(Date)") +
  labs(
    title = "Main Effect of Time on Gini",
    x = "Years BC/AD",
    y = "Effect on Gini"
  )

# plot tensor interaction surface
draw(gini_gam_main, select = "te(ML_TAH_std,Date)") +
  labs(
    title = "Wealth–Time Interaction on Gini",
    x = "Mean Wealth or Productivity (ML_TAH_std)",
    y = "Years BC/AD",
    fill = "Interaction Effect"
  )

# combine and save with patchwork
library(patchwork)

p1 <- draw(gini_gam_main, select = "s(ML_TAH_std)") +
  labs(title = "Main Effect: Wealth")

p2 <- draw(gini_gam_main, select = "s(Date)") +
  labs(title = "Main Effect: Time")

p3 <- draw(gini_gam_main, select = "te(ML_TAH_std,Date)") +
  labs(title = "Interaction Surface") +
  scale_fill_gradientn(
    colors = heat.colors(100),
    name = "Predicted Gini",
    na.value = "white"  # <- ensures areas beyond the data (e.g. low density) are white
  )


# Combine
# Define a layout where the interaction plot (C) is taller
layout <- "
AB
CC
"

combined_WACSEE <- p1 + p2 + p3 +
  plot_layout(design = layout,
              heights = c(1, 1.8))  # Top row slightly shorter, bottom row taller

combined_WACSEE

# Save
ggsave(here("output", "Gini_GAM_components.WACSEE.png"), combined_WACSEE, width = 7, height = 9)

# Now same thing, GB

gini_gam_main_GB <- gam(Gini ~ s(ML_TAH_std) + s(Date) + te(ML_TAH_std, Date), 
                     data = GB_dat, method = "REML")

capture.output(summary(gini_gam_main_GB),
               file = here("output", "GB_model_summary.txt"))



# plot main effect of wealth
draw(gini_gam_main, select = "s(ML_TAH_std)") +
  labs(
    title = "Main Effect of Wealth on Gini",
    x = "Mean Wealth or Productivity (ML_TAH_std)",
    y = "Effect on Gini"
  )

# Plot main effect of date
draw(gini_gam_main, select = "s(Date)") +
  labs(
    title = "Main Effect of Time on Gini",
    x = "Years BC/AD",
    y = "Effect on Gini"
  )

# plot tensor interaction surface
draw(gini_gam_main, select = "te(ML_TAH_std,Date)") +
  labs(
    title = "Wealth–Time Interaction on Gini",
    x = "Mean Wealth or Productivity (ML_TAH_std)",
    y = "Years BC/AD",
    fill = "Interaction Effect"
  )

# combine and save with patchwork
library(patchwork)

p1 <- draw(gini_gam_main, select = "s(ML_TAH_std)") +
  labs(title = "Main Effect: Wealth")

p2 <- draw(gini_gam_main, select = "s(Date)") +
  labs(title = "Main Effect: Time")

p3 <- draw(gini_gam_main, select = "te(ML_TAH_std,Date)") +
  labs(title = "Interaction Surface") +
  scale_fill_gradientn(
    colors = heat.colors(100),
    name = "Predicted Gini",
    na.value = "white"  # <- ensures areas beyond the data (e.g. low density) are white
  )



# Combine
# Define a layout where the interaction plot (C) is taller
layout <- "
AB
CC
"

combined_GB <- p1 + p2 + p3 +
  plot_layout(design = layout,
              heights = c(1, 1.8))  # Top row slightly shorter, bottom row taller

combined_GB

# Save
ggsave(here("output", "Gini_GAM_components.GB.png"), combined_GB, width = 7, height = 9)

# combine these two vertically

# Wrap each composite as single object
combined_top <- wrap_elements(combined_WACSEE)
combined_bottom <- wrap_elements(combined_GB)

# Combine vertically
combined_stacked <- combined_top / combined_bottom +
  plot_layout(ncol = 1, heights = c(1, 1))  # Adjust relative height if desired

# Now overlay custom labels in upper-left corners
final_plot <- ggdraw(combined_stacked) +
  draw_plot_label(
    label = c("A: Western Asia and SE Europe", "B: Great Britain"),
    x = c(0.01, 0.01),  # Horizontal positions for labels
    y = c(0.99, 0.48),  # Vertical positions (top and mid-way)
    hjust = 0, vjust = 1,
    fontface = "bold", size = 12
  )

# View
final_plot <- ggdraw(combined_stacked) +
  draw_plot_label(
    label = c("A: Western Asia and SE Europe", "B: Great Britain"),
    x = c(0.01, 0.01),       # Left aligned
    y = c(0.99, 0.51),       # ⬆️ Move label B upward slightly from 0.48
    hjust = 0, vjust = 1,
    fontface = "bold", size = 12
  )


final_plot
# Fig 4 STRECO
ggsave(here("output", "Fig_4.pdf"), final_plot, width = 7, height = 9)


########### end of Part 3, regressions for Figures 2 & 4 
