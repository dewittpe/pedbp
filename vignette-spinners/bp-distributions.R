#'---
#'title: "Pediatric Blood Pressure Distributions"
#'output:
#'  bookdown::html_document2:
#'    toc: true
#'    number_sections: true
#'bibliography: references.bib
#'vignette: >
#'  %\VignetteIndexEntry{Pediatric Blood Pressure Distributions}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---
#'
#+ label = "setup", include = FALSE
knitr::opts_chunk$set(collapse = TRUE)
#'
#' # Introduction
#'
#' Part of the work of @martin2022machine required transforming blood
#' pressurement measurement into percentiles based on published norms.  This
#' work was complicated by the fact that data for pediatric blood pressure
#' precentiles is sparse and gennerally only applicable to children at least
#' one year of age and requires height, a commonly unavailable data point in
#' electronic health records for a variety of reasons.
#'
#' A solution to building pediatric blood pressure percentiles was developed and
#' is presented here for others to use.  Inputs for the developed method are:
#'
#' 1. Patient sex (male/female) _required_
#' 2. Systolic blood pressure (mmHg) _required_
#' 3. Diastolic blood pressure (mmHg) _required_
#' 4. Patient height (cm) _if known_.
#'
#' Given the inputs the following logic is used to determine which data sets
#' will be used to inform the blood pressure percentiles.  Under one year of
#' age, the data from @gemelli1990longitudinal will be used; height is
#' irrelevent.  For those at least one year of age with a known height then the
#' @flynn2017clinical data sets are used.  If height is unknown and age is at
#' least three years then data from @lo2013prehypertension is used.  Lastly,
#' under three years of age with unknown height have blood pressure precentiles
#' estimated by the @flynn2017clinical data with the default of the median
#' height for sex and age (Figure \@ref(fig:flowchart)).
#'
#' ![](./flowchart.png)
#+ label = "flowchart", echo = FALSE, results = "hide", out.width = 0.1, out.height = 0.1, fig.cap = "Flowchart for determining which data source informs blood pressure percentiles by age and height."
plot(1:10) # this is just here to "trick" a figure caption onto the flowchart
#'
#'
# /* --------------------------------------------------------------------------
#
# Blake's Work
#' This is a copy of the work done my Martin and DeWitt is modifying
# Script to import, create, and graph the novel BP percentile approach used in the SBI project

# First will set the seed, load the necessary packages, and set up appropriate file path fragments
######
set.seed(32312)
library(magrittr)

# File Paths for all SBI Data files
options(user_norms_path = "./references/sbi/sbi_data/")
bp_norms_lo_boys <- paste0(getOption("user_norms_path"), "Lo_bp_norms_boys.csv")
bp_norms_lo_girls <- paste0(getOption("user_norms_path"), "Lo_bp_norms_girls.csv")                 
bp_norms_cdc_boys <- paste0(getOption("user_norms_path"), "CDC_bp_norms_boys.csv")
bp_norms_cdc_girls <- paste0(getOption("user_norms_path"), "CDC_bp_norms_girls.csv")
Gemelli_bp_under_one_boys <- paste0(getOption("user_norms_path"), "Gemelli_bp_under_one_boys.csv")
Gemelli_bp_under_one_girls <- paste0(getOption("user_norms_path"), "Gemelli_bp_under_one_girls.csv")
length_cdc_infant_boys <- paste0(getOption("user_norms_path"), "CDC_length_infant_boys.csv")
length_cdc_infant_girls <- paste0(getOption("user_norms_path"), "CDC_length_infant_girls.csv")
length_cdc_boys <- paste0(getOption("user_norms_path"), "CDC_length_boys.csv")
length_cdc_girls <- paste0(getOption("user_norms_path"), "CDC_length_girls.csv")

# Prompt user to enter whether or not height is available, if so what is the height in cm, and then what the patient's age is in years

#is_ht_pres <- readline(prompt = "Is patient height known (y/n)? ") 
is_ht_pres <- "y"
#
if(is_ht_pres == "y"){
  #ht_entered <- readline(prompt = "Please enter the patient's height in cm:  ")
  #ht_entered <- as.numeric(ht_entered)
  ht_entered  <- 72.5
}
#
#age_entered_yrs <- readline(prompt = "Please enter the patient's age in years (from 0.083 (1 month) to <=18: ")
#age_entered_yrs <- as.numeric(age_entered_yrs)
age_entered_yrs <- 12.1
#
#sex_entered <- readline(prompt = "Please enter the patient's biologic sex (m/f): ") 
sex_entered <- "m"
#
#sbp_entered <- readline(prompt = "Please enter the systolic blood pressure measurement: ")
#sbp_entered <- as.numeric(sbp_entered)
sbp_entered  <- 125
#
#dbp_entered <- readline(prompt = "Please enter the diastolic blood pressure measurement: ")
#dbp_entered <- as.numeric(dbp_entered)
dbp_entered <- 82

# Now that needed info is entered, will create functions that help with normalizing blood pressure vital signs
create_distr <- function(vs_norms_row, percents, pct_cols) {
  temp_distr <- rriskDistributions::get.norm.par(p = percents, q = as.vector(as.numeric((vs_norms_row[pct_cols]))), show.output = FALSE, plot = FALSE)
  vs_norms_row$distr_mean <- temp_distr["mean"]
  vs_norms_row$distr_sd <- temp_distr["sd"]
  return(vs_norms_row)
}


vs_normalize_fxn <- function(pt_vs_df, vs_norms_df, age_indicator) {
  i_range <- length(pt_vs_df)
  if(i_range > 0){
    for (i in c(1:i_range)) {
      if (is.na(pt_vs_df[i])) {pt_vs_df[i] <- NA}
      else{
        temp_norm <- vs_norms_df[age_indicator, ]
        pt_vs_df[i] <- pnorm(pt_vs_df[i], mean = temp_norm$distr_mean, sd = temp_norm$distr_sd) * 100
      }
    }
    return(pt_vs_df)
  }
  else{return (pt_vs_df)}
}


#Now will read in the bp norm data from the variuos sources
#CDC (NHLBI) norms will be used for those pts with a documented height who are >= 1 year of age, Lo et al wil be used for those patients without height measurements that are >= 3yo,
#for those 1-3yo without a height documented, we will use CDC and assume 50%ile for height. Lastly, for those <1 year of age will use Gemelli et al norm data

#First read the norms into respective data frames
bp_lo_boys <- readr::read_csv(file = bp_norms_lo_boys)
bp_lo_girls <- readr::read_csv(file = bp_norms_lo_girls)

bp_cdc_boys <- readr::read_csv(file = bp_norms_cdc_boys)
bp_cdc_girls <- readr::read_csv(file = bp_norms_cdc_girls)

bp_boys_under_one <- readr::read_csv(file = Gemelli_bp_under_one_boys); 
bp_boys_under_one <- bp_boys_under_one[, -1]

bp_girls_under_one <- readr::read_csv(file = Gemelli_bp_under_one_girls)
bp_girls_under_one <- bp_girls_under_one[, -1]

#Split the cdc bp df's into SBP and DBP df's
bp_cdc_boys_sbp <- dplyr::select(bp_cdc_boys, 1:9)
bp_cdc_boys_dbp <- dplyr::select(bp_cdc_boys, 1:2, 10:16)
colnames(bp_cdc_boys_sbp) <- sub("\\..*", "", colnames(bp_cdc_boys_sbp))
colnames(bp_cdc_boys_dbp) <- sub("\\..*", "", colnames(bp_cdc_boys_dbp))

bp_cdc_girls_sbp <- dplyr::select(bp_cdc_girls, 1:9)
bp_cdc_girls_dbp <- dplyr::select(bp_cdc_girls, 1:2, 10:16)
colnames(bp_cdc_girls_sbp) <- sub("\\..*", "", colnames(bp_cdc_girls_sbp))
colnames(bp_cdc_girls_dbp) <- sub("\\..*", "", colnames(bp_cdc_girls_dbp))


#Fix classes of all bp_lo df measurements, remove unecessary columns
bp_lo_boys <- bp_lo_boys %>% dplyr::select(-2, -3) %>% dplyr::mutate(SBP_mean = as.numeric(bp_lo_boys$SBP_mean))
bp_lo_girls <- bp_lo_girls %>% dplyr::select(-2, -3) %>% dplyr::mutate(SBP_mean = as.numeric(bp_lo_girls$SBP_mean))

#Load in the norms for length and weight for infants and children from CDC data sources for later normalization by %ile
length_infant_boys <- readr::read_csv(file = length_cdc_infant_boys)
length_infant_girls <- readr::read_csv(file = length_cdc_infant_girls)
length_boys <- readr::read_csv(file = length_cdc_boys)
length_girls <- readr::read_csv(file = length_cdc_girls)



# Now use same approach to normalize length and height, starting with the 3 month - 3yo age cohort
length_infant_boys$distr_mean <- NA
length_infant_boys$distr_sd <- NA
length_infant_girls$distr_mean <- NA
length_infant_girls$distr_sd <- NA
ht_inf_percents <- c(0.03, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.97)

# Loop through the rows of the length norms df's for infant boys and girls and create the mean and sd's
for (i in c(1:nrow(length_infant_boys))) {
  length_infant_boys[i, ] <- create_distr(length_infant_boys[i, ], ht_inf_percents, c(2:10))
  length_infant_girls[i, ] <- create_distr(length_infant_girls[i, ], ht_inf_percents, c(2:10))
}

# Create a df with the appropriate column names and values for normalization
vs_percentile <- data.frame(case_id = c(1), sex = c(ifelse(test = sex_entered == "m", yes = "Male", no = "Female")), age_years = c(age_entered_yrs),
                            sbp = c(sbp_entered), dbp = c(dbp_entered), height_init_cm = ifelse(test = is_ht_pres == "y", yes = ht_entered, no = NA))


# Change the height to a percentage for children under 3 years of age
if(age_entered_yrs < 3)
{

age_in_months <- age_entered_yrs * 12
df_index <- round(age_in_months, digits = 0) + 1

if (vs_percentile$sex[1] == "Male") {
  vs_percentile$height_init_cm[1] <- sapply(vs_percentile$height_init_cm, vs_normalize_fxn, length_infant_boys, df_index)
  } else if (vs_percentile$sex[1] == "Female") {
  vs_percentile$height_init_cm[1] <- sapply(vs_percentile$height_init_cm, vs_normalize_fxn, length_infant_girls, df_index)
  } else {
  print("Error, incorrect sex entered")
  }

}
  


# Repeat above process for changing heights to percentages for children > 36 months (i.e. 3 years)
length_boys$distr_mean <- NA
length_boys$distr_sd <- NA
length_girls$distr_mean <- NA
length_girls$distr_sd <- NA
ht_inf_percents <- c(0.03, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.97)

# Loop through the rows of the length norms df's for the school age boys and girls and create the mean and sd's
for (i in 1:nrow(length_boys)) {
  length_boys[i, ] <- create_distr(length_boys[i, ], ht_inf_percents, c(2:10))
  length_girls[i, ] <- create_distr(length_girls[i, ], ht_inf_percents, c(2:10))
}

# Now for a patient of age >=3 years old will change the height measurements to a percentile, do this for each age range and for both boys and girls via a for loop

if(age_entered_yrs >= 3)
{

age_in_months <- age_entered_yrs * 12
df_index <- round(age_in_months, digits = 0) - 23

if (vs_percentile$sex == "Male") 
  {
    vs_percentile$height_init_cm[1] <- sapply(vs_percentile$height_init_cm[1], vs_normalize_fxn, length_boys, df_index)
  } else if (vs_percentile$sex == "Female") {
      vs_percentile$height_init_cm[1] <- sapply(vs_percentile$height_init_cm[1], vs_normalize_fxn, length_girls, df_index)
  } else {
      print("Error, unclear biologic sex entered")
  }
    
}



# Now that we have normalized length/height, we will move onto normalizing blood pressure measurements for sbp and dbp based on age, sex, and availability of a height measurement

# Will start with the <1yo patients then move up in age. For <1yo cohort will use Gemelli et al. norm data
# Since we already have the mean and sd data, we don't need to calculate these values, 
# We simply need to get df's in the correct format for the normalize function
sbp_under_one_boys <- bp_boys_under_one %>% dplyr::mutate(distr_mean = bp_boys_under_one$SBP_mean, distr_sd = bp_boys_under_one$SBP_sd) %>% dplyr::select(age_mo, distr_mean, distr_sd)
dbp_under_one_boys <- bp_boys_under_one %>% dplyr::mutate(distr_mean = bp_boys_under_one$DBP_mean, distr_sd = bp_boys_under_one$DBP_sd) %>% dplyr::select(age_mo, distr_mean, distr_sd)

sbp_under_one_girls <- bp_girls_under_one %>% dplyr::mutate(distr_mean = bp_girls_under_one$SBP_mean, distr_sd = bp_girls_under_one$SBP_sd) %>% dplyr::select(age_mo, distr_mean, distr_sd)
dbp_under_one_girls <- bp_girls_under_one %>% dplyr::mutate(distr_mean = bp_girls_under_one$DBP_mean, distr_sd = bp_girls_under_one$DBP_sd) %>% dplyr::select(age_mo, distr_mean, distr_sd)

# Now use these Gemelli norms to convert SBP and DBP to percentiles
if(age_entered_yrs < 1)
{
  
  age_in_months <- age_entered_yrs * 12
  
  # Obtain the row index we should use when accessing the gemelli BP's using the patients age in months
  if(age_in_months >=1 & age_in_months < 3) {
    df_index <- 1
  } else if (age_in_months >=3 & age_in_months < 6) {
    df_index <- 2
  } else if (age_in_months >=6 & age_in_months < 9) {
    df_index <- 3
  } else if (age_in_months >=9 & age_in_months < 12) {
    df_index <- 4
  } else {
    print("error, age out of range")
  }
  
  #Normalize sbp & dbp for males
  if(vs_percentile$sex == "Male"){
  vs_percentile$sbp[1] <- sapply(vs_percentile$sbp[1], vs_normalize_fxn, sbp_under_one_boys, df_index)
  vs_percentile$dbp[1] <- sapply(vs_percentile$dbp[1], vs_normalize_fxn, dbp_under_one_boys, df_index)
  
  #Normalize sbp & dbp for females
  } else if (vs_percentile$sex == "Female"){
    vs_percentile$sbp[1] <- sapply(vs_percentile$sbp[1], vs_normalize_fxn, sbp_under_one_girls, df_index)
    vs_percentile$dbp[1] <- sapply(vs_percentile$dbp[1], vs_normalize_fxn, dbp_under_one_girls, df_index)
  } else {
    print("error, invalid sex")
  }

}
  
#Now we will move on to those patients without height measurements who are > 3 yo, and use norm data from Lo et al to normalize these SBP and DBPs, via a similar approach to above
sbp_lo_boys <- bp_lo_boys %>% dplyr::mutate(distr_mean = bp_lo_boys$SBP_mean, distr_sd = bp_lo_boys$SBP_sd) %>% dplyr::select(age_yr, distr_mean, distr_sd)
dbp_lo_boys <- bp_lo_boys %>% dplyr::mutate(distr_mean = bp_lo_boys$DBP_mean, distr_sd = bp_lo_boys$DBP_sd) %>% dplyr::select(age_yr, distr_mean, distr_sd)

sbp_lo_girls <- bp_lo_girls %>% dplyr::mutate(distr_mean = bp_lo_girls$SBP_mean, distr_sd = bp_lo_girls$SBP_sd) %>% dplyr::select(age_yr, distr_mean, distr_sd)
dbp_lo_girls <- bp_lo_girls %>% dplyr::mutate(distr_mean = bp_lo_girls$DBP_mean, distr_sd = bp_lo_girls$DBP_sd) %>% dplyr::select(age_yr, distr_mean, distr_sd)


# Normalize the SBP and DBP using the Lo et al norms for kids >=3 without a known height

if (age_entered_yrs >= 3 & is_ht_pres == "n")
{

  df_index <- floor(vs_percentile$age_years[1]) - 2

  if (vs_percentile$sex[1] == "Male") {
    vs_percentile$sbp[1] <- sapply(vs_percentile$sbp[1], vs_normalize_fxn, sbp_lo_boys , df_index)
    vs_percentile$dbp[1] <- sapply(vs_percentile$dbp[1], vs_normalize_fxn, dbp_lo_boys , df_index)
    
  } else if (vs_percentile$sex[1] == "Female") {
    vs_percentile$sbp[1] <- sapply(vs_percentile$sbp[1], vs_normalize_fxn, sbp_lo_girls , df_index)
    vs_percentile$dbp[1] <- sapply(vs_percentile$dbp[1], vs_normalize_fxn, dbp_lo_girls , df_index)
  } else {
    print("error, invalid sex provided")
  }

}


# For patients >=1yo and <3yo without height measurements, we will use CDC norm data and assume their height is the 50th percentile based on their age
# First split up the CDC bp df into SBP and DBP columns and fix the column names
cdc_sbp_boys <- bp_cdc_boys %>% dplyr::select(1:9)
cdc_dbp_boys <- bp_cdc_boys %>% dplyr::select(1, 2, 10:16)

cdc_sbp_girls <- bp_cdc_girls %>% dplyr::select(1:9)
cdc_dbp_girls <- bp_cdc_girls %>% dplyr::select(1, 2, 10:16)

# Fix the column names of the diastolic bp's (e.g. change column name from 5...3 to 5)
cdc_sbp_boys <- cdc_sbp_boys %>%  dplyr::rename('5' = '5...3', '10' = '10...4', '25' = '25...5', '50' = '50...6', '75' = '75...7', '90' = '90...8', '95' = '95...9')
cdc_sbp_girls <- cdc_sbp_girls %>%  dplyr::rename('5' = '5...3', '10' = '10...4', '25' = '25...5', '50' = '50...6', '75' = '75...7', '90' = '90...8', '95' = '95...9')

cdc_dbp_boys <- cdc_dbp_boys %>%  dplyr::rename('5' = '5...10', '10' = '10...11', '25' = '25...12', '50' = '50...13', '75' = '75...14', '90' = '90...15', '95' = '95...16')
cdc_dbp_girls <- cdc_dbp_girls %>%  dplyr::rename('5' = '5...10', '10' = '10...11', '25' = '25...12', '50' = '50...13', '75' = '75...14', '90' = '90...15', '95' = '95...16')


# Now use gather and spread to make the bp%iles the columns instead of height %iles
cdc_sbp_boys <- cdc_sbp_boys %>% tidyr::gather('height_%', bp_value, '5':'95') %>% tidyr::spread('bp_%', bp_value)
cdc_sbp_girls <- cdc_sbp_girls %>% tidyr::gather('height_%', bp_value, '5':'95') %>% tidyr::spread('bp_%', bp_value)
cdc_dbp_boys <- cdc_dbp_boys %>% tidyr::gather('height_%', bp_value, '5':'95') %>% tidyr::spread('bp_%', bp_value)
cdc_dbp_girls <- cdc_dbp_girls %>% tidyr::gather('height_%', bp_value, '5':'95') %>% tidyr::spread('bp_%', bp_value)

# Now loop through each df to create the mean and sd for each age/height% pair
cdc_sbp_boys$distr_mean <- NA; cdc_sbp_boys$distr_sd <- NA
cdc_dbp_boys$distr_mean <- NA; cdc_dbp_boys$distr_sd <- NA
cdc_sbp_girls$distr_mean <- NA; cdc_sbp_girls$distr_sd <- NA
cdc_dbp_girls$distr_mean <- NA; cdc_dbp_girls$distr_sd <- NA

cdc_bp_percents <- c(0.5, 0.9, 0.95, 0.99)

for (i in c(1:nrow(cdc_sbp_boys))) {
  cdc_sbp_boys[i, ] <- create_distr(cdc_sbp_boys[i, ], cdc_bp_percents, c(3:6))
  cdc_sbp_girls[i, ] <- create_distr(cdc_sbp_girls[i, ], cdc_bp_percents, c(3:6))
  cdc_dbp_boys[i, ] <- create_distr(cdc_dbp_boys[i, ], cdc_bp_percents, c(3:6))
  cdc_dbp_girls[i, ] <- create_distr(cdc_dbp_girls[i, ], cdc_bp_percents, c(3:6))
}

# Will also need a function that changes the actual height to the %ile used in the CDC table to send to the function we will define below, e.g. if pt height
# is 54% it will use the 50%ile height bp curve, if 92% then uses 90%, etc.
height_simplify <- function(pt_vs_df){
  
  for (i in 1:nrow(pt_vs_df)) {
    pt_ht <- pt_vs_df$height_init_cm[i]
    if (pt_ht < 10) {pt_vs_df$ht_simple[i] <- 5}
    else if (pt_ht >= 10 & pt_ht < 25) {pt_vs_df$ht_simple[i] <- 10}
    else if (pt_ht >= 25 & pt_ht < 50) {pt_vs_df$ht_simple[i] <- 25}
    else if (pt_ht >= 50 & pt_ht < 75) {pt_vs_df$ht_simple[i] <- 50}
    else if (pt_ht >= 75 & pt_ht < 90) {pt_vs_df$ht_simple[i] <- 75}
    else if (pt_ht >= 90 & pt_ht < 95) {pt_vs_df$ht_simple[i] <- 90}
    else {pt_vs_df$ht_simple[i] <- 95}
  }
  return(pt_vs_df)
}

# In order to normalize all bp measurements we will define an additional vs_normalize function that includes the height %ile as an input
vs_height_normalize <- function(pt_vs_df, vs_norms_df, age_indicator, height_ind) {
  i_range <- NROW(pt_vs_df)
  if (rlang::is_empty(pt_vs_df)) { }
  else {
    for (i in 1:i_range) {
      if (is.na(pt_vs_df[i])) {pt_vs_df[i] <- NA}
      else{
        temp_norm <- vs_norms_df[vs_norms_df$'height_%' == height_ind & vs_norms_df$age == age_indicator, ]
        pt_vs_df[i] <- pnorm(pt_vs_df[i], mean = temp_norm$distr_mean, sd = temp_norm$distr_sd) * 100
      }
    }
  }
  return(pt_vs_df)
}



# Now apply this function to the whole vs_percentile df to get a simplified height for all pts, store in columns ht_simple
vs_percentile$ht_simple <- NA

if(!is.na(vs_percentile$height_init_cm)){
vs_percentile <- height_simplify(vs_percentile)
}

# Now use the cdc bp chart for patients with heights who are >1yo and apply the bp normalize fxn
# Within this function will also normalize those >=1yo age < 3 years old without a height assuming the 50th%ile

if (age_entered_yrs >=1) {
  
  age <- floor(vs_percentile$age_years)
  
  if(is_ht_pres == "y") {
  
    #Convert male patient SBP & DBP
    if(vs_percentile$sex == "Male"){  
  
      vs_percentile$sbp[1] <- sapply(vs_percentile$sbp[1], vs_height_normalize, cdc_sbp_boys, age, vs_percentile$ht_simple)
      vs_percentile$dbp[1] <- sapply(vs_percentile$dbp[1], vs_height_normalize, cdc_dbp_boys, age, vs_percentile$ht_simple)
  
    } else if (vs_percentile$sex == "Female"){
    
      vs_percentile$sbp[1] <- sapply(vs_percentile$sbp[1], vs_height_normalize, cdc_sbp_girls, age, vs_percentile$ht_simple)
      vs_percentile$dbp[1] <- sapply(vs_percentile$dbp[1], vs_height_normalize, cdc_dbp_girls, age, vs_percentile$ht_simple)
    
    } else {
      print("error, invalid sex supplied")
    }

  } else if (is_ht_pres == "n" & age_entered_yrs < 3) {
    
    #Convert male patient SBP & DBP
    if(vs_percentile$sex == "Male"){  
      
      vs_percentile$sbp[1] <- sapply(vs_percentile$sbp[1], vs_height_normalize, cdc_sbp_boys, age, 50)
      vs_percentile$dbp[1] <- sapply(vs_percentile$dbp[1], vs_height_normalize, cdc_dbp_boys, age, 50)
      
    } else if (vs_percentile$sex == "Female"){
      
      vs_percentile$sbp[1] <- sapply(vs_percentile$sbp[1], vs_height_normalize, cdc_sbp_girls, age, 50)
      vs_percentile$dbp[1] <- sapply(vs_percentile$dbp[1], vs_height_normalize, cdc_dbp_girls, age, 50)
      
    } else {
      print("error, invalid sex supplied")
    }
  
        
  } else {
    print("error, invalid height yes/no answer supplied")
  }
  
}

###### Print out the SBP and DBP percentile results
paste0("The systolic blood pressure percentile is: ", round(vs_percentile$sbp[1], digits = 2))
paste0("The diastolic blood pressure percentile is: ", round(vs_percentile$dbp[1], digits = 2))
if(is_ht_pres == "y"){
  paste0("The height percentile is: ", round(vs_percentile$height_init_cm[1], digits = 2))
} else{
  "As height was not available, a height percentile of 50% was assumed"
}

############################################################  Additional code to create the plots for the BP norms paper ############################################################################## 
# Ultimate lines will need to have to plot. Boys and girls, SBP and DBP. The different percentiles for 5, 10, 
# 25, 50, 75, 90, 95, plot using 50th%ile height?


# ages we have are months 1, 3, 6, 9, initialize tibbles for boys and girls sbp and dbp (total of 4 section)
under_1_b_sbp_5  <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_b_sbp_10 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_b_sbp_25 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_b_sbp_50 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_b_sbp_75 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_b_sbp_90 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_b_sbp_95 <- tibble::tibble(age_mo = c(1, 3, 6, 9))

under_1_b_dbp_5  <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_b_dbp_10 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_b_dbp_25 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_b_dbp_50 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_b_dbp_75 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_b_dbp_90 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_b_dbp_95 <- tibble::tibble(age_mo = c(1, 3, 6, 9))

#Girls
under_1_g_sbp_5  <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_g_sbp_10 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_g_sbp_25 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_g_sbp_50 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_g_sbp_75 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_g_sbp_90 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_g_sbp_95 <- tibble::tibble(age_mo = c(1, 3, 6, 9))

under_1_g_dbp_5  <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_g_dbp_10 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_g_dbp_25 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_g_dbp_50 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_g_dbp_75 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_g_dbp_90 <- tibble::tibble(age_mo = c(1, 3, 6, 9))
under_1_g_dbp_95 <- tibble::tibble(age_mo = c(1, 3, 6, 9))


# Loop through the 4 different month ages and find the correct percentile values for each sbp/dbp and boy/girl combination
#Boys SBP
for(i in c(1:4)){
  under_1_b_sbp_5$sbp[i] <- qnorm(0.05, mean = bp_boys_under_one$SBP_mean[i], bp_boys_under_one$SBP_sd[i])
  under_1_b_sbp_10$sbp[i] <- qnorm(0.10, mean = bp_boys_under_one$SBP_mean[i], bp_boys_under_one$SBP_sd[i])
  under_1_b_sbp_25$sbp[i] <- qnorm(0.25, mean = bp_boys_under_one$SBP_mean[i], bp_boys_under_one$SBP_sd[i])
  under_1_b_sbp_50$sbp[i] <- qnorm(0.50, mean = bp_boys_under_one$SBP_mean[i], bp_boys_under_one$SBP_sd[i])
  under_1_b_sbp_75$sbp[i] <- qnorm(0.75, mean = bp_boys_under_one$SBP_mean[i], bp_boys_under_one$SBP_sd[i])
  under_1_b_sbp_90$sbp[i] <- qnorm(0.90, mean = bp_boys_under_one$SBP_mean[i], bp_boys_under_one$SBP_sd[i])
  under_1_b_sbp_95$sbp[i] <- qnorm(0.95, mean = bp_boys_under_one$SBP_mean[i], bp_boys_under_one$SBP_sd[i])
}

#Boys DBP
for(i in c(1:4)){
  under_1_b_dbp_5$dbp[i] <- qnorm(0.05, mean = bp_boys_under_one$DBP_mean[i], bp_boys_under_one$DBP_sd[i])
  under_1_b_dbp_10$dbp[i] <- qnorm(0.10, mean = bp_boys_under_one$DBP_mean[i], bp_boys_under_one$DBP_sd[i])
  under_1_b_dbp_25$dbp[i] <- qnorm(0.25, mean = bp_boys_under_one$DBP_mean[i], bp_boys_under_one$DBP_sd[i])
  under_1_b_dbp_50$dbp[i] <- qnorm(0.50, mean = bp_boys_under_one$DBP_mean[i], bp_boys_under_one$DBP_sd[i])
  under_1_b_dbp_75$dbp[i] <- qnorm(0.75, mean = bp_boys_under_one$DBP_mean[i], bp_boys_under_one$DBP_sd[i])
  under_1_b_dbp_90$dbp[i] <- qnorm(0.90, mean = bp_boys_under_one$DBP_mean[i], bp_boys_under_one$DBP_sd[i])
  under_1_b_dbp_95$dbp[i] <- qnorm(0.95, mean = bp_boys_under_one$DBP_mean[i], bp_boys_under_one$DBP_sd[i])
}

#Girls SBP
for(i in c(1:4)){
  under_1_g_sbp_5$sbp[i] <- qnorm(0.05, mean = bp_girls_under_one$SBP_mean[i], bp_girls_under_one$SBP_sd[i])
  under_1_g_sbp_10$sbp[i] <- qnorm(0.10, mean = bp_girls_under_one$SBP_mean[i], bp_girls_under_one$SBP_sd[i])
  under_1_g_sbp_25$sbp[i] <- qnorm(0.25, mean = bp_girls_under_one$SBP_mean[i], bp_girls_under_one$SBP_sd[i])
  under_1_g_sbp_50$sbp[i] <- qnorm(0.50, mean = bp_girls_under_one$SBP_mean[i], bp_girls_under_one$SBP_sd[i])
  under_1_g_sbp_75$sbp[i] <- qnorm(0.75, mean = bp_girls_under_one$SBP_mean[i], bp_girls_under_one$SBP_sd[i])
  under_1_g_sbp_90$sbp[i] <- qnorm(0.90, mean = bp_girls_under_one$SBP_mean[i], bp_girls_under_one$SBP_sd[i])
  under_1_g_sbp_95$sbp[i] <- qnorm(0.95, mean = bp_girls_under_one$SBP_mean[i], bp_girls_under_one$SBP_sd[i])
}

#Girls DBP
for(i in c(1:4)){
  under_1_g_dbp_5$dbp[i] <- qnorm(0.05, mean = bp_girls_under_one$DBP_mean[i], bp_girls_under_one$DBP_sd[i])
  under_1_g_dbp_10$dbp[i] <- qnorm(0.10, mean = bp_girls_under_one$DBP_mean[i], bp_girls_under_one$DBP_sd[i])
  under_1_g_dbp_25$dbp[i] <- qnorm(0.25, mean = bp_girls_under_one$DBP_mean[i], bp_girls_under_one$DBP_sd[i])
  under_1_g_dbp_50$dbp[i] <- qnorm(0.50, mean = bp_girls_under_one$DBP_mean[i], bp_girls_under_one$DBP_sd[i])
  under_1_g_dbp_75$dbp[i] <- qnorm(0.75, mean = bp_girls_under_one$DBP_mean[i], bp_girls_under_one$DBP_sd[i])
  under_1_g_dbp_90$dbp[i] <- qnorm(0.90, mean = bp_girls_under_one$DBP_mean[i], bp_girls_under_one$DBP_sd[i])
  under_1_g_dbp_95$dbp[i] <- qnorm(0.95, mean = bp_girls_under_one$DBP_mean[i], bp_girls_under_one$DBP_sd[i])
}

####################################################################
# Now find the CDC/NHLBI BP's for the 50th %ile for kids 1yo+
# Boys SBP, using 50th%ile height
cdc_kids_50h_b_sbp_5  <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_b_sbp_10 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_b_sbp_25 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_b_sbp_50 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_b_sbp_75 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_b_sbp_90 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_b_sbp_95 <- tibble::tibble(age_mo = 12*c(1:17))

# Boys DBP, using 50th%ile height
cdc_kids_50h_b_dbp_5  <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_b_dbp_10 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_b_dbp_25 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_b_dbp_50 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_b_dbp_75 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_b_dbp_90 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_b_dbp_95 <- tibble::tibble(age_mo = 12*c(1:17))

# Repeat SBP and DBP for girls, again using 50th%ile height
# Girls SBP, using 50th%ile height
cdc_kids_50h_g_sbp_5  <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_g_sbp_10 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_g_sbp_25 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_g_sbp_50 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_g_sbp_75 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_g_sbp_90 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_g_sbp_95 <- tibble::tibble(age_mo = 12*c(1:17))


# Girls DBP, using 50th%ile height
cdc_kids_50h_g_dbp_5  <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_g_dbp_10 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_g_dbp_25 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_g_dbp_50 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_g_dbp_75 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_g_dbp_90 <- tibble::tibble(age_mo = 12*c(1:17))
cdc_kids_50h_g_dbp_95 <- tibble::tibble(age_mo = 12*c(1:17))

# For loop to find and save all of the different percentile BP's for boys and girls for SBP and DBP
# First create versions of the cdc/nhlbi df's with only the 50th%ile for height
cdc_50_sbp_boys  <- cdc_sbp_boys %>% dplyr::filter(`height_%` == "50")
cdc_50_dbp_boys  <- cdc_dbp_boys %>% dplyr::filter(`height_%` == "50")
cdc_50_sbp_girls <- cdc_sbp_girls %>% dplyr::filter(`height_%` == "50")
cdc_50_dbp_girls <- cdc_dbp_girls %>% dplyr::filter(`height_%` == "50")

for(i in c(1:17)){
  cdc_kids_50h_b_sbp_5$sbp[i] <-  qnorm(0.05, mean = cdc_50_sbp_boys$distr_mean[i], cdc_50_sbp_boys$distr_sd[i])
  cdc_kids_50h_b_sbp_10$sbp[i] <- qnorm(0.10, mean = cdc_50_sbp_boys$distr_mean[i], cdc_50_sbp_boys$distr_sd[i])
  cdc_kids_50h_b_sbp_25$sbp[i] <- qnorm(0.25, mean = cdc_50_sbp_boys$distr_mean[i], cdc_50_sbp_boys$distr_sd[i])
  cdc_kids_50h_b_sbp_50$sbp[i] <- qnorm(0.50, mean = cdc_50_sbp_boys$distr_mean[i], cdc_50_sbp_boys$distr_sd[i])
  cdc_kids_50h_b_sbp_75$sbp[i] <- qnorm(0.75, mean = cdc_50_sbp_boys$distr_mean[i], cdc_50_sbp_boys$distr_sd[i])
  cdc_kids_50h_b_sbp_90$sbp[i] <- qnorm(0.90, mean = cdc_50_sbp_boys$distr_mean[i], cdc_50_sbp_boys$distr_sd[i])
  cdc_kids_50h_b_sbp_95$sbp[i] <- qnorm(0.95, mean = cdc_50_sbp_boys$distr_mean[i], cdc_50_sbp_boys$distr_sd[i])
  
  cdc_kids_50h_b_dbp_5$dbp[i]  <- qnorm(0.05, mean = cdc_50_dbp_boys$distr_mean[i], cdc_50_dbp_boys$distr_sd[i])
  cdc_kids_50h_b_dbp_10$dbp[i] <- qnorm(0.10, mean = cdc_50_dbp_boys$distr_mean[i], cdc_50_dbp_boys$distr_sd[i])
  cdc_kids_50h_b_dbp_25$dbp[i] <- qnorm(0.25, mean = cdc_50_dbp_boys$distr_mean[i], cdc_50_dbp_boys$distr_sd[i])
  cdc_kids_50h_b_dbp_50$dbp[i] <- qnorm(0.50, mean = cdc_50_dbp_boys$distr_mean[i], cdc_50_dbp_boys$distr_sd[i])
  cdc_kids_50h_b_dbp_75$dbp[i] <- qnorm(0.75, mean = cdc_50_dbp_boys$distr_mean[i], cdc_50_dbp_boys$distr_sd[i])
  cdc_kids_50h_b_dbp_90$dbp[i] <- qnorm(0.90, mean = cdc_50_dbp_boys$distr_mean[i], cdc_50_dbp_boys$distr_sd[i])
  cdc_kids_50h_b_dbp_95$dbp[i] <- qnorm(0.95, mean = cdc_50_dbp_boys$distr_mean[i], cdc_50_dbp_boys$distr_sd[i])
  
  cdc_kids_50h_g_sbp_5$sbp[i] <-  qnorm(0.05, mean = cdc_50_sbp_girls$distr_mean[i], cdc_50_sbp_girls$distr_sd[i])
  cdc_kids_50h_g_sbp_10$sbp[i] <- qnorm(0.10, mean = cdc_50_sbp_girls$distr_mean[i], cdc_50_sbp_girls$distr_sd[i])
  cdc_kids_50h_g_sbp_25$sbp[i] <- qnorm(0.25, mean = cdc_50_sbp_girls$distr_mean[i], cdc_50_sbp_girls$distr_sd[i])
  cdc_kids_50h_g_sbp_50$sbp[i] <- qnorm(0.50, mean = cdc_50_sbp_girls$distr_mean[i], cdc_50_sbp_girls$distr_sd[i])
  cdc_kids_50h_g_sbp_75$sbp[i] <- qnorm(0.75, mean = cdc_50_sbp_girls$distr_mean[i], cdc_50_sbp_girls$distr_sd[i])
  cdc_kids_50h_g_sbp_90$sbp[i] <- qnorm(0.90, mean = cdc_50_sbp_girls$distr_mean[i], cdc_50_sbp_girls$distr_sd[i])
  cdc_kids_50h_g_sbp_95$sbp[i] <- qnorm(0.95, mean = cdc_50_sbp_girls$distr_mean[i], cdc_50_sbp_girls$distr_sd[i])
  
  cdc_kids_50h_g_dbp_5$dbp[i] <-  qnorm(0.05, mean = cdc_50_dbp_girls$distr_mean[i], cdc_50_dbp_girls$distr_sd[i])
  cdc_kids_50h_g_dbp_10$dbp[i] <- qnorm(0.10, mean = cdc_50_dbp_girls$distr_mean[i], cdc_50_dbp_girls$distr_sd[i])
  cdc_kids_50h_g_dbp_25$dbp[i] <- qnorm(0.25, mean = cdc_50_dbp_girls$distr_mean[i], cdc_50_dbp_girls$distr_sd[i])
  cdc_kids_50h_g_dbp_50$dbp[i] <- qnorm(0.50, mean = cdc_50_dbp_girls$distr_mean[i], cdc_50_dbp_girls$distr_sd[i])
  cdc_kids_50h_g_dbp_75$dbp[i] <- qnorm(0.75, mean = cdc_50_dbp_girls$distr_mean[i], cdc_50_dbp_girls$distr_sd[i])
  cdc_kids_50h_g_dbp_90$dbp[i] <- qnorm(0.90, mean = cdc_50_dbp_girls$distr_mean[i], cdc_50_dbp_girls$distr_sd[i])
  cdc_kids_50h_g_dbp_95$dbp[i] <- qnorm(0.95, mean = cdc_50_dbp_girls$distr_mean[i], cdc_50_dbp_girls$distr_sd[i])
}

################################################################################################################################
# Now will create plots for the different BP percentiles frmo Lo et al (those >=3yo without a height available)

# Boys SBP
lo_kids_b_sbp_5  <- tibble::tibble(age_mo =  12*c(3:17))
lo_kids_b_sbp_10 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_b_sbp_25 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_b_sbp_50 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_b_sbp_75 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_b_sbp_90 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_b_sbp_95 <- tibble::tibble(age_mo = 12*c(3:17))

# Boys DBP
lo_kids_b_dbp_5  <- tibble::tibble(age_mo =  12*c(3:17))
lo_kids_b_dbp_10 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_b_dbp_25 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_b_dbp_50 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_b_dbp_75 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_b_dbp_90 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_b_dbp_95 <- tibble::tibble(age_mo = 12*c(3:17))

# Girls SBP
lo_kids_g_sbp_5  <- tibble::tibble(age_mo =  12*c(3:17))
lo_kids_g_sbp_10 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_g_sbp_25 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_g_sbp_50 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_g_sbp_75 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_g_sbp_90 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_g_sbp_95 <- tibble::tibble(age_mo = 12*c(3:17))

# Girls DBP
lo_kids_g_dbp_5  <- tibble::tibble(age_mo =  12*c(3:17))
lo_kids_g_dbp_10 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_g_dbp_25 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_g_dbp_50 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_g_dbp_75 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_g_dbp_90 <- tibble::tibble(age_mo = 12*c(3:17))
lo_kids_g_dbp_95 <- tibble::tibble(age_mo = 12*c(3:17))

# Now loop through and obtain the percentiles for each group
for(i in c(1:15)){
  lo_kids_b_sbp_5$sbp[i] <-  qnorm(0.05, mean = bp_lo_boys$SBP_mean[i], bp_lo_boys$SBP_sd[i])
  lo_kids_b_sbp_10$sbp[i] <- qnorm(0.10, mean = bp_lo_boys$SBP_mean[i], bp_lo_boys$SBP_sd[i])
  lo_kids_b_sbp_25$sbp[i] <- qnorm(0.25, mean = bp_lo_boys$SBP_mean[i], bp_lo_boys$SBP_sd[i])
  lo_kids_b_sbp_50$sbp[i] <- qnorm(0.50, mean = bp_lo_boys$SBP_mean[i], bp_lo_boys$SBP_sd[i])
  lo_kids_b_sbp_75$sbp[i] <- qnorm(0.75, mean = bp_lo_boys$SBP_mean[i], bp_lo_boys$SBP_sd[i])
  lo_kids_b_sbp_90$sbp[i] <- qnorm(0.90, mean = bp_lo_boys$SBP_mean[i], bp_lo_boys$SBP_sd[i])
  lo_kids_b_sbp_95$sbp[i] <- qnorm(0.95, mean = bp_lo_boys$SBP_mean[i], bp_lo_boys$SBP_sd[i])
  
  lo_kids_b_dbp_5$dbp[i] <-  qnorm(0.05, mean = bp_lo_boys$DBP_mean[i], bp_lo_boys$DBP_sd[i])
  lo_kids_b_dbp_10$dbp[i] <- qnorm(0.10, mean = bp_lo_boys$DBP_mean[i], bp_lo_boys$DBP_sd[i])
  lo_kids_b_dbp_25$dbp[i] <- qnorm(0.25, mean = bp_lo_boys$DBP_mean[i], bp_lo_boys$DBP_sd[i])
  lo_kids_b_dbp_50$dbp[i] <- qnorm(0.50, mean = bp_lo_boys$DBP_mean[i], bp_lo_boys$DBP_sd[i])
  lo_kids_b_dbp_75$dbp[i] <- qnorm(0.75, mean = bp_lo_boys$DBP_mean[i], bp_lo_boys$DBP_sd[i])
  lo_kids_b_dbp_90$dbp[i] <- qnorm(0.90, mean = bp_lo_boys$DBP_mean[i], bp_lo_boys$DBP_sd[i])
  lo_kids_b_dbp_95$dbp[i] <- qnorm(0.95, mean = bp_lo_boys$DBP_mean[i], bp_lo_boys$DBP_sd[i])
  
  lo_kids_g_sbp_5$sbp[i] <-  qnorm(0.05, mean = bp_lo_girls$SBP_mean[i], bp_lo_girls$SBP_sd[i])
  lo_kids_g_sbp_10$sbp[i] <- qnorm(0.10, mean = bp_lo_girls$SBP_mean[i], bp_lo_girls$SBP_sd[i])
  lo_kids_g_sbp_25$sbp[i] <- qnorm(0.25, mean = bp_lo_girls$SBP_mean[i], bp_lo_girls$SBP_sd[i])
  lo_kids_g_sbp_50$sbp[i] <- qnorm(0.50, mean = bp_lo_girls$SBP_mean[i], bp_lo_girls$SBP_sd[i])
  lo_kids_g_sbp_75$sbp[i] <- qnorm(0.75, mean = bp_lo_girls$SBP_mean[i], bp_lo_girls$SBP_sd[i])
  lo_kids_g_sbp_90$sbp[i] <- qnorm(0.90, mean = bp_lo_girls$SBP_mean[i], bp_lo_girls$SBP_sd[i])
  lo_kids_g_sbp_95$sbp[i] <- qnorm(0.95, mean = bp_lo_girls$SBP_mean[i], bp_lo_girls$SBP_sd[i])
  
  lo_kids_g_dbp_5$dbp[i] <-  qnorm(0.05, mean = bp_lo_girls$DBP_mean[i], bp_lo_girls$DBP_sd[i])
  lo_kids_g_dbp_10$dbp[i] <- qnorm(0.10, mean = bp_lo_girls$DBP_mean[i], bp_lo_girls$DBP_sd[i])
  lo_kids_g_dbp_25$dbp[i] <- qnorm(0.25, mean = bp_lo_girls$DBP_mean[i], bp_lo_girls$DBP_sd[i])
  lo_kids_g_dbp_50$dbp[i] <- qnorm(0.50, mean = bp_lo_girls$DBP_mean[i], bp_lo_girls$DBP_sd[i])
  lo_kids_g_dbp_75$dbp[i] <- qnorm(0.75, mean = bp_lo_girls$DBP_mean[i], bp_lo_girls$DBP_sd[i])
  lo_kids_g_dbp_90$dbp[i] <- qnorm(0.90, mean = bp_lo_girls$DBP_mean[i], bp_lo_girls$DBP_sd[i])
  lo_kids_g_dbp_95$dbp[i] <- qnorm(0.95, mean = bp_lo_girls$DBP_mean[i], bp_lo_girls$DBP_sd[i])
}

# Now create the 5%, 10%, 25%, etc df's for plotting, using prior df's such as:
# lo_kids_b__sbp_5, under_1_b_sbp_5, and cdc_kids_50h_b_sbp_5

# First unlist and turn to numeric the values in cdc_kids... df's
cdc_kids_50h_b_sbp_5  <- cdc_kids_50h_b_sbp_5  %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_b_sbp_5$sbp)))
cdc_kids_50h_b_sbp_10 <- cdc_kids_50h_b_sbp_10 %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_b_sbp_10$sbp)))
cdc_kids_50h_b_sbp_25 <- cdc_kids_50h_b_sbp_25 %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_b_sbp_25$sbp)))
cdc_kids_50h_b_sbp_50 <- cdc_kids_50h_b_sbp_50 %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_b_sbp_50$sbp)))
cdc_kids_50h_b_sbp_75 <- cdc_kids_50h_b_sbp_75 %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_b_sbp_75$sbp)))
cdc_kids_50h_b_sbp_90 <- cdc_kids_50h_b_sbp_90 %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_b_sbp_90$sbp)))
cdc_kids_50h_b_sbp_95 <- cdc_kids_50h_b_sbp_95 %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_b_sbp_95$sbp)))

cdc_kids_50h_b_dbp_5  <- cdc_kids_50h_b_dbp_5  %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_b_dbp_5$dbp)))
cdc_kids_50h_b_dbp_10 <- cdc_kids_50h_b_dbp_10 %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_b_dbp_10$dbp)))
cdc_kids_50h_b_dbp_25 <- cdc_kids_50h_b_dbp_25 %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_b_dbp_25$dbp)))
cdc_kids_50h_b_dbp_50 <- cdc_kids_50h_b_dbp_50 %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_b_dbp_50$dbp)))
cdc_kids_50h_b_dbp_75 <- cdc_kids_50h_b_dbp_75 %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_b_dbp_75$dbp)))
cdc_kids_50h_b_dbp_90 <- cdc_kids_50h_b_dbp_90 %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_b_dbp_90$dbp)))
cdc_kids_50h_b_dbp_95 <- cdc_kids_50h_b_dbp_95 %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_b_dbp_95$dbp)))

# Repeat for girls values
cdc_kids_50h_g_sbp_5  <- cdc_kids_50h_g_sbp_5  %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_g_sbp_5$sbp)))
cdc_kids_50h_g_sbp_10 <- cdc_kids_50h_g_sbp_10 %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_g_sbp_10$sbp)))
cdc_kids_50h_g_sbp_25 <- cdc_kids_50h_g_sbp_25 %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_g_sbp_25$sbp)))
cdc_kids_50h_g_sbp_50 <- cdc_kids_50h_g_sbp_50 %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_g_sbp_50$sbp)))
cdc_kids_50h_g_sbp_75 <- cdc_kids_50h_g_sbp_75 %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_g_sbp_75$sbp)))
cdc_kids_50h_g_sbp_90 <- cdc_kids_50h_g_sbp_90 %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_g_sbp_90$sbp)))
cdc_kids_50h_g_sbp_95 <- cdc_kids_50h_g_sbp_95 %>% dplyr::mutate(sbp = as.numeric(unlist(cdc_kids_50h_g_sbp_95$sbp)))

cdc_kids_50h_g_dbp_5  <- cdc_kids_50h_g_dbp_5  %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_g_dbp_5$dbp)))
cdc_kids_50h_g_dbp_10 <- cdc_kids_50h_g_dbp_10 %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_g_dbp_10$dbp)))
cdc_kids_50h_g_dbp_25 <- cdc_kids_50h_g_dbp_25 %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_g_dbp_25$dbp)))
cdc_kids_50h_g_dbp_50 <- cdc_kids_50h_g_dbp_50 %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_g_dbp_50$dbp)))
cdc_kids_50h_g_dbp_75 <- cdc_kids_50h_g_dbp_75 %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_g_dbp_75$dbp)))
cdc_kids_50h_g_dbp_90 <- cdc_kids_50h_g_dbp_90 %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_g_dbp_90$dbp)))
cdc_kids_50h_g_dbp_95 <- cdc_kids_50h_g_dbp_95 %>% dplyr::mutate(dbp = as.numeric(unlist(cdc_kids_50h_g_dbp_95$dbp)))



# Now link together the different percentiles datasets
no_ht_b_sbp_5  <- dplyr::bind_rows(under_1_b_sbp_5, (cdc_kids_50h_b_sbp_5 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_sbp_5) %>% dplyr::mutate(group = "5% ")
no_ht_b_sbp_10 <- dplyr::bind_rows(under_1_b_sbp_10, (cdc_kids_50h_b_sbp_10 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_sbp_10) %>% dplyr::mutate(group = "10% ")
no_ht_b_sbp_25 <- dplyr::bind_rows(under_1_b_sbp_25, (cdc_kids_50h_b_sbp_25 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_sbp_25)  %>% dplyr::mutate(group = "25% ")
no_ht_b_sbp_50 <- dplyr::bind_rows(under_1_b_sbp_50, (cdc_kids_50h_b_sbp_50 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_sbp_50) %>% dplyr::mutate(group = "50% ")
no_ht_b_sbp_75 <- dplyr::bind_rows(under_1_b_sbp_75, (cdc_kids_50h_b_sbp_75 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_sbp_75) %>% dplyr::mutate(group = "75% ")
no_ht_b_sbp_90 <- dplyr::bind_rows(under_1_b_sbp_90, (cdc_kids_50h_b_sbp_90 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_sbp_90) %>% dplyr::mutate(group = "90% ")
no_ht_b_sbp_95 <- dplyr::bind_rows(under_1_b_sbp_95, (cdc_kids_50h_b_sbp_95 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_sbp_95) %>% dplyr::mutate(group = "95% ")

no_ht_all_b_sbp <- dplyr::bind_rows(no_ht_b_sbp_5, no_ht_b_sbp_10, no_ht_b_sbp_25, no_ht_b_sbp_50, no_ht_b_sbp_75, no_ht_b_sbp_90, no_ht_b_sbp_95)
no_ht_all_b_sbp$label <- NA
no_ht_all_b_sbp$label[which(no_ht_all_b_sbp$age_mo == max(no_ht_all_b_sbp$age_mo))] <- 
  no_ht_all_b_sbp$group[which(no_ht_all_b_sbp$age_mo == max(no_ht_all_b_sbp$age_mo))]

# Repeat for boys DBP
no_ht_b_dbp_5  <- dplyr::bind_rows(under_1_b_dbp_5, (cdc_kids_50h_b_dbp_5 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_dbp_5) %>% dplyr::mutate(group = "5%")
no_ht_b_dbp_10 <- dplyr::bind_rows(under_1_b_dbp_10, (cdc_kids_50h_b_dbp_10 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_dbp_10) %>% dplyr::mutate(group = "10%")
no_ht_b_dbp_25 <- dplyr::bind_rows(under_1_b_dbp_25, (cdc_kids_50h_b_dbp_25 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_dbp_25)  %>% dplyr::mutate(group = "25%")
no_ht_b_dbp_50 <- dplyr::bind_rows(under_1_b_dbp_50, (cdc_kids_50h_b_dbp_50 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_dbp_50) %>% dplyr::mutate(group = "50%")
no_ht_b_dbp_75 <- dplyr::bind_rows(under_1_b_dbp_75, (cdc_kids_50h_b_dbp_75 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_dbp_75) %>% dplyr::mutate(group = "75%")
no_ht_b_dbp_90 <- dplyr::bind_rows(under_1_b_dbp_90, (cdc_kids_50h_b_dbp_90 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_dbp_90) %>% dplyr::mutate(group = "90%")
no_ht_b_dbp_95 <- dplyr::bind_rows(under_1_b_dbp_95, (cdc_kids_50h_b_dbp_95 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_b_dbp_95) %>% dplyr::mutate(group = "95%")

no_ht_all_b_dbp <- dplyr::bind_rows(no_ht_b_dbp_5, no_ht_b_dbp_10, no_ht_b_dbp_25, no_ht_b_dbp_50, no_ht_b_dbp_75, no_ht_b_dbp_90, no_ht_b_dbp_95)
no_ht_all_b_dbp$label <- NA
no_ht_all_b_dbp$label[which(no_ht_all_b_dbp$age_mo == max(no_ht_all_b_dbp$age_mo))] <- 
  no_ht_all_b_dbp$group[which(no_ht_all_b_dbp$age_mo == max(no_ht_all_b_dbp$age_mo))]

no_ht_all_b_both <- dplyr::bind_rows(no_ht_all_b_sbp, no_ht_all_b_dbp %>% dplyr::rename(sbp = dbp)) %>% dplyr::rename(bp = sbp, age = age_mo) %>% dplyr::mutate(age = age/12) 

# Create ggplot
no_ht_b_plot <- ggplot2::ggplot(no_ht_all_b_both, ggplot2::aes(x = age, y = bp, col = group)) + ggplot2::geom_smooth(se = FALSE) +
  ggrepel::geom_label_repel(ggplot2::aes(label = label), size = 3, nudge_x = 1, nudge_y = 1, na.rm = TRUE) + ggplot2::labs(title = "Blood Pressure Percentiles: Boys", tag = 'A', y = "Blood Pressure (mmHg)", 
                                                                                                x = "Age (yr)") + ggplot2::scale_y_continuous(breaks=c(30,60, 90, 120), limits = c(28, 140)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold"), axis.text = ggplot2::element_text(size = 14, face = "bold"), axis.title = ggplot2::element_text(size = 16, face = "bold"), 
        legend.position = "none") + ggplot2::scale_x_continuous(breaks=c(1,5, 10, 15), limits = c(0.0833333, 17.1))

# Save the BP plot for boys using 50% height assumption
boys_bp_no_h_plot <- ggplot2::ggsave(filename = "boys_bp_no_h_plot.svg", device = "svg", dpi = 1200)


##########################################################################################################################
### Now repeat above process for girls blood pressures

# Link together the different percentiles datasets
no_ht_g_sbp_5  <- dplyr::bind_rows(under_1_g_sbp_5, (cdc_kids_50h_g_sbp_5 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_sbp_5) %>% dplyr::mutate(group = "5% ")
no_ht_g_sbp_10 <- dplyr::bind_rows(under_1_g_sbp_10, (cdc_kids_50h_g_sbp_10 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_sbp_10) %>% dplyr::mutate(group = "10% ")
no_ht_g_sbp_25 <- dplyr::bind_rows(under_1_g_sbp_25, (cdc_kids_50h_g_sbp_25 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_sbp_25)  %>% dplyr::mutate(group = "25% ")
no_ht_g_sbp_50 <- dplyr::bind_rows(under_1_g_sbp_50, (cdc_kids_50h_g_sbp_50 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_sbp_50) %>% dplyr::mutate(group = "50% ")
no_ht_g_sbp_75 <- dplyr::bind_rows(under_1_g_sbp_75, (cdc_kids_50h_g_sbp_75 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_sbp_75) %>% dplyr::mutate(group = "75% ")
no_ht_g_sbp_90 <- dplyr::bind_rows(under_1_g_sbp_90, (cdc_kids_50h_g_sbp_90 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_sbp_90) %>% dplyr::mutate(group = "90% ")
no_ht_g_sbp_95 <- dplyr::bind_rows(under_1_g_sbp_95, (cdc_kids_50h_g_sbp_95 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_sbp_95) %>% dplyr::mutate(group = "95% ")

no_ht_all_g_sbp <- dplyr::bind_rows(no_ht_g_sbp_5, no_ht_g_sbp_10, no_ht_g_sbp_25, no_ht_g_sbp_50, no_ht_g_sbp_75, no_ht_g_sbp_90, no_ht_g_sbp_95)
no_ht_all_g_sbp$label <- NA
no_ht_all_g_sbp$label[which(no_ht_all_g_sbp$age_mo == max(no_ht_all_g_sbp$age_mo))] <- 
  no_ht_all_g_sbp$group[which(no_ht_all_g_sbp$age_mo == max(no_ht_all_g_sbp$age_mo))]

# Repeat for girls DBP
no_ht_g_dbp_5  <- dplyr::bind_rows(under_1_g_dbp_5, (cdc_kids_50h_g_dbp_5 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_dbp_5) %>% dplyr::mutate(group = "5%")
no_ht_g_dbp_10 <- dplyr::bind_rows(under_1_g_dbp_10, (cdc_kids_50h_g_dbp_10 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_dbp_10) %>% dplyr::mutate(group = "10%")
no_ht_g_dbp_25 <- dplyr::bind_rows(under_1_g_dbp_25, (cdc_kids_50h_g_dbp_25 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_dbp_25)  %>% dplyr::mutate(group = "25%")
no_ht_g_dbp_50 <- dplyr::bind_rows(under_1_g_dbp_50, (cdc_kids_50h_g_dbp_50 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_dbp_50) %>% dplyr::mutate(group = "50%")
no_ht_g_dbp_75 <- dplyr::bind_rows(under_1_g_dbp_75, (cdc_kids_50h_g_dbp_75 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_dbp_75) %>% dplyr::mutate(group = "75%")
no_ht_g_dbp_90 <- dplyr::bind_rows(under_1_g_dbp_90, (cdc_kids_50h_g_dbp_90 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_dbp_90) %>% dplyr::mutate(group = "90%")
no_ht_g_dbp_95 <- dplyr::bind_rows(under_1_g_dbp_95, (cdc_kids_50h_g_dbp_95 %>% dplyr::filter(age_mo >=12 & age_mo < 36)), lo_kids_g_dbp_95) %>% dplyr::mutate(group = "95%")

no_ht_all_g_dbp <- dplyr::bind_rows(no_ht_g_dbp_5, no_ht_g_dbp_10, no_ht_g_dbp_25, no_ht_g_dbp_50, no_ht_g_dbp_75, no_ht_g_dbp_90, no_ht_g_dbp_95)
no_ht_all_g_dbp$label <- NA
no_ht_all_g_dbp$label[which(no_ht_all_g_dbp$age_mo == max(no_ht_all_g_dbp$age_mo))] <- 
  no_ht_all_g_dbp$group[which(no_ht_all_g_dbp$age_mo == max(no_ht_all_g_dbp$age_mo))]

no_ht_all_g_both <- dplyr::bind_rows(no_ht_all_g_sbp, no_ht_all_g_dbp %>% dplyr::rename(sbp = dbp)) %>% dplyr::rename(bp = sbp, age = age_mo) %>% dplyr::mutate(age = age/12) 

# Create ggplot
no_ht_g_plot <- ggplot2::ggplot(no_ht_all_g_both, ggplot2::aes(x = age, y = bp, col = group)) + ggplot2::geom_smooth(se = FALSE) +
  ggrepel::geom_label_repel(ggplot2::aes(label = label), size = 3, nudge_x = 1, nudge_y = 1, na.rm = TRUE) + ggplot2::labs(title = "Blood Pressure Percentiles: Girls", tag = 'B', y = "Blood Pressure (mmHg)", 
                                                                                                x = "Age (yr)") + ggplot2::scale_y_continuous(breaks=c(30,60, 90, 120), limits = c(28, 140)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold"), axis.text = ggplot2::element_text(size = 14, face = "bold"), axis.title = ggplot2::element_text(size = 16, face = "bold"), 
        legend.position = "none") + ggplot2::scale_x_continuous(breaks=c(1,5, 10, 15), limits = c(0.0833333, 17.1))

# Save the BP plot for boys using 50% height assumption
girls_bp_no_h_plot <- ggplot2::ggsave(filename = "girls_bp_no_h_plot.svg", device = "svg", dpi = 1200)


###########################################################################################################################
# Ok now need to make the plots for boys and girls when height is available. In this case use Gemelli for children <1yo and
# the NHLBI/CDC values for when age is >= 1. We already have the under 1 with the gemelli df's, thus just need to isolate out
# the height percentiles we want (i.e. 5th, 50th, 95th)

cdc_b_sbp_5h <- cdc_sbp_boys %>% dplyr::filter(`height_%` == "5") %>% dplyr::select(age, '50') %>% dplyr::rename(age_mo = age, sbp = '50') %>% dplyr::mutate(age_mo = age_mo * 12)
cdc_b_sbp_50h <- cdc_sbp_boys %>% dplyr::filter(`height_%` == "50") %>% dplyr::select(age, '50') %>% dplyr::rename(age_mo = age, sbp = '50') %>% dplyr::mutate(age_mo = age_mo * 12)
cdc_b_sbp_95h<- cdc_sbp_boys %>% dplyr::filter(`height_%` == "95") %>% dplyr::select(age, '50') %>% dplyr::rename(age_mo = age, sbp = '50') %>% dplyr::mutate(age_mo = age_mo * 12)

cdc_kids_5h_b_sbp_50 <- dplyr::bind_rows(under_1_b_sbp_50, cdc_b_sbp_5h) %>% dplyr::mutate(group = "5% Height ")
cdc_kids_50h_b_sbp_50 <- dplyr::bind_rows(under_1_b_sbp_50, cdc_b_sbp_50h) %>% dplyr::mutate(group = "50% Height ")
cdc_kids_95h_b_sbp_50 <- dplyr::bind_rows(under_1_b_sbp_50, cdc_b_sbp_95h) %>% dplyr::mutate(group = "95% Height ")

### Now repeat for DBP
cdc_b_dbp_5h <- cdc_dbp_boys %>% dplyr::filter(`height_%` == "5") %>% dplyr::select(age, '50') %>% dplyr::rename(age_mo = age, dbp = '50') %>% dplyr::mutate(age_mo = age_mo * 12)
cdc_b_dbp_50h <- cdc_dbp_boys %>% dplyr::filter(`height_%` == "50") %>% dplyr::select(age, '50') %>% dplyr::rename(age_mo = age, dbp = '50') %>% dplyr::mutate(age_mo = age_mo * 12)
cdc_b_dbp_95h<- cdc_dbp_boys %>% dplyr::filter(`height_%` == "95") %>% dplyr::select(age, '50') %>% dplyr::rename(age_mo = age, dbp = '50') %>% dplyr::mutate(age_mo = age_mo * 12)

cdc_kids_5h_b_dbp_50 <- dplyr::bind_rows(under_1_b_dbp_50, cdc_b_dbp_5h) %>% dplyr::mutate(group = "5% Height")
cdc_kids_50h_b_dbp_50 <- dplyr::bind_rows(under_1_b_dbp_50, cdc_b_dbp_50h)  %>% dplyr::mutate(group = "50% Height")
cdc_kids_95h_b_dbp_50 <- dplyr::bind_rows(under_1_b_dbp_50, cdc_b_dbp_95h) %>% dplyr::mutate(group = "95% Height")

# Group all the above together to allow for plotting
full_boys_with_h_sbp <- dplyr::bind_rows(cdc_kids_5h_b_sbp_50 %>% dplyr::rename(bp = sbp), cdc_kids_50h_b_sbp_50 %>% dplyr::rename(bp = sbp), 
                                  cdc_kids_95h_b_sbp_50 %>% dplyr::rename(bp = sbp))

full_boys_with_h_sbp$label <- NA

full_boys_with_h_dbp <- dplyr::bind_rows(cdc_kids_5h_b_dbp_50 %>% dplyr::rename(bp = dbp), 
                                  cdc_kids_50h_b_dbp_50 %>% dplyr::rename(bp = dbp), cdc_kids_95h_b_dbp_50 %>% dplyr::rename(bp = dbp))

full_boys_with_h_dbp$label <- NA

full_boys_with_h_dbp$label[which(full_boys_with_h_dbp$age_mo == max(full_boys_with_h_dbp$age_mo))] <- 
  full_boys_with_h_dbp$group[which(full_boys_with_h_dbp$age_mo == max(full_boys_with_h_dbp$age_mo))]

full_boys_with_h_sbp$label[which(full_boys_with_h_sbp$age_mo == max(full_boys_with_h_sbp$age_mo))] <- 
  full_boys_with_h_sbp$group[which(full_boys_with_h_sbp$age_mo == max(full_boys_with_h_sbp$age_mo))]

full_boys_with_h_all_bps <- dplyr::bind_rows(full_boys_with_h_sbp, full_boys_with_h_dbp) %>% dplyr::rename(age = age_mo) %>%
  dplyr::mutate(age = age / 12)

## Now plot the boys' data
yes_ht_b_plot <- ggplot2::ggplot(full_boys_with_h_all_bps, ggplot2::aes(x = age, y = bp, col = group)) + ggplot2::geom_smooth(se = FALSE) +
  ggrepel::geom_label_repel(ggplot2::aes(label = label), size = 3, nudge_x = 1, nudge_y = 1, na.rm = TRUE) + ggplot2::labs(title = "Blood Pressure Percentiles: Boys", tag = 'A', y = "Blood Pressure (mmHg)", 
                                                                                                x = "Age (yr)") + ggplot2::scale_y_continuous(breaks=c(30,60, 90, 120), limits = c(28, 140)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold"), axis.text = ggplot2::element_text(size = 14, face = "bold"), axis.title = ggplot2::element_text(size = 16, face = "bold"), 
        legend.position = "none") + ggplot2::scale_x_continuous(breaks=c(1,5, 10, 15), limits = c(0.0833333, 17.1))

boys_bp_yes_h_plot <- ggplot2::ggsave(filename = "boys_bp_yes_h_plot.svg", device = "svg", dpi = 1200)

#################################################
# Repeat now for girls
cdc_g_sbp_5h <- cdc_sbp_girls %>% dplyr::filter(`height_%` == "5") %>% dplyr::select(age, '50') %>% dplyr::rename(age_mo = age, sbp = '50') %>% dplyr::mutate(age_mo = age_mo * 12)
cdc_g_sbp_50h <- cdc_sbp_girls %>% dplyr::filter(`height_%` == "50") %>% dplyr::select(age, '50') %>% dplyr::rename(age_mo = age, sbp = '50') %>% dplyr::mutate(age_mo = age_mo * 12)
cdc_g_sbp_95h<- cdc_sbp_girls %>% dplyr::filter(`height_%` == "95") %>% dplyr::select(age, '50') %>% dplyr::rename(age_mo = age, sbp = '50') %>% dplyr::mutate(age_mo = age_mo * 12)

cdc_kids_5h_g_sbp_50 <- dplyr::bind_rows(under_1_g_sbp_50, cdc_g_sbp_5h) %>% dplyr::mutate(group = "5% Height ")
cdc_kids_50h_g_sbp_50 <- dplyr::bind_rows(under_1_g_sbp_50, cdc_g_sbp_50h) %>% dplyr::mutate(group = "50% Height ")
cdc_kids_95h_g_sbp_50 <- dplyr::bind_rows(under_1_g_sbp_50, cdc_g_sbp_95h) %>% dplyr::mutate(group = "95% Height ")

### Now repeat for DBP
cdc_g_dbp_5h <- cdc_dbp_girls %>% dplyr::filter(`height_%` == "5") %>% dplyr::select(age, '50') %>% dplyr::rename(age_mo = age, dbp = '50') %>% dplyr::mutate(age_mo = age_mo * 12)
cdc_g_dbp_50h <- cdc_dbp_girls %>% dplyr::filter(`height_%` == "50") %>% dplyr::select(age, '50') %>% dplyr::rename(age_mo = age, dbp = '50') %>% dplyr::mutate(age_mo = age_mo * 12)
cdc_g_dbp_95h<- cdc_dbp_girls %>% dplyr::filter(`height_%` == "95") %>% dplyr::select(age, '50') %>% dplyr::rename(age_mo = age, dbp = '50') %>% dplyr::mutate(age_mo = age_mo * 12)

cdc_kids_5h_g_dbp_50 <- dplyr::bind_rows(under_1_g_dbp_50, cdc_g_dbp_5h) %>% dplyr::mutate(group = "5% Height")
cdc_kids_50h_g_dbp_50 <- dplyr::bind_rows(under_1_g_dbp_50, cdc_g_dbp_50h)  %>% dplyr::mutate(group = "50% Height")
cdc_kids_95h_g_dbp_50 <- dplyr::bind_rows(under_1_g_dbp_50, cdc_g_dbp_95h) %>% dplyr::mutate(group = "95% Height")

# Group all the above together to allow for plotting
full_girls_with_h_sbp <- dplyr::bind_rows(cdc_kids_5h_g_sbp_50 %>% dplyr::rename(bp = sbp), cdc_kids_50h_g_sbp_50 %>% dplyr::rename(bp = sbp), 
                                   cdc_kids_95h_g_sbp_50 %>% dplyr::rename(bp = sbp))

full_girls_with_h_sbp$label <- NA

full_girls_with_h_dbp <- dplyr::bind_rows(cdc_kids_5h_g_dbp_50 %>% dplyr::rename(bp = dbp), 
                                   cdc_kids_50h_g_dbp_50 %>% dplyr::rename(bp = dbp), cdc_kids_95h_g_dbp_50 %>% dplyr::rename(bp = dbp))

full_girls_with_h_dbp$label <- NA

full_girls_with_h_dbp$label[which(full_girls_with_h_dbp$age_mo == max(full_girls_with_h_dbp$age_mo))] <- 
  full_girls_with_h_dbp$group[which(full_girls_with_h_dbp$age_mo == max(full_girls_with_h_dbp$age_mo))]

full_girls_with_h_sbp$label[which(full_girls_with_h_sbp$age_mo == max(full_girls_with_h_sbp$age_mo))] <- 
  full_girls_with_h_sbp$group[which(full_girls_with_h_sbp$age_mo == max(full_girls_with_h_sbp$age_mo))]

full_girls_with_h_all_bps <- dplyr::bind_rows(full_girls_with_h_sbp, full_girls_with_h_dbp) %>% dplyr::rename(age = age_mo) %>%
  dplyr::mutate(age = age / 12)

## Now plot the girls' data
yes_ht_g_plot <- ggplot2::ggplot(full_girls_with_h_all_bps, ggplot2::aes(x = age, y = bp, col = group)) + ggplot2::geom_smooth(se = FALSE) +
  ggrepel::geom_label_repel(ggplot2::aes(label = label), size = 3, nudge_x = 1, nudge_y = 1, na.rm = TRUE) + ggplot2::labs(title = "Blood Pressure Percentiles: Girls", tag = 'A', y = "Blood Pressure (mmHg)", 
                                                                                                x = "Age (yr)") + ggplot2::scale_y_continuous(breaks=c(30,60, 90, 120), limits = c(28, 140)) +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold"), axis.text = ggplot2::element_text(size = 14, face = "bold"), axis.title = ggplot2::element_text(size = 16, face = "bold"), 
        legend.position = "none") + ggplot2::scale_x_continuous(breaks=c(1,5, 10, 15), limits = c(0.0833333, 17.1))

girls_bp_yes_h_plot <- ggplot2::ggsave(filename = "girls_bp_yes_h_plot.svg", device = "svg", dpi = 1200)


# -------------------------------------------------------------------------- */
#'
#'
#' # References
#'<div id="refs"></div>
#'
#' # Session Info
#+ label = "sessioninfo"
sessionInfo()
