# custom functions 

library(tidyverse)
library(haven)
library(gt)
library(countrycode)
library(directlabels)
library(gtsummary)
library(rnaturalearth)
library(sf)
library(rnaturalearthdata)
library(WeightIt)
library(ordinal)
library(svglite)

# Function to get the name of the latest file
get_latest <- function(file_pattern){
    file_list <- list.files(path = "data/processed", pattern = file_pattern)
    latest_file <- tail(sort(file_list), n = 1)
    return(latest_file)
}

# Function to build the regression model

get_model  <- function(df, dv, ivs, w = 'pspwght', type = 'glm'){
    
    myform  <-  as.formula(paste0(dv, " ~ ", paste(ivs, collapse= "+")))

    if (type == 'lm'){
        mymodel <- lm(myform, weights = if(!is.null(w)) get(w) else NULL, data = df)
    } else if (type == 'glm') {
       mymodel <- glm(myform, family = 'binomial', weights =  if(!is.null(w)) get(w) else NULL, data = df)
    }

    return(mymodel)
}

get_table <- function(model, toprint = everything(), type = 'glm'){
    
    if (type == 'glm'){
        table_reg <- tbl_regression(
            model,
            exponentiate = TRUE,
            include = toprint,
            label = list_labels
        ) |> 
        modify_column_hide(conf.low) |> 
        remove_abbreviation("CI = Confidence Interval") |> 
        add_significance_stars()
    } else {
        table_reg <- tbl_regression(model, include = toprint, label = list_labels)  |> remove_abbreviation(c("CI = Confidence Interval")) |> bold_p(t = 0.05, q = FALSE)
    }
}


# Function to save an image of the table/plot and print the image on the report
save_output  <- function(myobj, format = "svg", w=NULL, h=NULL, dpi = 300){
    name_str  <- deparse(substitute(myobj))
    file_path <- paste0("output/", name_str, ".", format)
    
    if(str_detect(name_str, 'tbl')){
        # Tables are saved as RTF files
        myobj |> as_gt() |> gtsave(paste0("output/", name_str, '.rtf'))
    } else if (str_detect(name_str, 'fig')) {
        if(missing(w)){
            ggsave(myobj, filename = file_path, dpi=dpi)
        } else {
            ggsave(myobj, filename = file_path, width = w, height = h, dpi=dpi)
        }
    } else {
        print("Name of the object should contain either 'fig' or 'tbl'")
    }
}

######################
### Import files

# Read file in the root of sub-folder "data" with name containing "ESS" 
df_ess_raw <- read_dta(paste0("data/processed/", get_latest("ESS")))

# Getting complete observations
df_ess <- df_ess_raw |> filter(!if_any(c(agea, gndr, brncntr, gincdif), ~is.na(.x)))

# Read file in the root of sub-folder "data" with name containing "VS" 
df_vs <- read_dta(paste0("data/processed/", get_latest("VS_")))

# Read file in the root of sub-folder "data" with name containing "VSEU" 
df_vseu <- read_dta(paste0("data/processed/", get_latest("VSEU_")))

# Data from the electoral commission 
df_neds <- read_dta("data/raw/neds/NEDS_General_Elections_2024_F2F_Post.dta")



######################
### Variables

# Main model

df_ess$gincdif_agree <- if_else(df_ess$gincdif %in% c(1, 2),  1, 0, missing = NA)
df_ess$gincdif_strong_agree <- if_else(df_ess$gincdif == 1,  1, 0, missing = NA)

df_ess$age_group  <- cut(df_ess$agea, c(0, 24, 35, 45, 55, 65, Inf), c("18-24", "25-35", "36-45", "46-55", "56-65", ">66"), include.lowest=TRUE)

df_ess$age_group <-  relevel(df_ess$age_group, ref = "18-24")

df_ess$false_cohort <- cut(df_ess$yrbrn, breaks = c(1940, 1960, 1980, 2020), right = FALSE, labels = c("1940-1960", "1961-1980", "1981-"))

df_ess$false_cohort  <-  cut(df_ess$yrbrn, breaks = c(1900, 1940, 1960, 1980, 2020), right = FALSE, labels = c("1900-1940", "1941-1960", "1961-1980", "1981-"))

df_ess <- df_ess |> mutate(
    hinc_quint = case_match(hinctnta,
        c(1, 2) ~ "1st quintile",
        c(3, 4) ~ "2nd quintile",
        c(5, 6) ~ "3rd quintile",
        c(7, 8) ~ "4th quintile",
        c(9,10) ~ "Top quintile", 
        .default = NA        
    )
)

df_ess$hinc_topQ <- if_else(df_ess$hinctnta %in% c(9,10), "Top quintile", "Bottom quintiles", missing = NA)

df_ess$hinc_topD <- if_else(df_ess$hinctnta == 10, "Top Decile", "Bottom Deciles", missing = NA)


vars_indo <- c('age_group', 'as_factor(gndr)', 'as_factor(brncntr)', 'eisced', "as_factor(class5)", "as_factor(hincfel)",  'as_factor(mnactic)', 'lrscale',  'as_factor(essround)')

vars_omit <- c('eisced', 'as_factor(mnactic)', 'as_factor(essround)', "as_factor(hincfel)")

# Beliefs models
believe_vars <- c("sbeqsoc", "sblazy", "sblwcoa", "sbprvpv", "sbstrec", "sbbsntx")

# Creating inverted scales so higher values denote higher agreement
df_ess <- df_ess |> mutate(across(all_of(believe_vars), ~ 6 - .x, .names = "{.col}_inv"))
believe_vars_inv <- paste0(believe_vars,"_inv")

# Creating dummy variables for agreement
df_ess <- df_ess |> mutate(across(all_of(believe_vars), ~ if_else(.x %in% c(1,2), 1, 0), .names = "{.col}_agree"))
believe_vars_agree <- paste0(believe_vars,"_agree")

# Creating dummy variables for strong agreement
df_ess <- df_ess |> mutate(across(all_of(believe_vars), ~ if_else(.x == 1, 1, 0), .names = "{.col}_strong_agree"))
believe_vars_strong_agree <- paste0(believe_vars,"_strong_agree")

# Create variable indicating priority to unemployed over old
df_ess$old_prior <- df_ess$gvslvol - df_ess$gvslvue

# Create variable with country label
df_ess$cntry_label <- countrycode(df_ess$cntry, "iso2c", "country.name")

# Labels for independent variables in output tables
list_labels <- list(
            `as_factor(brncntr)` = "Born in country",
            `age_group` = "Age group",
            `as_factor(gndr)` = "Gender", 
            `as_factor(class)` = 'Social class',
            `as_factor(hincfel)` = 'Financial situation'
        )

# Generating EU MAP
eumap <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf") |> filter(!name %in% c("Russia", "Faeroe Is."))

# Creating default source
source_ess  <-  "Source: Authors' own analysis of the European Social Survey"
