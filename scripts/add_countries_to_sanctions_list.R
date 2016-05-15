#!/usr/bin/env R
# -*- encoding:utf-8 -*-

# Author:   John Sandall
# Date:     Sunday 15th May 2016
# Contact:  @john_sandall


# Load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringdist)


##########################
# Part 1: Helper functions

# Pulls a single vector out of a dplyr data.frame, see http://stackoverflow.com/questions/21618423/extract-a-dplyr-tbl-column-as-a-vector
pull = function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}

# Generic helper function used elsewhere
read_panama_csv = function(filename) {
    file_path = paste0('../data/offshore_leaks/', filename)
    return(read.csv(file_path, stringsAsFactors=F))
}


###########################
# Part 2: Load & check data

sanctions_list = read.csv('../data/sanctions/sanctionsconlist.csv', stringsAsFactors=F, skip=1)
officers = "Officers.csv" %>% read_panama_csv()

# Clean up ICIJ countries -> country codes
icij_country_data = officers %>%
    select(country_codes, countries) %>%
    filter(country_codes != "") %>%
    arrange(country_codes) %>%
    distinct()

icij_country_data.single_countries = icij_country_data %>% filter(!grepl(";", country_codes))

# Match against standardised country codes
# (http://data.okfn.org/data/core/country-codes/r/country-codes.csv)
country_codes = read.csv('../data/sanctions/country-codes.csv', stringsAsFactors=F) %>%
    select(name, code = ISO3166.1.Alpha.3)

# Check matches between ICIJ data and ISO country codes, anything missing?
check_for_missing_countries = icij_country_data.single_countries %>%
    left_join(country_codes, by = c('country_codes' = 'code'))

# Country code lookup (manually curated during our hackathon)
country_code_lookup = read.csv('../data/sanctions/country_code_lookup.csv', stringsAsFactors=F) %>% unique()


#################################################
# Part 3: Lookup country codes for sanctions data

# Lookup country code
lookup_country = function(x) {
    x = trimws(x)
    return(country_code_lookup %>% filter(name == x) %>% pull(code))
}


# Returns best guess for country code given a row of sanctions data
find_country = function(row) {
    bad_countries = c("", "Africa")
    if (!row$Country %in% bad_countries) {
        return(lookup_country(row$Country))
    } else if (row$Nationality != "") {
        return(lookup_country(row$Nationality))
    } else if (row$Country.of.Birth != "") {
        return(lookup_country(row$Country.of.Birth))
    } else if (row$Address.6 != "") {
        return(lookup_country(row$Address.6))
    } else if (row$Address.4 != "") {
        return(lookup_country(row$Address.4))
    } else if (row$Address.3 != "") {
        return(lookup_country(row$Address.3))
    } else if (row$Address.2 != "") {
        return(lookup_country(row$Address.2))
    } else if (row$Regime != "") {
        regime = lookup_country(row$Regime)
        if(length(regime) > 0) {
            return(regime)
        } else {
            return("NA")
        }
    } else {
        return("NA")
    }
}

# Go ahead & add the country codes
sanctions_list$matched_country_code = NA
for (i in 1:nrow(sanctions_list)) {
    if (i %% 100 == 0) print(i)
    country_code = find_country(sanctions_list[i,])
    sanctions_list$matched_country_code[i] = country_code
}

# Check how well we did
sanctions_list %>% filter(matched_country_code == "NA") %>% nrow()  # 297 NAs remaining
sanctions_list %>% filter(matched_country_code != "NA") %>% nrow()  # 8513 matched!

# Save the result for later use
write.table(sanctions_list, file="../data/sanctions/sanctions_list_with_country_codes.csv", col.names=T, row.names=F, sep=',')
