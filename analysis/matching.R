library(dplyr)
library(magrittr)
library(stringdist)

# # Original sanction list
# download.file("http://hmt-sanctions.s3.amazonaws.com/sanctionsconlist.csv", 
#               "sanctionsconlist.csv")
# sanction <- read.csv("sanctionsconlist.csv", skip=1, stringsAsFactors=FALSE)
# Sanction list with country codes
sanction <- read.csv("./data/sanctions/sanctions_list_with_country_codes.csv",
                     stringsAsFactors=FALSE)
# Exclude organizations
sanction <- sanction[sanction$Name.1 != "",]

# Officers list
download.file("https://cloudfront-files-1.publicintegrity.org/offshoreleaks/data-csv.zip",
              "data-csv.zip")
unzip("data-csv.zip", files = "Officers.csv")
officers <- read.csv("Officers.csv", stringsAsFactors=FALSE)
officers$name %<>% tolower()

# Get lowercase first and last name from the sanctioned people table
extractSanctionName <- function(sanction_names) {
  # Force UTF-8
  sanction_names[,c(2,1)] <- apply(sanction_names[,c(2,1)], 2, enc2utf8)
  # Clean names
  # sanction_names[,c(2,1)] <- apply(sanction_names[,c(2,1)], 2, sub, " ", "")
  sanction_names[,1] <- gsub(" ", "", sanction_names[,1])
  sanction_names[,2] <- gsub(" ", "", sanction_names[,2])
  names <- apply(sanction_names[,c(2,1)], 1, paste, collapse=" ")
  names <- tolower(names)
  names
}

# Match sanction names to names from Panama Papers
# Simple regex for start
matchName <- function(sanction_name, panama_name) {
  matched <- rep(NA, length(sanction_name))
  for (i in 1:length(sanction_name)) {
    m <- grep(sanction_name[i], panama_name)
    if (length(m) == 0) {
      matched[i] <- 0
    }
    else {
      matched[i] <- m
    }
  }
  matched
}

# Match sanction names to names from Panama Papers
# Fuzzy matching, pre-filtering on country code
# 
# Cut-off criteria allows to adjust inclusion criteria
# Treat matches with distance larger than half the number of characters as non-match.
matchApproxName <- function(sanction, panama, cut=0.5) {
  matched <- data.frame(matrix(nrow=nrow(sanction),ncol=ncol(panama)))
  colnames(matched) <- colnames(panama)
  for (i in 1:nrow(sanction)) {
    if (is.na(sanction[i,]$matched_country_code)) {
      matched[i,] <- rep(NA, ncol(matched))
    }
    else {
      country <- panama[panama$country_codes==sanction[i,]$matched_country_code,]
      if (nrow(country) == 0) {
        matched[i,] <- rep(NA, ncol(matched))
      }
      else {
        # Calculate Longest common substring distance and get the closest entry from Panama Papers
        mat <- stringdistmatrix(sanction[i,"name"], country$name, method = "lcs")
        if (min(mat[1,])>(cut*nchar(sanction[i,"name"]))) {
          matched[i,] <- rep(NA, ncol(matched))
        }
        else {
          matched[i,] <- country[which.min(apply(mat,2,min)),]
        }
      }
    }
  }
  matched
}

sanction$name <- extractSanctionName(sanction)

# # Regex matching
# mpanama <- matchName(names, officers$name)
# save(mpanama, file="mpanama.RData")
# # Matching rate
# length(mpanama[mpanama != 0])/length(mpanama)
# grepmatches <- sanction[mpanama != 0,]
# write.csv(grepmatches, file="./data/sanctions/matches.csv")
# sanction[mpanama != 0,] %>%
#   group_by(Nationality) %>%
#   summarise(n = n()) %>%
#   arrange(-n)

# Fuzzy matching
mpanama <- matchApproxName(sanction, officers)
# Matching rate
nrow(mpanama[!is.na(mpanama[,1]),])/nrow(mpanama)
write.csv(mpanama, file="./data/sanctions/fuzzymatches.csv")
