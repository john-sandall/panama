library(dplyr)
library(magrittr)
library(stringdist)

# Sanction list
download.file("http://hmt-sanctions.s3.amazonaws.com/sanctionsconlist.csv", 
              "sanctionsconlist.csv")
sanction <- read.csv("sanctionsconlist.csv", skip=1, stringsAsFactors=FALSE)
# Exclude organizations
sanction <- sanction[sanction$Name.1 != "",]

# Officers list
download.file("https://cloudfront-files-1.publicintegrity.org/offshoreleaks/data-csv.zip",
              "data-csv.zip")
unzip("data-csv.zip", files = "Officers.csv")
officers <- read.csv("Officers.csv")
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
# Fuzzy matching
mat <- stringdistmatrix(names, officers$name, method = "lcs")

# names <- extractSanctionName(sanction)
# mpanama <- matchName(names, officers$name)
# save(mpanama, file="mpanama.RData")
# # Matching rate
# length(mpanama[mpanama != 0])/length(mpanama)
# 
# sanction[mpanama != 0,] %>%
#   group_by(Nationality) %>%
#   summarise(n = n()) %>%
#   arrange(-n)