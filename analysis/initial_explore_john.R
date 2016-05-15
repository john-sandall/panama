library(dplyr)
library(ggplot2)
library(lubridate)

# Read in data
read_panama_csv = function(filename) {
    file_path = paste0('offshore_leaks_csvs-20160513/', filename)
    return(read.csv(file_path, stringsAsFactors=F))
}

addresses = "Addresses.csv" %>% read_panama_csv()
edges = "all_edges.csv" %>% read_panama_csv()
entities = "Entities.csv" %>% read_panama_csv()
intermediaries = "Intermediaries.csv" %>% read_panama_csv()
officers = "Officers.csv" %>% read_panama_csv()

nrow(addresses)
nrow(edges)
nrow(entities)
nrow(intermediaries)
nrow(officers)

View(head(addresses))
View(head(edges))
View(head(entities))
View(head(intermediaries))
View(head(officers))

# Let's find Emma Watson
lower_names = officers$name %>% tolower()
contains_emma = grepl("emma", lower_names)
contains_watson = grepl("watson", lower_names)
officers %>% filter(contains_emma & contains_watson)
# officer_id 12126782 and 13005189

# Officer 12126782 is shareholder of 10152535 and 14020827 is reg addresss
edges %>% filter(node_1 == 12126782)
edges %>% filter(node_2 == 12126782)

# Node 13005189 is beneficiary of 10152535 and 14086672 is reg addresss
edges %>% filter(node_1 == 13005189)
edges %>% filter(node_2 == 13005189)

# Empty
addresses %>% filter(node_id == 12126782) # emma1
addresses %>% filter(node_id == 13005189) # emma2
addresses %>% filter(node_id == 10152535) # O1 is shareholder of this, O2 is benefic

# 69 Apsley House; 23-29 Finchley Road; London NW8 0NZ
addresses %>% filter(node_id == 14020827) # reg address for O1

# 2 Cannonbury Place; London; N1 2NQ; UK
addresses %>% filter(node_id == 14086672) # reg address for O2

# Falling Leaves Ltd
entities %>% filter(node_id == 10152535)

# Who else connected?
edges %>% filter(node_1 == 10152535)
edges %>% filter(node_2 == 10152535)

11011539
12106514


