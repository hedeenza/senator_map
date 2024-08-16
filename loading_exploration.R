library(tidyverse)
library(jsonlite)

# https://bioguide.congress.gov/
# Then click browse biographies
# Download (top right)
# Select "download bulk data, json format"
# zip file
# unzips to 12,972 separate json files. 
link <- "https://bioguide.congress.gov/search" 

# Loading the JSON, converting to a data frame
data <- fromJSON("BioguideProfiles/A000001.json", flatten = TRUE)

names(data)
str(data)


df <- as.data.frame(data)

fromJSON()


# Getting all the file names into a text file with the shell
  # ls > json-list.txt

# Now that all the files are listed, adding a coma after each in neo vim
  # :%norm I" <ENTER> to insert a " at the beginning of each line
  # :%norm A", <ENTER> to insert a ", at the end of each line

# Read the file in, specify the UTF-16 encoding to make everyone happy
file_list <- 
  read_csv("BioguideProfiles/json-list.txt", 
           col_names = FALSE,
           locale = locale(encoding = "UTF-16"))

# Convert the file list to a vector 
file_vector <- 
  file_list[1]

# Renaming the column
names(file_vector) <- "Bio_File"

head(file_vector)

# using {jsonlite} 
# "https://r4ds.hadley.nz/rectangling.html#json"

# Trying with one first
file_try <- 
  read_json("BioguideProfiles/A000001.json")

# "Rectangling"
file_try2 <-
  tibble(json = list(file_try))

# having issues - currently cannot combine the logical "birthcirca" with the characters in the other fields
file_try2 |>
  unnest_longer(json) |>
  unnest_longer(json)
  unnest_wider(results)
  
file_try3 <-
  file_try2 |>
  unnest_wider(json) |>
  unnest_wider(jobPositions, names_sep = ",")


file_try2 |>
  unnest_wider(json) |> # worked to make the table wide! but still nested in..
                              # jobPositions, creativeWork, researchRecord
  unnest_longer(jobPositions) |> # playing around with continued unnest_wider() and unnest()longer until the 
                                  # data is extracted
  unnest_longer(jobPositions)


