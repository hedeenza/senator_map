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
  
file_try2 |>
  unnest_wider(json) |> # worked to make the table wide! but still nested in..    
                            # jobPositions, creativeWork, researchRecord
  unnest_wider(jobPositions, names_sep = ",") # playing around with continued unnest_wider() and unnest()longer until the 
                                                  # data is extracted
  unnest_longer(jobPositions)
  
file_try3 <-
  file_try2 |>
  unnest_wider(json) |>
  unnest_wider(jobPositions, names_sep = ",")

names(file_try3)[15] <- "jobPositions"

file_try4 <-
  file_try3 |>
  unnest_wider(jobPositions) |> # got the job positions to "job" and "congressAffiliation"
  unnest_wider(congressAffiliation) # WAIT!! I just need to remove the birthCirca and deathCirca!! then I can unnest longer

# New Method 
new_method <- 
  file_try2 |>
  unnest_wider(json) |> # Unnesting wider to allow us to select the problematic, currently irrelevant columns
  select(-birthCirca, -deathCirca) # selecting everything except the two columns causing problems
  # need to re-collapse into a list 
  
collapsed_half <-
  new_method |>
  select(-c(1:12))

first_nest <- 
  pivot_longer(
    collapsed_half,
    cols = c(jobPositions, creativeWork, researchRecord)) 
  
  #unnest_longer(value) |>
  #unnest_longer(value) 


# Back to the beginning
tibble(file_try)[-c(10,12),] |>
  unnest_longer(file_try)

# And again 
new_try <- 
  tibble(json = list(file_try)) |>
  unnest_wider(json)

names(new_try)

new_try2 <-
  new_try |>
  unnest_wider(
    jobPositions,
    names_sep = "_")

glimpse(new_try2)

new_try3 <-
  new_try2 |>
  unnest_wider(
    jobPositions_1,
    names_sep = "_")

glimpse(new_try3)

new_try4 <-
  new_try3 |>
  unnest_wider(
    jobPositions_1_job,
    names_sep = "_")

glimpse(new_try4)

new_try5 <-
  new_try4 |>
  unnest_wider(
    jobPositions_1_congressAffiliation,
    names_sep = "_")

glimpse(new_try5)

new_try6 <-
  new_try5 |>
  unnest_wider(
    jobPositions_1_congressAffiliation_congress,
    names_sep = "_")

glimpse(new_try6)

new_try7 <-
  new_try6 |>
  unnest_wider(
    jobPositions_1_congressAffiliation_partyAffiliation,
    names_sep = "_")

glimpse(new_try7)

new_try8 <-
  new_try7 |>
  unnest_wider(
    jobPositions_1_congressAffiliation_partyAffiliation_1,
    names_sep = "_")

glimpse(new_try8)

new_try9 <-
  new_try8 |>
  unnest_wider(
    jobPositions_1_congressAffiliation_partyAffiliation_1_party,
    names_sep = "_")

glimpse(new_try9)

new_try10 <-
  new_try9 |>
  unnest_wider(
    jobPositions_1_congressAffiliation_caucusAffiliation,
    names_sep = "_")

glimpse(new_try10)

new_try11 <-
  new_try10 |>
  unnest_wider(
    jobPositions_1_congressAffiliation_caucusAffiliation_1,
    names_sep = "_")

glimpse(new_try11)

new_try12 <-
  new_try11 |>
  unnest_wider(
    jobPositions_1_congressAffiliation_caucusAffiliation_1_party,
    names_sep = "_")

glimpse(new_try12)

new_try13 <-
  new_try12 |>
  unnest_wider(
    jobPositions_1_congressAffiliation_represents,
    names_sep = "_")

glimpse(new_try13)

new_try14 <-
  new_try13 |>
  unnest_wider(
    creativeWork,
    names_sep = "_")

glimpse(new_try14)
