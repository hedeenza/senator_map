library(tidyverse)

# Read in the combined table
bios <- read_csv("combined_bios.csv")

# Separating into the "Care about" and "Don't care about" columns
sections <-
  bios |>
  separate_wider_delim(
    cols = profileText,
    names_sep = "_",
    delim = ";",
    too_few = "align_start",
    too_many = "drop")

# Creating a proof of concept tester
tester <- sections |> head(n=20)

# Detecting the position 
tester$profileText_1 |> str_detect("Representative|Senator")

# Extracting just the position name
tester$profileText_1 |> str_extract("Representative|Senator")


# Experimenting with what to separate, and in what order 
year_split <-
  tester$profileText_2 |> 
  str_remove_all("\\bborn\\b|\\bin\\b|\\bnear\\b") |> # includes the boundaries around the words, so "in" doesn't get removed from the middle of a word
  str_replace_all(" ([0123456789][0123456789][0123456789][0123456789])", "*\\1") |> # four of any digit in a row is the year, detecting a space followed by the year
  str_trim() |>
  as.data.frame() |>
  separate_wider_delim(
    cols = 1, 
    names_sep = "_",
    delim = "*",
    too_few = "align_start",
    too_many = "drop"
  ) # separating off the year

# 
year_split
  
  

str_replace_all("January|February|March|April|May|June|July|August|September|October|November|December", "-") |>
  str_replace_all("\\, -", "*") |>
  str_trim() |>
  as.data.frame() |> 
  separate_wider_delim(
    cols = 1,
    names_sep = "_",
    delim = "*",
    too_few = "align_start",
    too_many = "drop")
