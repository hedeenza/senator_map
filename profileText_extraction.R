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

# Separating out the position 

  sections |>
  str_view("Senator|Representative") |>
    head()


table(position$profileText_1_38)
