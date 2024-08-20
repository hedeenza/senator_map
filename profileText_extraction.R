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

# Creating a proof of concept tester ----
tester <- sections |> head(n=50)

both_names <- 
  tester |>
  select(familyName, givenName)

# Detecting the position 
tester$profileText_1 |> str_detect("Representative|Senator|Delegate")

# Extracting just the position name
position <- 
  tester$profileText_1 |> 
  str_extract("Representative|Senator|Delegate") |>
  as.data.frame()


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
names(year_split) <- c("Working", "Year")

 # Do something about the double space - everything before is a birth name. 
day_split <- 
  year_split$Working |>   

  str_replace_all("January", "*January") |>        # need a way of separating the months from what's on the left if they're there
  str_replace_all("February", "*February") |> 
  str_replace_all("March", "*March") |> 
  str_replace_all("April", "*April") |> 
  
  str_replace_all("May", "*May") |> 
  str_replace_all("June", "*June") |> 
  str_replace_all("July", "*July") |> 
  str_replace_all("August", "*August") |>
  
  str_replace_all("September", "*September") |> 
  str_replace_all("October", "*October") |> 
  str_replace_all("November", "*November") |> 
  str_replace_all("December", "*December") |>
  
  as.data.frame() |>
  
  separate_wider_delim(
    cols = 1,
    names_sep = "-",
    delim = "*",
    too_few = "align_start"
  )

# Renaming to continue working 
day_split

names(day_split) <- c("Working", "Day")

# Detecting the "born by this name" getting lumped into this type error
day_split$Working |>
  str_detect("  ") # two spaces were where "in" was removed

# Replacing that space with a delimiter, separating, aligning right to effectively remove the name
name_error_split <-
  day_split$Working |>
  str_replace_all("  ", "*") |>
  
  as.data.frame() |>
  
  separate_wider_delim(
    cols = 1,
    names_sep = "_",
    delim = "*",
    too_few = "align_end"
  ) |>
  print(n=50)

# Renaming to continue working
names(name_error_split) <- c("Error", "Working")

# The first thing is always the town, aligning to start to grab just that column
town <- 
  name_error_split$Working |>
  as.data.frame() |>
  separate_wider_delim(
    cols = 1,
    names_sep = "_",
    delim = ",",
    too_few = "align_start"
  )

names(town) <- c("Town", "mix1", "mix2", "mix3", "mix4")

town$Town

# The last thing is always the state/country, aligning to end to grab just that column
state <-
  name_error_split$Working |>
  str_remove_all("\\bon\\b") |>
  str_trim() |>
  as.data.frame() |>
  separate_wider_delim(
    cols = 1, 
    names_sep = "_",
    delim = ",",
    too_few = "align_end"
  ) |> print(n=50)

names(state) <- c("mix1", "mix2", "mix3", "State", "mix4")

state$State


# Merging thing back together ----
remerge <-
  tibble(
    both_names,
    position,
    town$Town,
    state$State,
    day_split$Day,
    year_split$Year
    )


# Attempting with the full table now that proof of concepts seem to mostly work

last_first <- 
  sections |> 
  select(familyName, givenName)

  # quick test 
  table(last_first$familyName) |> as.data.frame() |> arrange(desc(Freq)) |> head()
  table(last_first$givenName) |> as.data.frame() |> arrange(desc(Freq)) |> head()
  round((2382/12973)*100,1)
  
  last_first |> filter(familyName == "Smith" & givenName == "John")
  
role <- 
  sections$profileText_1 |>
  str_extract("Representative|Senator|Delegate|Representative and a Senator") |>
  as.data.frame()

  # quick test
  names(role) <- c("Role")

  table(role$Role) |> as.data.frame() |> arrange(desc(Freq))
  
  # testing for two roles 
  role_test <- 
    sections$profileText_1 |>
    str_detect("Representative and a Senator") |>
    as.data.frame()
  
  names(role_test) <- "joint_position" 

  table(role_test$joint_position) |> as.data.frame() |> arrange()

  # There are double and even triple positions... going to need to rethink my strategy
  sections$profileText_1 |>
    str_view("Delegate and a Senator")
  
  sections$profileText_1 |>
    str_view("Delegate and a Representative")
  
  
  
birth_year <-
  sections$profileText_2 |> 
  str_remove_all("\\bborn\\b|\\bin\\b|\\bnear\\b") |> # includes the boundaries around the words, so "in" doesn't get removed from the middle of a word
  str_replace_all(" ([0123456789][0123456789][0123456789][0123456789])", "*\\1") |> # four of any digit in a row is the year, detecting a space followed by the year
  str_trim() |>
  as.data.frame() |>
  separate_wider_delim(
    cols = 1, 
    names_sep = "_",
    delim = "*",
    too_few = "align_start",
    too_many = "drop")

# Encountering some errors, or lines typed in a different order, making a table to examine
birth_table <- 
  table(birth_year$Year) |> 
  as.data.frame() |> 
  arrange(desc(Freq))

names(birth_year) <- c("Place", "Year", "mix1", "mix2")

table(birth_year$mix1) |>
  as.data.frame() |>
  arrange(desc(Freq))

# Years have been distributed through the Year, mix1, and mix2 columns
# will need a way of selecting all 3, extracting the proper year from all of those,
# then merging into one column

