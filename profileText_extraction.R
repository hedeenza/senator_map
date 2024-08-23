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
  str_extract("Representative|Senator|Delegate") |>
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
  
  sections$profileText_1 |> str_view("Representative")
  
  
  # Detect for the desired combination of the positions, add a column with a logical value indicating that combination of positions
  sections |> mutate(R_S = str_detect(sections$profileText_1, "Representative and a Senator")) |> select(R_S)
  sections |> mutate(D_R = str_detect(sections$profileText_1, "Delegate and a Representative")) |> select(D_R)
  sections |> mutate(D_S = str_detect(sections$profileText_1, "Delegate and a Senator")) |> select(D_S)
  
  sections$profileText_1 |> str_view("vice president") # row 2,845
  sections$profileText_1 |> str_view("Representative|(.ice) (.resident)") # that way both lower case and capital P are detected
  
  sections |> mutate(S_VP = str_detect(sections$profileText_1, "(.ice) (.resident)")) |> filter(S_VP == TRUE) |> select(familyName, profileText_1, S_VP)
  
  
  sections |> mutate(P = str_detect(sections$profileText_1, "(\\d.+) (.resident)")) |> filter(P == T) |> select(givenName, familyName, profileText_1, P)
  

  # Plan is to add columns to the sections table that indicate whether they were each position separately,
  # allowing multiple positions to be true
  # later filtering will be along those columns
  
method_test <- 
    sections |> 
    mutate(
      Rep = str_detect(sections$profileText_1, "Representative"),
      Sen = str_detect(sections$profileText_1, "Senator"),
      Del = str_detect(sections$profileText_1, "Delegate"),
      VP = str_detect(sections$profileText_1, "(.ice) (.resident)"),
      P = str_detect(sections$profileText_1, "(\\d.+) (.resident)"),
      P1 = str_detect(sections$profileText_1, "eighth")) |>
    select(
      usCongressBioId,
      givenName,
      familyName,
      profileText_1,
      Rep,
      Sen,
      Del,
      VP,
      P,
      P1)
  
  method_test |> 
    filter(
      #Rep == F & 
      #Sen == F & 
      #Del == T  &
      #VP == F & 
      P == T) |> 
    arrange(familyName) |> 
    print(n=49)
  
  # President: first, third, eighth..... (continue during next session) there are likely more
  
  eight_check <- bios |> filter(usCongressBioId == "F000260" | usCongressBioId == "L000381")
  
  delegates <- method_test |> filter(Del == T) |> arrange(familyName) |>print(n=40)
  
  # It appears those who were neither Representatives, Senators, Delegates, or Vice Presidents before becoming
  # President of the United States are not listed in the bios
  wh <- bios |> filter(givenName == "William" & familyName == "Harrison")
  bush <- bios |> filter(familyName == "Bush")
  biden <- bios |> filter(familyName == "Biden") |> arrange(familyName) |> print()
    biden$profileText

  clinton <- bios |> filter(familyName == "Clinton") |> print(n=30)
  
  bios |> filter(familyName == "Washington")
  
sections$profileText_1 |>
  str_view("six") # fir > 1; .eco > 2; third > 3rd; six > 6; eight > 8; .hirty- > 3; .inety- > 9


# Fixing the inconsistent naming conventions before piping this into the method-test
# Washington did not show up before
numerical_fix <-
  sections$profileText_1 |>
    str_replace_all("firs", "1") |>
  str_replace_all(".econ", "2") |>
  str_replace_all("third", "3d") |>
  str_replace_all("sixt", "6") |>
  str_replace_all("eight", "8") |>
  str_replace_all(".hirty-", "3") |>
  str_replace_all(".inety-", "9") |>
  as.data.frame()

names(numerical_fix) <- "profileText_1"  

# Grabbing just the first few columns of the table
id_info <- 
  sections |>
  select(
    usCongressBioId,
    familyName,
    givenName)
# Binding the numerically fixed columns
id_numerical_fix <- 
  cbind(
    id_info,
    numerical_fix)


id_numerical_fix$profileText_1 |> str_view("1t")

id_numerical_fix |> filter(familyName == "Washington")

method_test_2 <- 
  id_numerical_fix |> 
  mutate(
    Rep = str_detect(id_numerical_fix$profileText_1, "Representative"),
    Sen = str_detect(id_numerical_fix$profileText_1, "Senator"),
    Del = str_detect(id_numerical_fix$profileText_1, "Delegate"),
    VP = str_detect(id_numerical_fix$profileText_1, "(.ice) (.resident)"),
    P = str_detect(id_numerical_fix$profileText_1, "(\\d.*) (.resident)")) |>
  select(
    usCongressBioId,
    givenName,
    familyName,
    profileText_1,
    Rep,
    Sen,
    Del,
    VP,
    P)

method_test_2 |> 
  filter(
    #Rep == F & 
    #Sen == F & 
    #Del == T  &
    #VP == F & 
    P == T) |>
  select(
    givenName,
    familyName,
    Rep,
    Sen,
    Del,
    VP,
    P) |>
  arrange(familyName) 

  
  

    
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

