library(tidyverse)

# load the show rows
shows_only <- read_csv('shows_only.csv')

# next step: identify problem rows and handle them separately
shows_only_problem <- shows_only %>% 
  mutate(n_shows = str_count(all_text, 'PM')) %>% 
  filter(n_shows > 1)

# if this evaluates to TRUE, we have some duplicates but no triples+
all(shows_only_problem$n_shows == 2)

shows_only_problem_split <- shows_only_problem %>% 
  # get the last character of the first show (the M in PM)
  mutate(endchar_first_show = str_locate(all_text, 'PM')[, 2]) %>% 
  # and separate the show name at that position
  mutate(show1 = str_sub(all_text, 1, endchar_first_show),
         show2 = str_sub(all_text, endchar_first_show + 1, -1)) %>% 
  # want one row per unique show
  gather('show_no', 'artist_time', starts_with('show')) %>% 
  # only care about the original text and the resulting artist_time pairs
  select(all_text, artist_time)

# combine the problem children into the original data and get artist_time for the other shows too
# then extract artist and time as separate columns
shows_allcols <- shows_only %>% 
  # left_join will duplicate the rows in original tibble with two shows, which is what we want!
  left_join(shows_only_problem_split, by = 'all_text') %>% 
  # create the artist_time column for other rows
  mutate(artist_time = if_else(is.na(artist_time), all_text, artist_time)) %>% 
  # separate artist and time columns at either period or comma
  separate(artist_time, c('artist', 'time'), sep = '[,\\.] ') %>% 
  # drop the raw text column
  select(artist, stage, date, time)

# and write it to csv
shows_allcols %>% 
  write_csv('shows_allcols.csv')








