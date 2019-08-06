library(tidyverse)

# read raw text
raw_text <- read_csv('extracted_text_raw.csv')

# first, just because p.m. is a nightmare for regex, replace it with PM ... do the same to apostrophe in Sandy's
raw_text_nopm <- raw_text %>% 
  mutate(all_text = str_replace_all(all_text, 'p\\.m\\.', 'PM'),
         all_text = str_replace(all_text, 'Sandy.s', 'Sandys'))

# remove rows above  'Friday, Aug. 9' or below last row that ends with 'PM'
first_keeper_text <- 'Friday, Aug. 9'

stages <- c('Lands End', 'Sutro', 'Twin Peaks', 'Panhandle') %>% paste('Stage')
# basically a big series of or's
stage_regex <- paste0('(', paste(stages, collapse = '|'), ')')

raw_text_goodrows <- raw_text_nopm %>% 
  # first, remove all rows before 'Friday Aug 9...'
  filter(cumany(all_text == first_keeper_text)) %>% 
  # then reverse sort and remove all rows before the last one that ends with a number, then PM
  mutate(row_num = row_number()) %>% 
  arrange(-row_num) %>% 
  filter(cumany(str_ends(all_text, '[0-9] PM'))) %>% 
  # and return to original order
  arrange(row_num)

# next, pull out dates and stage id's
text_dates_and_stages <- raw_text_goodrows %>% 
  # this is ... some freaky regexing
  mutate(date  = str_extract(all_text,'(Fri|Satur|Sun)day, Aug\\. [019]{1,2}'),
         stage = str_extract(all_text, stage_regex),
         date_num  = cumsum(!is.na(date)),
         stage_num = cumsum(!is.na(stage)))

# get the index for each date/stage
all_dates  <- text_dates_and_stages %>% filter(!is.na(date))  %>% select(date, date_num)
all_stages <- text_dates_and_stages %>% filter(!is.na(stage)) %>% select(stage, stage_num)

# drop the rows that have date/stage info and apply the dates to the rows that don't!
shows_only <- text_dates_and_stages %>% 
  filter(is.na(date), is.na(stage)) %>% 
  select(-date, -stage) %>% 
  left_join(all_dates,  by = 'date_num') %>% 
  left_join(all_stages, by = 'stage_num') %>% 
  select(-ends_with('num'))

# and save the show rows to disk ... at this point the only issue is the small number of unseparated rows
shows_only %>% write_csv('shows_only.csv')

