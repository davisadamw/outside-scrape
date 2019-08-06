library(tidyverse)
library(httr)
library(rvest)

# trying again with SF Chronicle website since the 
website_root <- 'https://datebook.sfchronicle.com/guide/outside-lands-2019-the-ultimate-insiders-guide'

# test 1
t1 <- GET(website_root)

# looks like the interior tag in grabs the body of the article
interior <- html_nodes(content(t1), '.interior')

# extract text from body
int_txt <- html_text(interior, trim = T)

# split text up into lines
all_rows <- str_split(int_txt, '\n')

# and make it a column in a tibble
all_text <- tibble(all_text = all_rows) %>% unnest()

all_text %>% write_csv('extracted_text_raw.csv')



