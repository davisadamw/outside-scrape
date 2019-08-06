library(tidyverse)
library(lubridate)

source('99_freaky-time-function.R')

# load shows
shows_allcols <- read_csv('shows_allcols.csv')


# get a bit more of ~ start/end time info
shows_showtime <- shows_allcols %>% 
  separate(time, c('start_time', 'end_time'), sep = '-', remove = FALSE) %>% 
  mutate(end_time = str_replace(end_time, ' PM', ''),
         date_dmy = lubridate::parse_date_time(date, '%A, %b. %d')) %>% 
  mutate_at(vars(ends_with('_time')), ~ time_from_noon(., date_dmy)) %>% 
  mutate(show_interval = interval(start_time, end_time))
            
         #    
         #    ,
         # start_time_l = if_else(hm(start_time),
         # end_time_l   = hm(end_time),
         # start_dt = date_dmy + hours(12) + start_time_l)
         # 
