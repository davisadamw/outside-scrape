library(tidyverse)
library(lubridate)


# parse time with exceptions for full hours, 'noon'
# can set any maximum allowed time, but likely 12 or 24 hours
hm_safe <- function(hm_text, max_hour = 12) {
  
  hm_text_clean <- case_when(
    # case 1: noon stored as text
    hm_text == 'noon'         ~ '0:00',
    # case 2: whole hour, no minutes
    !str_detect(hm_text, ':') ~ str_c(hm_text, '00', sep=':'),
    # conventional case, 
    TRUE                      ~ hm_text)
  
  # separate hours and matrix
  hm_matrix <- str_split_fixed(hm_text_clean, pattern = ':', n = 2)
  
  # make a big tibble of time, fix hours over limit
  hm_tibble <- tibble(h = as.integer(hm_matrix[,1]),
                      m = as.integer(hm_matrix[,2])) %>% 
    # fix hours over limit
    mutate(h = if_else(h < max_hour, h, as.integer(h %% max_hour)),
           # convert to period for returning
           time_period = hours(h) + minutes(m))
  
  return(pull(hm_tibble, time_period))
  
}


# function to combine date and time
time_from_noon <- function(time_text, date, PM=TRUE) {
  
  # first replace noon with 0:00
  time_c1 <- hm_safe(if_else(time_text == 'noon', '0:00', time_text))
  
  # then combine it with the date time object, adjusting for AM-PM as necessary
  if (PM) return(date + hours(12) + time_c1)
  else    return(date + time_c1)
}


# and a couple labeling functions
# time label format
lt24 <- function(ts) {
  
  label_init <- paste(hour(ts), 
                      str_pad(minute(ts), 2, 'left', '0'), sep = ':')

  label_upd  <- case_when(label_init ==  '0:00' ~ 'Midnight',
                          label_init == '12:00' ~ 'Noon',
                          TRUE                  ~ label_init)
  
  return(label_upd)
}

lt12p <- function(ts) {
  
  label_init <- paste(hour(ts) %% 12, 
                      str_pad(minute(ts), 2, 'left', '0'), sep = ':')
  
  label_upd  <- case_when(label_init ==  '0:00' ~ 'Noon',
                          TRUE                  ~ label_init)
  
  return(label_upd)
}


