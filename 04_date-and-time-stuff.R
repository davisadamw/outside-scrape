library(tidyverse)
library(lubridate)

source('99_freaky-time-function.R')

# load shows
shows_allcols <- read_csv('shows_allcols.csv')

# manually add BA test kitchen events
ba_alltimes <- tribble(~artist,                        ~stage,        ~date,               ~time,
                       'Back-to-Back with Molly Baz',  'GastroMagic', 'Saturday, Aug. 10', '1:55-2:25 PM',
                       'Big Wild (MCed by AR)',        'GastroMagic', 'Saturday, Aug. 10', '3:10-3:40 PM',
                       'Slurpers (CupcakKe and Andy)', 'GastroMagic', 'Saturday, Aug. 10', '4:25-4:55 PM',
                       'Test Kitchen Live (errebody)', 'GastroMagic', 'Saturday, Aug. 10', '5:45-6:15 PM')


# get a bit more of ~ start/end time info
shows_alltimes <- shows_allcols %>% 
  bind_rows(ba_alltimes) %>% # attach the ba shows at the bottom
  separate(time, c('start_time', 'end_time'), sep = '-', remove = FALSE) %>% 
  mutate(end_time = str_replace(end_time, ' PM', ''),
         date_dmy = lubridate::parse_date_time(date, '%A, %b. %d')) %>% 
  mutate_at(vars(ends_with('_time')), 
            list(to = hm_safe,
                 dt = ~ time_from_noon(., date_dmy))) %>% 
  mutate(show_interval = interval(start_time_dt, end_time_dt))

# write to disk
shows_alltimes %>% 
  write_rds('shows_all_time_data.rds')






