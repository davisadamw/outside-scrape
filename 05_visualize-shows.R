library(tidyverse)

source('99_freaky-time-function.R')

# read show info with all time columns
shows_alltimes <- read_rds('shows_all_time_data.rds')

# long format, only cols we want
shows_long <- shows_alltimes %>% 
  select(artist, stage, date, start_time, end_time) %>% 
  gather('set', 'time', ends_with('time')) %>% 
  mutate(time_hm  = hm_safe(time) + hours(12),
         time_dhm = time_from_noon(time, lubridate::parse_date_time(date, '%A, %b. %d')),
         Stage = str_replace(stage, ' Stage', '') %>% 
           fct_relevel('Lands End', 'Sutro', 'Twin Peaks', 'Panhandle')) 


# plot with path
shows_long %>%
  ggplot(aes(x = time_hm, y = Stage, group = artist)) +
  scale_x_time(labels = lt24) + 
  facet_grid(rows = vars(date), scales = 'free_x') +
  geom_path() +
  theme(axis.title.x = element_blank())

