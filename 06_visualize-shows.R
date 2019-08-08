library(tidyverse)
library(googledrive)

source('99_freaky-time-function.R')

# read show info with all time columns
shows_alltimes <- read_rds('shows_all_time_data.rds')

# load who we wanna see
wwws <- readxl::read_excel('OSL-2019.xlsx') %>%
  select(artist, ymn:comments) %>% 
  mutate(`See Em` = if_else(is.na(ymn), 'Unk', ymn) %>% 
           fct_relevel('Unk', 'No', 'Maybe', 'Yes'),
         see_em_size = case_when(ymn == 'No'    ~ 0.5,
                                 ymn == 'Maybe' ~ 1,
                                 ymn == 'Yes'   ~ 1,
                                 TRUE           ~ 0.5))


color_scale = c('Yes'   = 'Green',
                'Maybe' = 'Yellow',
                'No'    = 'Red',
                'Unk'   = '#505050')

# long format, only cols we want
shows_long <- shows_alltimes %>% 
  select(artist, stage, date, start_time, end_time) %>% 
  left_join(wwws, by = 'artist') %>% 
  gather('set', 'time', ends_with('time')) %>% 
  mutate(time_hm  = hm_safe(time) + hours(12),
         time_dhm = time_from_noon(time, lubridate::parse_date_time(date, '%A, %b. %d')),
         Stage = str_replace(stage, ' Stage', '') %>% 
           fct_relevel('GastroMagic', 'Sutro', 'Lands End', 'Panhandle', 'Twin Peaks')) 


# plot with path
shows_long %>%
  ggplot(aes(x = time_hm, y = Stage, 
             group = artist, color = `See Em`, size = 4*see_em_size)) +
  scale_x_time(labels = lt12p) + 
  scale_size_identity() +
  scale_color_manual(values = color_scale, guide = 'none') +
  facet_grid(rows = vars(date), scales = 'free') +
  geom_path() +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 15))

ggsave('t1.png')
