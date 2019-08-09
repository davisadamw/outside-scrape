library(tidyverse)

source('99_freaky-time-function.R')

# read show info with all time columns
shows_alltimes <- read_rds('shows_all_time_data.rds')

# load who we wanna see
wwws <- readxl::read_excel('OSL-2019.xlsx') %>%
  select(artist, artist_simp, ymn:comments) %>% 
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
  mutate(time_hm  = hm_safe(time),
         time_minute = 60 * hour(time_hm) + minute(time_hm),
         Stage = str_replace(stage, ' Stage', '') %>% 
           fct_relevel('GastroMagic', 'Sutro', 'Lands End', 'Panhandle', 'Twin Peaks')) 

# and the text positioning
shows_text <- shows_long %>% 
  filter(`See Em` == 'Yes' | `See Em` == 'Maybe') %>% 
  select(artist, artist_simp, Stage, `See Em`, date, time_minute) %>% 
  group_by(artist, artist_simp, Stage, `See Em`, date) %>% 
  summarise(time_minute = mean(time_minute))

# plot with path
shows_long %>%
  ggplot(aes(x = time_minute, y = Stage, 
             group = artist, color = `See Em`, size = 4*see_em_size)) +
  scale_x_continuous(breaks = seq(0, 600, 60), minor_breaks = seq(0, 600, 20),
                     limits = c(-10, 610), expand = c(0,0),
                     labels = c('Noon', paste(seq(1,10), '00', sep= ':'))) + 
  scale_size_identity() +
  scale_color_manual(values = color_scale, guide = 'none') +
  facet_grid(rows = vars(date), scales = 'free') +
  geom_path() +
  geom_text(aes(label=artist_simp), data = shows_text,
            color = 'black', size = 3, nudge_y = 0.3) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 11, angle = 30),
        axis.text.x = element_text(size = 11))

ggsave('OSL_outline.png', width=7, height=7)

# plot with path FRIDAY
shows_long %>%
  filter(str_detect(date, 'Fri')) %>% 
  ggplot(aes(x = time_minute, y = Stage, 
             group = artist, color = `See Em`, size = 4*see_em_size)) +
  scale_x_continuous(breaks = seq(0, 600, 60), minor_breaks = seq(0, 600, 20),
                     limits = c(-10, 610), expand = c(0,0),
                     labels = c('Noon', paste(seq(1,10), '00', sep= ':'))) + 
  scale_size_identity() +
  scale_color_manual(values = color_scale, guide = 'none') +
  facet_grid(rows = vars(date), scales = 'free') +
  geom_path() +
  geom_text(aes(label=artist_simp), data = filter(shows_text, str_detect(date, 'Fri')),
            color = 'black', size = 3, nudge_y = 0.3) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 11, angle = 30),
        axis.text.x = element_text(size = 11))

ggsave('OSL_friday.png', width=7, height=3)

# plot with path FRIDAY
shows_long %>%
  filter(str_detect(date, 'Sat')) %>% 
  ggplot(aes(x = time_minute, y = Stage, 
             group = artist, color = `See Em`, size = 4*see_em_size)) +
  scale_x_continuous(breaks = seq(0, 600, 60), minor_breaks = seq(0, 600, 20),
                     limits = c(-10, 610), expand = c(0,0),
                     labels = c('Noon', paste(seq(1,10), '00', sep= ':'))) + 
  scale_size_identity() +
  scale_color_manual(values = color_scale, guide = 'none') +
  facet_grid(rows = vars(date), scales = 'free') +
  geom_path() +
  geom_text(aes(label=artist_simp), data = filter(shows_text, str_detect(date, 'Sat')),
            color = 'black', size = 3, nudge_y = 0.3) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 11, angle = 30),
        axis.text.x = element_text(size = 11))

ggsave('OSL_Saturday.png', width=7, height=3)

# plot with path FRIDAY
shows_long %>%
  filter(str_detect(date, 'Sun')) %>% 
  ggplot(aes(x = time_minute, y = Stage, 
             group = artist, color = `See Em`, size = 4*see_em_size)) +
  scale_x_continuous(breaks = seq(0, 600, 60), minor_breaks = seq(0, 600, 20),
                     limits = c(-10, 610), expand = c(0,0),
                     labels = c('Noon', paste(seq(1,10), '00', sep= ':'))) + 
  scale_size_identity() +
  scale_color_manual(values = color_scale, guide = 'none') +
  facet_grid(rows = vars(date), scales = 'free') +
  geom_path() +
  geom_text(aes(label=artist_simp), data = filter(shows_text, str_detect(date, 'Sun')),
            color = 'black', size = 3, nudge_y = 0.3) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(size = 11, angle = 30),
        axis.text.x = element_text(size = 11))

ggsave('OSL_Sunday.png', width=7, height=3)
