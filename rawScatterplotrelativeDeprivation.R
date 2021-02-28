library(tidyverse)
library(readxl)

# DATA CLEANING AND RESHAPING
hardship_raw <- read_excel("presentation2.xlsx")
hardship <- hardship_raw %>% 
  select(-index) %>%
  rename(community = Community) %>% 
  pivot_longer(-community,
               values_to = 'raw_score',
               names_to = 'Year') %>% 
  mutate(index_area = str_extract(Year, "[A-Z]+"),
         Year = as.numeric(str_extract(Year, "\\-*\\d+\\.*\\d*"))) %>% 
  select(community, index_area, Year, raw_score) %>% 
  pivot_wider(names_from = c('index_area'),
              values_from = c('raw_score'),
              names_sep = "") %>% 
  mutate(Year = factor(Year, levels = c(14, 17)))

# for plotting and labelling points:
hardship_sub <- hardship %>% 
  mutate(match = 
           community == 'Woodlawn' | 
           community == 'Austin' | 
           community == 'Englewood') %>% 
  mutate(community = if_else(match, community, ""))
hardship_sub2 <- hardship %>% 
  filter(community %in% c("Woodlawn", "Austin", "Englewood"))

# FIGURE 1
ggplot(hardship, aes(x = UNEMP, y = NOHS)) + 
  geom_point(aes(color = Year), alpha = 0.5) + 
  scale_color_manual(values=c("grey40", "red")) + 
  geom_text(data = hardship_sub,
            aes(label = community, color = Year), 
            vjust = -.9,
            show.legend = F) +
  geom_point(data = hardship_sub2,
             shape = 24, 
             size = 2) +
  labs(title = "Unemployment and Education",
       x = "% age 16+ unemployed",
       y = "Age 25+ without high school diploma") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlim(-2, 43)


# FIGURE 2
# restructure data and calculate normalized scores
hardship <- hardship_raw %>% 
  select(-index) %>%
  rename(community = Community) %>% 
  pivot_longer(-community,
               values_to = 'raw_score',
               names_to = 'year') %>% 
  mutate(index_area = str_extract(year, "[A-Z]+"),
         year = as.numeric(str_extract(year, "\\-*\\d+\\.*\\d*"))) %>% 
  filter(index_area != 'HI') %>% 
  group_by(year, index_area) %>% 
  mutate(min = min(raw_score),
         max = max(raw_score)) %>% 
  ungroup()

hardship_other <- hardship %>% 
  filter(index_area != 'INC') %>% 
  mutate(normal_score = 100*(raw_score - min)/(max - min))

hardship_inc <- hardship %>% 
  filter(index_area == 'INC') %>% 
  mutate(normal_score = 100*(max - raw_score)/(max - min))

hardship <- hardship_other %>% 
  full_join(hardship_inc, 
            by = c("community", "year", "raw_score", "index_area",
                   "min", "max", "normal_score")) %>% 
  group_by(community, index_area) %>% 
  mutate(diff_raw = raw_score[year == 17] - raw_score[year == 14],
         diff_normal = normal_score[year == 17] - normal_score[year == 14]) %>% 
  ungroup() %>% 
  mutate(
    raw_change = case_when(
      index_area != 'INC' & diff_raw > 0 ~ 'more_hardship',
      index_area == 'INC' & diff_raw > 0 ~ 'less_hardship',
      index_area != 'INC' & diff_raw < 0 ~ 'less_hardship',
      index_area == 'INC' & diff_raw < 0 ~ 'more_hardship',
      diff_raw == 0 ~ 'neutral'),
    raw_change = factor(raw_change, 
                        levels = c('less_hardship', 
                                   'neutral', 
                                   'more_hardship')),
    normal_change = case_when(
      diff_normal > 0 ~ 'more_hardship',
      diff_normal == 0 ~ 'neutral',
      diff_normal < 0 ~ 'less_hardship'),
    normal_change = factor(normal_change, 
                           levels = c('less_hardship', 
                                      'neutral', 
                                      'more_hardship'))) %>% 
  mutate(rel_dep = case_when(
    raw_change == 'less_hardship' & normal_change == 'more_hardship' ~ 'Yes',
    TRUE                                                             ~ 'No')) %>% 
  arrange(index_area, community, year)

changes <- hardship %>% 
  distinct(community, index_area, diff_raw, diff_normal, .keep_all = T) %>% 
  select(community, year, index_area, diff_raw, diff_normal, raw_change, 
         normal_change, rel_dep)
norm <- changes %>% 
  rename(rel_dep_normal = rel_dep) %>% 
  select(community, index_area, rel_dep_normal)

remove(hardship_other, hardship_inc)

# include work from other group that calculated standardization
stand <- read_excel("rel_dev_standard.xlsx") %>% 
  rename(community = Community) %>% 
  pivot_longer(-community,
               values_to = "rel_dep_standard",
               names_to = "index_area") %>% 
  mutate(rel_dep_standard = if_else(rel_dep_standard == 1,
                                    'Yes',
                                    'No'))
comb <- stand %>% 
  right_join(norm, by = c('community', 'index_area')) %>% # get rid of HI from stand
  arrange(community, index_area) %>% 
  mutate(match = if_else(rel_dep_standard == rel_dep_normal,
                         'agree',
                         'disagree'))

for_plot <- comb %>% 
  filter(index_area != "HI", index_area != "HOM") %>% 
  full_join(hardship_raw %>% 
              rename(community = Community) %>% 
              select(community, HI14, HI17),
            by = c('community')) %>% 
  mutate(match_num = if_else(match == 'agree',1,0)) %>% 
  group_by(community, HI14, HI17) %>% 
  summarise(match = sum(match_num)) %>% 
  ungroup() %>% 
  mutate(match_bin = case_when(
    match == 6 ~ 'Match in All Areas',
    match == 5 ~ 'Match in 5/6 Areas',
    match == 4 ~ 'Match in 4/6 Areas',
    match == 3 ~ 'Match in 3/6 Areas',
    match == 2 ~ 'Match in 2/6 Areas')) %>% 
  mutate(match_bin = case_when(
    match <= 4 ~ 
      'Mismatches (4 or less indicators match)',
    match > 4 ~ 
      'Matches (at least 5 out of 6 indicators match)')) %>% 
  mutate(match_bin = factor(match_bin, 
                            levels = c('Mismatches (4 or less indicators match)',
                                       'Matches (at least 5 out of 6 indicators match)')))

ggplot(for_plot %>% 
         rename(" " = match_bin), 
       aes(x = HI14, fill = ` `)) +
  geom_histogram(binwidth = 10) +
  scale_fill_manual(values = c('skyblue3','gray75')) +
  # scale_x_continuous(limits = c(0,90)) +
  labs(x = 'Hardship Index 2014',
       y = 'Number of Communities',
       title = 'Comparison of Standardization and Normalization Techniques in Relative Deprivation Analysis') + 
  theme(plot.title = element_text(hjust = 0.5))



