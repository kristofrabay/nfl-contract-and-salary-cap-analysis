library(tidyverse)
library(stringr)
library(scales)

data <- read.csv("data_salary_cap/data/salary_caps.csv", sep = ";")
data <- data %>% rename("Year" = ď.żYear)
View(data)

str(data)

to_convert <- names(data)[ sapply(data, is.factor) ][2:14]
data[to_convert] <- lapply(data[to_convert], function(x) as.numeric(str_replace_all(x, "[^0-9]", "")))
data[to_convert] <- NULL # not needed features for now

to_factor <- c("SB_victory", "SB_appearance", "Division_championship", "Playoff_appearance")
data[to_factor] <- lapply(data[to_factor], function(x) factor(x))

str(data)

# averages by binaries

data %>% 
  filter(Year != 2020) %>% 
  select(-Year, -Team, -Total_pct, -Division_championship, -SB_appearance, -SB_victory) %>% 
  group_by(Playoff_appearance) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather('Position', 'Percentage_spent', QB_pct:Defense_pct) %>% 
  ggplot(aes(reorder(Position, Percentage_spent), Percentage_spent, color = Playoff_appearance, fill = Playoff_appearance)) +
  geom_bar(stat="identity", width = 0.75, position = "dodge", ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), breaks = seq(0, 0.7, 0.05)) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

data %>% 
  filter(Year != 2020) %>% 
  select(-Year, -Team, -Total_pct, -Playoff_appearance, -SB_appearance, -SB_victory) %>% 
  group_by(Division_championship) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather('Position', 'Percentage_spent', QB_pct:Defense_pct) %>% 
  ggplot(aes(reorder(Position, Percentage_spent), Percentage_spent, color = Division_championship, fill = Division_championship)) +
  geom_bar(stat="identity", width = 0.75, position = "dodge", ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), breaks = seq(0, 0.7, 0.05)) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

data %>% 
  filter(Year != 2020) %>% 
  select(-Year, -Team, -Total_pct, -Division_championship, -Playoff_appearance, -SB_victory) %>% 
  group_by(SB_appearance) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather('Position', 'Percentage_spent', QB_pct:Defense_pct) %>% 
  ggplot(aes(reorder(Position, Percentage_spent), Percentage_spent, color = SB_appearance, fill = SB_appearance)) +
  geom_bar(stat="identity", width = 0.75, position = "dodge", ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), breaks = seq(0, 0.7, 0.05)) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

data %>% 
  filter(Year != 2020) %>% 
  select(-Year, -Team, -Total_pct, -Division_championship, -SB_appearance, -Playoff_appearance) %>% 
  group_by(SB_victory) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather('Position', 'Percentage_spent', QB_pct:Defense_pct) %>% 
  ggplot(aes(reorder(Position, Percentage_spent), Percentage_spent, color = SB_victory, fill = SB_victory)) +
  geom_bar(stat="identity", width = 0.75, position = "dodge", ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), breaks = seq(0, 0.7, 0.05)) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


# trend by offense / defense

data %>% 
  filter(Year != 2020) %>% 
  select(Year, Offense_pct, Defense_pct, Playoff_appearance) %>% 
  group_by(Year, Playoff_appearance) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Position", "Spending", Offense_pct, Defense_pct) %>% 
  ggplot(aes(Year, Spending, color = Playoff_appearance)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2013, 2019, 1)) +
  facet_grid(rows = vars(Position), scales ='free') +
  theme_bw() 


data %>% 
  filter(Year != 2020) %>% 
  select(Year, Offense_pct, Defense_pct, SB_appearance) %>% 
  group_by(Year, SB_appearance) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Position", "Spending", Offense_pct, Defense_pct) %>% 
  ggplot(aes(Year, Spending, color = SB_appearance)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2013, 2019, 1)) +
  facet_grid(rows = vars(Position), scales ='free') +
  theme_bw() 


data %>% 
  filter(Year != 2020) %>% 
  select(Year, Offense_pct, Defense_pct, SB_victory) %>% 
  group_by(Year, SB_victory) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Position", "Spending", Offense_pct, Defense_pct) %>% 
  ggplot(aes(Year, Spending, color = SB_victory)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2013, 2019, 1)) +
  facet_grid(rows = vars(Position), scales ='free') +
  theme_bw()


# trend by positions

data %>% 
  filter(Year != 2020) %>% 
  select(-Offense_pct, -Defense_pct, -Team, -Total_pct, -SB_appearance, -Division_championship, -SB_victory) %>% 
  group_by(Year, Playoff_appearance) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Position", "Spending", QB_pct:CB_pct) %>% 
  ggplot(aes(Year, Spending, color = Playoff_appearance)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2013, 2019, 1)) +
  facet_wrap(~Position, scales ='free_y') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

data %>% 
  filter(Year != 2020) %>% 
  select(-Offense_pct, -Defense_pct, -Team, -Total_pct, -Playoff_appearance, -Division_championship, -SB_victory) %>% 
  group_by(Year, SB_appearance) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Position", "Spending", QB_pct:CB_pct) %>% 
  ggplot(aes(Year, Spending, color = SB_appearance)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2013, 2019, 1)) +
  facet_wrap(~Position, scales ='free_y') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

data %>% 
  filter(Year != 2020) %>% 
  select(-Offense_pct, -Defense_pct, -Team, -Total_pct, -Playoff_appearance, -Division_championship, -SB_appearance) %>% 
  group_by(Year, SB_victory) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Position", "Spending", QB_pct:CB_pct) %>% 
  ggplot(aes(Year, Spending, color = SB_victory)) + 
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = seq(2013, 2019, 1)) +
  facet_wrap(~Position, scales ='free_y') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

