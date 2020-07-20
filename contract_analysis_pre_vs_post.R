library(tidyverse)
library(stringr)
library(scales)

data <- read.csv("data/NFL_10yr_contracts.csv", sep = ";")
data <- data %>% rename("Name" = ď.żName)
View(data)

# clean up year col
data$Year <- str_replace_all(data$Year, "[^0-9]", "")
data$Year <- as.numeric(data$Year)

# check str
str(data)
# convert factors to numerics
names(data)[ sapply(data, is.factor) ]
to_convert <- c("Start.", "Win.", "Cmp.")
data[to_convert] <- lapply(data[to_convert], function(x) as.numeric(str_replace_all(x, "[^0-9]", "")) / 100)

# per game stats
data$TD_thrown_per_game <- data$TD / data$G
data$Int_thrown_per_game <- data$Int / data$G
data$TD_ran_per_game <- data$Rush.TDs / data$G
data$FirstD_pass_per_game <- data$X1D / data$G
data$FirstD_run_per_game <- data$Rush.1D / data$G
data$pass_attempts_per_game <- data$Att / data$G
data$pass_to_rush_attempts <- data$pass_attempts_per_game / data$Rush.A.G


# before after avgs

pass <- data %>% 
  select(Name, Post_Contract, TD_thrown_per_game, Int_thrown_per_game, Win., Cmp., Y.G, FirstD_pass_per_game, pass_attempts_per_game) %>% 
  rename("TDs thrown / game" = TD_thrown_per_game,
         "Ints thrown / game" = Int_thrown_per_game, 
         "Pass attempts / game" = pass_attempts_per_game,
         "Completion %" = Cmp.,
         "Yards / game" = Y.G,
         '1st down passes / game' = FirstD_pass_per_game) %>% 
  group_by(Name, Post_Contract) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Stats", "Values", "Yards / game", "Completion %" , "TDs thrown / game", "Ints thrown / game", '1st down passes / game', "Pass attempts / game")

pass %>% 
  ggplot(aes(Post_Contract, Values, color = Name, fill = Name, shape = Name)) +
  #geom_line(size = 1/3, linetype = 'dotted') +
  geom_point(size = 2.5) +
  geom_point(data = pass %>% filter(Name == "Patrick Mahomes"), aes(Post_Contract, Values), size=5, show.legend = F) +
  geom_line(linetype = 'dotted', size = 0.8) +
  facet_wrap(~Stats, scales = 'free_y') +
  labs(title = "Pass stats per season before and after long-term contract",
       x = NULL,
       y = NULL) +
  scale_x_continuous( limits = c(-0.3, 1.3), breaks = c(0, 1), labels = c("Pre-contract", "Post-contract")) +
  theme_bw() 

ggsave("charts/pass_stats.png")




misc <- data %>% 
  select(Name, Post_Contract, Yds, TD, Int, Win., Cmp., Rate, Y.G, X4QC, GWD) %>% 
  rename("Win pctage / season" = Win.,
         "QB rating / season" = Rate,
         "4th quarter comebacks / season" = X4QC,
         "Game winning drives / season" = GWD) %>% 
  group_by(Name, Post_Contract) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Stats", "Values", "Win pctage / season", "QB rating / season", "4th quarter comebacks / season", "Game winning drives / season")

misc %>% 
  ggplot(aes(Post_Contract, Values, color = Name, fill = Name, shape = Name)) +
  #geom_line(size = 1/3, linetype = 'dotted') +
  geom_point(size = 2.5) +
  geom_point(data = misc %>% filter(Name == "Patrick Mahomes"), aes(Post_Contract, Values), size=5, show.legend = F) +
  geom_line(linetype = 'dotted', size = 0.8) +
  facet_wrap(~Stats, scales = 'free_y') +
  labs(title = "Misc stats per season before and after long-term contract",
       x = NULL,
       y = NULL) +
  scale_x_continuous( limits = c(-0.3, 1.3), breaks = c(0, 1), labels = c("Pre-contract", "Post-contract")) +
  theme_bw() 

ggsave("charts/misc_stats.png")




rush <- data %>% 
  select(Name, Post_Contract, Rush.Y.A, Rush.A.G, Rush.Y.G, TD_ran_per_game, FirstD_run_per_game, pass_to_rush_attempts) %>% 
  rename("Rush yards / attempt" = Rush.Y.A, 
         "Rush attempts / game" = Rush.A.G,
         "Rush yards / game" = Rush.Y.G,
         "Rush TDs / game" = TD_ran_per_game,
         "1st down rushes / game" = FirstD_run_per_game,
         "Pass att. to rush att. / game" = pass_to_rush_attempts) %>% 
  group_by(Name, Post_Contract) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Stats", "Values", "Rush yards / attempt", "Rush attempts / game", "Rush yards / game", "Rush TDs / game", "1st down rushes / game", "Pass att. to rush att. / game")

rush %>% 
  ggplot(aes(Post_Contract, Values, color = Name, fill = Name, shape = Name)) +
  #geom_line(size = 1/3, linetype = 'dotted') +
  geom_point(size = 2.5) +
  geom_point(data = rush %>% filter(Name == "Patrick Mahomes"), aes(Post_Contract, Values), size=5, show.legend = F) +
  geom_line(linetype = 'dotted', size = 0.8) +
  facet_wrap(~Stats, scales = 'free_y') +
  labs(title = "Rush stats per season before and after long-term contract",
       x = NULL,
       y = NULL) +
  scale_x_continuous( limits = c(-0.3, 1.3), breaks = c(0, 1), labels = c("Pre-contract", "Post-contract")) +
  theme_bw() 

ggsave("charts/rush_stats.png")


post_season <- data %>% 
  select(Name, Post_Contract, Division.championships, P.O.appearances, SB.appearances, SB.wins) %>% 
  rename("% of seasons with division championships" = Division.championships, 
         "% of seasons with PO appearances" = P.O.appearances,
         "% of seasons with SB appearances" = SB.appearances,
         "% of seasons with SB wins" = SB.wins) %>% 
  group_by(Name, Post_Contract) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Stats", "Values", "% of seasons with division championships", "% of seasons with PO appearances", "% of seasons with SB appearances", "% of seasons with SB wins")


post_season %>% 
  #mutate(Values = 1 / Values) %>% 
  ggplot(aes(Post_Contract, Values, color = Name, fill = Name, shape = Name)) +
  #geom_line(size = 1/3, linetype = 'dotted') +
  geom_point(size = 2.5) +
  geom_point(data = post_season %>% filter(Name == "Patrick Mahomes"), aes(Post_Contract, Values), size=5, show.legend = F) +
  geom_line(linetype = 'dotted', size = 0.8) +
  facet_wrap(~Stats, scales = 'free_y') +
  labs(title = "Post season stats per season before and after long-term contract",
       x = NULL,
       y = NULL) +
  scale_x_continuous( limits = c(-0.3, 1.3), breaks = c(0, 1), labels = c("Pre-contract", "Post-contract")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  theme_bw() 

ggsave("charts/post_season_stats.png")


# before years and after years analysis

# TODO make first year of contract year 0
# take 3 years before and 3 after
# compare trend


trend <- data %>% 
  group_by(Name) %>% 
  mutate("year0" = ifelse((Post_Contract != lag(Post_Contract)) & (Post_Contract != first(Post_Contract)), 1, 0),
         "year1" = lag(year0),
         "year2" = lag(year1),
         "year3" = lag(year2),
         "year-1" = lead(year0),
         "year-2"= lead(`year-1`),
         "year-3" = lead(`year-2`)) %>% 
  ungroup() %>% 
  filter((year0 == 1) | (year1 == 1) |(year2 == 1) |(year3 == 1) |(`year-1` == 1) |(`year-2` == 1) |(`year-3` == 1) | (Name == 'Patrick Mahomes')) %>%
  mutate(year1 = ifelse(is.na(year1), 0, year1),
         year2 = ifelse(is.na(year2), 0, year2),
         year3 = ifelse(is.na(year3), 0, year3)) %>% 
  mutate(year = ifelse(year0 == 1, 0, 
                       ifelse(year1 == 1, 1, 
                              ifelse(year2 == 1, 2, 
                                     ifelse(year3 == 1, 3, 
                                            ifelse(`year-1` == 1, -1, 
                                                   ifelse(`year-2` == 1, -2, -3))))))) %>% 
  mutate(year = ifelse((Name == 'Patrick Mahomes') & (Year == 2018), -2, 
                       ifelse((Name == 'Patrick Mahomes') & (Year == 2019), -1, year)))

trend[is.na(trend)] <- 0

post_season_trend <- trend %>% 
  select(Name, Post_Contract, year, Division.championships, P.O.appearances, SB.appearances, SB.wins) %>% 
  rename("Div championships / season" = Division.championships, 
         "PO appearances / season" = P.O.appearances,
         "SB appearances / season" = SB.appearances,
         "SB wins / season" = SB.wins) %>% 
  gather("Stats", "Values", "Div championships / season", "PO appearances / season", "SB appearances / season", "SB wins / season")


post_season_trend %>% 
  ggplot(aes(year, Values, color = Name, fill = Name, shape = Name)) +
  geom_point(size = 2.5) +
  geom_line(linetype = 'dotted', size = 0.8) +
  facet_wrap(~Stats, scales = 'free') +
  labs(title = "Post season stats per season before and after long-term contract",
       x = NULL,
       y = NULL) +
  theme_bw() 


rush_trend <- trend %>% 
  select(Name, year, Post_Contract, Rush.Y.A, Rush.A.G, Rush.Y.G, TD_ran_per_game, FirstD_run_per_game, pass_to_rush_attempts) %>% 
  rename("Rush yards / attempt" = Rush.Y.A, 
         "Rush attempts / game" = Rush.A.G,
         "Rush yards / game" = Rush.Y.G,
         "Rush TDs / game" = TD_ran_per_game,
         "1st down rushes / game" = FirstD_run_per_game,
         "Pass att. to rush att. / game" = pass_to_rush_attempts) %>% 
  gather("Stats", "Values", "Rush yards / attempt", "Rush attempts / game", "Rush yards / game", "Rush TDs / game", "1st down rushes / game", "Pass att. to rush att. / game")

rush_trend %>% 
  filter(!Name %in% c("Brett Favre", "Drew Bledsoe")) %>% 
  ggplot(aes(year, Values, color = Name, fill = Name, shape = Name)) +
  geom_point(size = 2.5) +
  geom_line(linetype = 'dotted', size = 0.8) +
  facet_wrap(~Stats, scales = 'free') +
  labs(title = "Rush stats around contract signing year 0",
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(-3,3,1)) +
  theme_bw() 

ggsave("charts/rush_stats_trend.png")


pass_trend <- trend %>% 
  select(Name, year, Post_Contract, TD_thrown_per_game, Int_thrown_per_game, Win., Cmp., Y.G, FirstD_pass_per_game, pass_attempts_per_game) %>% 
  rename("TDs thrown / game" = TD_thrown_per_game,
         "Ints thrown / game" = Int_thrown_per_game, 
         "Pass attempts / game" = pass_attempts_per_game,
         "Completion %" = Cmp.,
         "Yards / game" = Y.G,
         '1st down passes / game' = FirstD_pass_per_game) %>% 
  gather("Stats", "Values", "Yards / game", "Completion %" , "TDs thrown / game", "Ints thrown / game", '1st down passes / game', "Pass attempts / game")

pass_trend %>% 
  ggplot(aes(year, Values, color = Name, fill = Name, shape = Name)) +
  geom_point(size = 2.5) +
  geom_line(linetype = 'dotted', size = 0.8) +
  facet_wrap(~Stats, scales = 'free') +
  labs(title = "Pass stats around contract signing year 0",
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(-3,3,1)) +
  theme_bw() 

ggsave("charts/pass_stats_trend.png")


misc_trend <- trend %>% 
  select(Name, year, Post_Contract, Yds, TD, Int, Win., Cmp., Rate, Y.G, X4QC, GWD) %>% 
  rename("Win pctage / season" = Win.,
         "QB rating / season" = Rate,
         "4th quarter comebacks / season" = X4QC,
         "Game winning drives / season" = GWD) %>% 
  gather("Stats", "Values", "Win pctage / season", "QB rating / season", "4th quarter comebacks / season", "Game winning drives / season")

misc_trend %>% 
  ggplot(aes(year, Values, color = Name, fill = Name, shape = Name)) +
  geom_point(size = 2.5) +
  geom_line(linetype = 'dotted', size = 0.8) +
  facet_wrap(~Stats, scales = 'free') +
  labs(title = "Misc stats around contract signing year 0",
       x = NULL,
       y = NULL) +
  scale_x_continuous(breaks = seq(-3,3,1)) +
  theme_bw() 

ggsave("charts/misc_stats_trend.png")





# run all time series for Brett Favre

data$Post_Contract <- ifelse(data$Post_Contract == 0, "Pre", "Post")

# passing stats
data %>% 
  select(Name, Year, Post_Contract, TD, Int, Cmp., Y.G, X1D) %>% 
  rename("TDs / season" = TD,
         "Ints / season" = Int, 
         "Completion % / season" = Cmp.,
         "Yards / game" = Y.G,
         '1st down passing / season' = X1D) %>% 
  gather("Stats", "Values", "Yards / game", "Completion % / season" , "TDs / season", "Ints / season", '1st down passing / season') %>% 
  filter(Name == "Brett Favre") %>% 
  ggplot(aes(Year, Values, color = Post_Contract, fill = Post_Contract, shape = Post_Contract)) +
  geom_point(size = 2.5, show.legend = F) +
  geom_line(linetype = 'dotted', size = 0.8, show.legend = F) +
  facet_wrap(~Stats, scales = 'free') +
  labs(title = "Pass stats trend for Brett Favre",
       x = NULL,
       y = NULL) +
  theme_bw() 

ggsave("charts/favre_pass_trend.png")
