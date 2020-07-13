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


# before after avgs

pass <- data %>% 
  select(Name, Post_Contract, Yds, TD, Int, Win., Cmp., Rate, Y.G, X1D) %>% 
  rename("TDs / season" = TD,
         "Ints / season" = Int, 
         "Completion % / season" = Cmp.,
         "Yards / game" = Y.G,
         '1st down passing / season' = X1D) %>% 
  group_by(Name, Post_Contract) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Stats", "Values", "Yards / game", "Completion % / season" , "TDs / season", "Ints / season", '1st down passing / season')

pass %>% 
  ggplot(aes(Post_Contract, Values, color = Name, fill = Name, shape = Name)) +
  #geom_line(size = 1/3, linetype = 'dotted') +
  geom_point(size = 2.5) +
  geom_point(data = pass %>% filter(Name == "Patrick Mahomes"), aes(Post_Contract, Values), size=5, show.legend = F) +
  geom_line(linetype = 'dotted', size = 0.8) +
  facet_wrap(~Stats, scales = 'free') +
  labs(title = "Pass stats per season before and after long-term contract",
       x = NULL,
       y = NULL) +
  scale_x_continuous( limits = c(-0.3, 1.3), breaks = c(0, 1), labels = c("Pre", "Post")) +
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
  facet_wrap(~Stats, scales = 'free') +
  labs(title = "Misc stats per season before and after long-term contract",
       x = NULL,
       y = NULL) +
  scale_x_continuous( limits = c(-0.3, 1.3), breaks = c(0, 1), labels = c("Pre", "Post")) +
  theme_bw() 

ggsave("charts/misc_stats.png")




rush <- data %>% 
  select(Name, Post_Contract, Rush.Y.G, Rush.A.G, Rush.TDs, Rush, Rush.1D) %>% 
  rename("Yards / game" = Rush.Y.G, 
         "Yards / attempt" = Rush.A.G,
         "TDs / season" = Rush.TDs,
         "Attempts / season" = Rush, 
         "1st down rushes / season" = Rush.1D) %>% 
  group_by(Name, Post_Contract) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Stats", "Values", "Yards / game", "Yards / attempt", "TDs / season", "Attempts / season", "1st down rushes / season")

rush %>% 
  ggplot(aes(Post_Contract, Values, color = Name, fill = Name, shape = Name)) +
  #geom_line(size = 1/3, linetype = 'dotted') +
  geom_point(size = 2.5) +
  geom_point(data = rush %>% filter(Name == "Patrick Mahomes"), aes(Post_Contract, Values), size=5, show.legend = F) +
  geom_line(linetype = 'dotted', size = 0.8) +
  facet_wrap(~Stats, scales = 'free') +
  labs(title = "Rush stats per season before and after long-term contract",
       x = NULL,
       y = NULL) +
  scale_x_continuous( limits = c(-0.3, 1.3), breaks = c(0, 1), labels = c("Pre", "Post")) +
  theme_bw() 

ggsave("charts/rush_stats.png")


post_season <- data %>% 
  select(Name, Post_Contract, Division.championships, P.O.appearances, SB.appearances, SB.wins) %>% 
  rename("Div championships / season" = Division.championships, 
         "PO appearances / season" = P.O.appearances,
         "SB appearances / season" = SB.appearances,
         "SB wins / season" = SB.wins) %>% 
  group_by(Name, Post_Contract) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  gather("Stats", "Values", "Div championships / season", "PO appearances / season", "SB appearances / season", "SB wins / season")


post_season %>% 
  #mutate(Values = 1 / Values) %>% 
  ggplot(aes(Post_Contract, Values, color = Name, fill = Name, shape = Name)) +
  #geom_line(size = 1/3, linetype = 'dotted') +
  geom_point(size = 2.5) +
  geom_point(data = post_season %>% filter(Name == "Patrick Mahomes"), aes(Post_Contract, Values), size=5, show.legend = F) +
  geom_line(linetype = 'dotted', size = 0.8) +
  facet_wrap(~Stats, scales = 'free') +
  labs(title = "Post season stats per season before and after long-term contract",
       x = NULL,
       y = NULL) +
  scale_x_continuous( limits = c(-0.3, 1.3), breaks = c(0, 1), labels = c("Pre", "Post")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  theme_bw() 

ggsave("charts/post_season_stats.png")


# before years and after years analysis

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
