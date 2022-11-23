library(tidyverse)
library(ggplot2)
library(dplyr)

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

incarceration_trends <- read.csv("C:/Users/eddyw/Documents/Info 201/Assignments/incarceration-trends/incarceration_trends.csv")

state_code <- read.csv("C:/Users/eddyw/Documents/Info 201/Assignments/a4-EddyW206/source/state_names_and_codes.csv")

## Section 2  ---- 
#----------------------------------------------------------------------------#

# Average jail population of all states combined in 2018 <todo: update comment>
avg_jail_pop <- incarceration_trends %>% 
  filter(year == max(year)) %>% 
  summarise(avg_jail_pop = mean(total_jail_pop, na.rm = T)) %>% 
  pull(avg_jail_pop)

# Average population of blacks incarcerated in 2018
avg_black_inc <- incarceration_trends %>% 
  filter(year == max(year)) %>% 
  select(black_jail_pop) %>% 
  summarise(avg_black_inc = sum(black_jail_pop, na.rm = T)) %>% 
  pull(avg_black_inc)

# Average population of whites incarcerated in 2018
avg_white_inc <- incarceration_trends %>% 
  filter(year == max(year)) %>% 
  select(white_jail_pop) %>% 
  summarise(avg_white_pop = sum(white_jail_pop, na.rm = T)) %>% 
  pull(avg_white_pop)
#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  jail_pop <- incarceration_trends %>% 
    group_by(year) %>% 
    summarise(total_jail_pop = mean(total_jail_pop, na.rm = T)) %>% 
    select(year, total_jail_pop)
  return(jail_pop)
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  chart <- ggplot(get_year_jail_pop()) +
    geom_col(mapping = aes(x = year, y = total_jail_pop)) +
    labs(x = "Year", y = "Jail Population Total")
  return(chart)
} 
plot_jail_pop_for_us()

#----------------------------------------------------------------------------#

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
get_jail_pop_by_states <- function(states) {
  prison_pop_state <- incarceration_trends[incarceration_trends$state %in% c('WA', 'TX', 'IL', 'CA', 'FL', 'OH'), ] %>% 
    group_by(state, year) %>% 
    summarise(jail_pop = sum(total_jail_pop, na.rm = T)) %>% 
    return(prison_pop_state)
}
plot_jail_pop_by_states <- function() {
  line_chart <- ggplot(get_jail_pop_by_states(states)) +
    geom_line(mapping = aes(x = year, y = jail_pop, color = state)) +
    labs(x = "Year", y = "Jail Population", title = "Year over Year growth of Jail Population")
  return(line_chart)
}
plot_jail_pop_by_states()

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
# Comparison of total jail population vs total black jail population & total white population vs total jail population

# total_black <- df %>%
#   group_by(year) %>%
#   filter(year == max(year)) %>%
#   summarise(total_black = sum(black_pop_15to64), na.rm = T) %>%
#   pull(total_black)
# 
# total_white <- df %>%
#   group_by(year) %>% 
#   filter(year == max(year)) %>%
#   summarise(total_white = sum(white_pop_15to64), na.rm = T) %>%
#   pull(total_white)

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
# Map of Black Jail Population by State (2018)
df2 <- rename(incarceration_trends, Code = state)
df3 <- right_join(df2, state_code, by = "Code")
df4 <- df3 %>% 
  rename(state = State) %>% 
  mutate(state = tolower(state))

inequality <- function() {
  ine <- df4 %>% 
    filter(year == max(year)) %>% 
    group_by(state) %>% 
    summarise(total_black_jail = sum(black_jail_pop, na.rm = T), total_jail_pop = sum(total_jail_pop, na.rm = T)) %>% 
    mutate(total = (total_black_jail / total_jail_pop)) %>% 
    select(state, total)
  return(ine)
}

state_shape <- map_data("state") %>% 
  rename(state = region) %>% 
  left_join(inequality(), by = "state")

map_ine <- function() {
  plot <- ggplot(state_shape) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = total),
                 color = "white",
                 size = .1
                 ) +
    coord_map() +
    scale_fill_continuous(low = "#132B43", high = "Red") +
    labs(fill = "Total", title = "Total black population jailed compared to total jail population by State (2018)") +
    theme_grey()
  return(plot)
}

map_ine()

# 16.4
