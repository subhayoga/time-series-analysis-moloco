# Time series analysis - Moloco

# Setup
library(tidyverse)
setwd("D://Subha//Moloco")

# Reading input
ts_df <- 
  read.csv("SWE sample data - Q3 data.csv") %>% 
  mutate(ts = as.POSIXct(ts, format="%Y-%m-%d %H:%M:%S")) %>% 
  as_tibble()

str(ts_df)

# Which site_id has the largest number of unique users? And what's the number?
(ts_df %>% 
  filter(country_id == "BDV") %>% 
  group_by(site_id) %>% 
  summarise(unique_users = n_distinct(user_id)) %>% 
  ungroup() %>% 
  arrange(desc(unique_users)) %>% 
  top_n(1))$site_id
  
# Between 2019-02-03 00:00:00 and 2019-02-04 23:59:59, there are four users who visited a certain site more than 10 times. 
# Find these four users & which sites they (each) visited more than 10 times.
ts_df %>% 
  filter(ts >= as.POSIXct(as.factor("2019-02-03 00:00:00"), format="%Y-%m-%d %H:%M:%S") &
           ts <= as.POSIXct(as.factor("2019-02-04 23:59:59"), format="%Y-%m-%d %H:%M:%S")) %>% 
  group_by(user_id,site_id) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  filter(n>10)


# For each site, compute the unique number of users whose last visit (found in the original data set) was to that site. 
# Based on this measure, what are top three sites?
ts_df %>% 
  arrange(ts) %>% 
  group_by(user_id) %>% 
  summarise(last_visited_site=last(site_id)) %>% 
  ungroup() %>% 
  group_by(last_visited_site) %>% 
  summarise(unique_users=n_distinct(user_id)) %>% 
  ungroup() %>% 
  arrange(desc(unique_users)) %>% 
  top_n(3)

# For each user, determine the first site he/she visited and the last site he/she visited based on the timestamp data  
# Compute the number of users whose first/last visits are to the same website. What is the number?
ts_df %>% 
  arrange(ts) %>% 
  group_by(user_id) %>% 
  summarise(first_site = first(site_id),
            last_site = last(site_id)) %>% 
  ungroup() %>% 
  filter(first_site == last_site) %>% 
  summarise(n_distinct(user_id)) %>% 
  ungroup()

# For each site, count the following numbers: (A) the number of unique users who have visited at least two different countries (B) the number of all unique users. 
# Please calculate the ratio B/A for each site and list top three sites and the corresponding ratio.

# Each site's unique count of users 
unique_users_per_site <- 
  ts_df %>% 
  group_by(site_id) %>% 
  summarise(B=n_distinct(user_id)) %>% 
  ungroup()

# Unique users with 2 or more countries
users_2ormore_countries <-
  ts_df %>% 
  group_by(user_id) %>% 
  summarise(n_countries = n_distinct(country_id)) %>% 
  ungroup() %>% 
  filter(n_countries >= 2)

# Sites grouped by unique users who visited 2 or more countries only
sites_by_users_2ormore_countries <- 
  ts_df %>% 
  filter(user_id %in% users_2ormore_countries$user_id) %>% 
  group_by(site_id) %>% 
  summarise(A = n_distinct(user_id)) %>% 
  ungroup()

# Overall ratio
sites_by_users_2ormore_countries %>% 
  inner_join(unique_users_per_site) %>% 
  mutate(ratio = B/A) %>% 
  arrange(desc(ratio)) %>% 
  top_n(3)

