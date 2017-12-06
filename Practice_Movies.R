
yrs_sum <- function(start, fin){
  yrs_movies <- filter(Test_Movie_Data, Test_Movie_Data$movie_production_year >= start & Test_Movie_Data$movie_production_year <= fin)
  return(sum(yrs_movies$TBO))
}

yrs_sum(1976, 1982)

decs <- c(1970, 1979, 1980, 1989, 1990, 1999, 2000, 2009, 2010, 2019)

decades <- matrix(decs, nrow = 5, ncol = 2, byrow = TRUE)

dec_sums <- c()
for (i in 1:5){
  a <- yrs_sum(dec_df$Start[i], dec_df$End[i])
  dec_sums <- c(dec_sums, a)
}

dec_movie_count <- function(start, fin) {
  dec_movies <- filter(Test_Movie_Data, Test_Movie_Data$movie_production_year >= start & Test_Movie_Data$movie_production_year <= fin)
  return(length(dec_movies$movie_display_name))
}

movies_in_dec <- c()
for (i in 1:5){
  a <- dec_movie_count(dec_df$Start[i], dec_df$End[i])
  movies_in_dec <- c(movies_in_dec, a)
}

m_by_dec_2 <- function(x,y) {
  m_in_tf <- filter(Test_Movie_Data, Test_Movie_Data$movie_production_year >= x & Test_Movie_Data$movie_production_year <= y)
  return(length(m_in_tf$movie_display_name))
}

#For movies in the 80s calculate box office per minute and arrange by highest DPM
Yes_in_the_80s %>% 
  filter(!is.na(movie_running_time)) %>% 
  mutate(DPM = TBO / movie_running_time) %>% 
  select(movie_display_name, DPM) %>% 
  arrange(desc(DPM))

#For movies in the aughts, calculate box office per minute by genre, arrange lowest to highest
Maroon_5 <- Test_Movie_Data %>% 
  filter(movie_production_year >= 2000, movie_production_year <= 2009, !is.na(movie_running_time), !is.na(TBO), movie_running_time > 0) %>% 
  select(movie_genre_display_name, TBO, movie_running_time) %>% 
  mutate(DPM = TBO / movie_running_time) %>% 
  select(movie_genre_display_name, DPM) %>%
  group_by(movie_genre_display_name) %>% 
  summarise(ave_dpm = mean(DPM))

Drama_disc <- Test_Movie_Data %>% 
  filter(movie_production_year >= 2000, movie_production_year <= 2009, !is.na(movie_running_time), !is.na(TBO)) %>%
  select(movie_display_name, TBO, movie_running_time) %>% 
  arrange((TBO))

Rel_date <- Test_Movie_Data %>% 
  select(movie_theatrical_release_release_date) %>% 
  seperate(c("year","month", "day"))

# Calculate TBO by month in entire data set
Month_TBO <- Rel_date %>% 
  group_by(month) %>% 
  summarise("count" = n(), "total" = sum(TBO), "total_budget" = sum(as.numeric(movie_financial_summary_production_budget))) %>% 
  select(month, "count", "total", "total_budget") %>% 
  mutate("supply_perc" = count / sum(count), y"demand_perc" = total / sum(total), "budg_perc" = total_budget / sum(total_budget))

Mo_dat_for_plot <- Test_Month_TBO %>% gather(key = "measure", value = "percentage", 5:7) %>% 
  select(month, measure, percentage) %>% 
  arrange(month)

# Plot the supply, demand, and budget percentages in bar chart
ggplot(Mo_dat_for_plot, aes(x = month, y = percentage)) + geom_bar(position = "dodge", stat = "identity", aes(fill = measure))

Budg_dem <- Mo_dat_for_plot %>% 
  filter(measure == bud_perc | measure == demand_perc)

#Line graph of budget v demand percentages
ggplot(Budg_dem, aes(x = month, y = percentage, col = measure, group = measure)) + geom_line()

ggplot(Mo_dat_for_plot, aes(x = month, y = percentage, col = measure, group = measure)) + geom_line()

Test_Movie_Data_2 <- Test_Movie_Data %>% 
  mutate(Rel_Day_of_Yr = yday(movie_theatrical_release_release_date)) 

Movies_Infl_Adj_Filtered <- Movies_Infl_Adj %>% 
  select(movie_display_name, movie_theatrical_release_release_date, movie_financial_summary_inflation_adjusted_domestic_box_office, TBO, movie_financial_summary_production_budget) %>% 
  filter(!is.na(movie_financial_summary_inflation_adjusted_domestic_box_office), !is.na(movie_financial_summary_production_budget)) %>% 
  mutate(Rel_Day_of_Yr = yday(movie_theatrical_release_release_date))

Det_season <- function(x) {
  results <- c()
  for(i in 1:length(x)) {
    print(x[i])
    if((x[i] >= 0 & i <= 122) | (x[i] >= 244)) {
      var = 1
    } else {
      var = 0
    }
    print(var)
    results <- c(results, var)
    }
  return(results)
}

#for determining if a film is realeased during 'high season'
Det_season2 <- function(x) {
  results <- c()
  for(i in 1:length(x)){
    var = as.numeric((x[i] >= 74 & x[i] <= 105) | (x[i] >= 140 & x[i] <= 212) | (x[i] >= 319))
    results <- c(results, var)
  }
  return(results)
}

#Add column about whether or not a film is released in 'high season' [Spring break, summer, or holiday season]
Movies_Infl_Adj_Filtered_Tes2t <- Movies_Infl_Adj_Filtered %>% 
  mutate("High_Season" = Det_season2(Rel_Day_of_Yr))


MIAFT_Ratio <- Movies_Infl_Adj_Filtered_Test %>% 
  mutate("perf_ratio" = Inf_Adj_BO/budget) %>% 
  mutate("winner" = ifelse(perf_ration >= 1, 1, 0))

time_spent_tv2 <- atusact_0316 %>% 
  select(TUCASEID, TUACTDUR, TRCODEP) %>% 
  filter(TRCODEP == "120303") %>% 
  left_join(y = distinct(select(atuscps_0316, TUCASEID, HRMONTH), by = 'TUCASEID')) %>% 
  left_join(y = distinct(select(atussum_0316, TUCASEID, TUYEAR), by = 'TUCASEID'))

time_spent_summary <- time_spent_tv2 %>% 
  group_by(HRMONTH, TUYEAR) %>% 
  summarise(mean = mean(TUACTDUR))

time_spent_tv3 <- atusact_0316 %>% 
  select(TUCASEID, TUACTDUR, TRCODEP) %>% 
  filter(TRCODEP == "120303")
