
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
  mutate("supply_perc" = count / sum(count), "demand_perc" = total / sum(total), "budg_perc" = total_budget / sum(total_budget))

Mo_dat_for_plot <- Test_Month_TBO %>% 
  gather(key = "measure", value = "percentage", 5:7) %>% 
  select(month, measure, percentage) %>% 
  arrange(month)

MTS_FJ <- time_spent_month %>% 
  gather(key = "measure", value = "value", 2:3) %>% 
  arrange(HRMONTH)

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
MIAFT2 <- Movies_Infl_Adj_Filtered %>% 
  mutate("High_Season" = Det_season2(Rel_Day_of_Yr)) %>% 
  mutate()

MIAFT_Ratio2 <- Movies_Infl_Adj_Filtered_Test %>% 
  mutate("perf_ratio" = (TBO-budget)/budget) %>% 
  mutate("winner" = ifelse(perf_ratio >= 1, 1, 0))

time_spent_sum <- distinct(select(BLS_Sum, TUCASEID, TUYEAR))
time_spent_cps <- distinct(select(Cur_Pop, TUCASEID, HRMONTH))
time_spent_tv3 <- Surv_Resp %>% 
  select(TUCASEID, TUACTDUR, TRCODEP) %>% 
  filter(TRCODEP == "120303") %>% 
  left_join(y = time_spent_sum, by = 'TUCASEID') %>% 
  left_join(y = time_spent_cps, by = 'TUCASEID')

time_spent_tv2 <- atusact_0316 %>% 
  select(TUCASEID, TUACTDUR, TRCODEP) %>% 
  filter(TRCODEP == "120303") %>% 
  left_join(y = distinct(select(atuscps_0316, TUCASEID, HRMONTH), by = 'TUCASEID')) %>% 
  left_join(y = distinct(select(atussum_0316, TUCASEID, TUYEAR), by = 'TUCASEID'))

time_spent_summary <- time_spent_tv3 %>% 
  group_by(HRMONTH) %>% 
  summarise(mean_ts = mean(TUACTDUR)) %>% 
  mutate("index" = mean_ts/mean(mean_ts))

      
time_spent_tv3 <- atusact_0316 %>% 
  select(TUCASEID, TUACTDUR, TRCODEP) %>% 
  filter(TRCODEP == "120303")

tbl_4_test <- data_frame("subprime" = c(2798, 3.129, 16.119), "prime" = c(2086, 3.541, 18.708))

Mad_Max <- MIAFT_Ratio2 %>% 
  filter(movie == "Mad Max")

#atuscps = Cur_Pop (current population information about the respondents / where we get month)
#atussum = BLS_Sum (summary information about the responses)
#atusact = Surv_Resp (respondent level data = what they said they did)

hits <- movies_scs_base %>% 
  filter(perf_ratio > 20, Prod_budget > 1000000)

#import FRED data for indexing inflation (Dec 2017 = 100)
FRED_infl <- read_csv("~/Downloads/CPIAUCNS (2).csv", 
                      col_types = cols(CPIAUCNS_NBD20171201 = col_number(), 
                      DATE = col_date(format = "%Y-%m-%d")))
colnames(FRED_infl) <- c("Date", "Infl_index")

FRED_infl <- FRED_infl %>% 
  mutate("yearmon" = as.yearmon(Date))

# adjust budgets for inflation
movies_scs_base_infl <- movies_scs_base %>% 
  mutate("Rel_yearmon" = as.yearmon(Rel_date_TH)) %>% 
  left_join(y = FRED_infl, Infl_index, by = c("Rel_yearmon" = "yearmon")) %>% 
  mutate("Infl_prod_budget" = as.numeric(Prod_budget * (100 / Infl_index)))

movies_scs_base_ave <- movies_scs_base %>% 
  mutate("Rel_yearmon" = as.yearmon(Rel_date_TH)) %>% 
  left_join(y = FRED_infl, Infl_index, by = c("Rel_yearmon" = "yearmon")) %>% 
  mutate("Infl_prod_budget" = as.numeric(Prod_budget * (100 / Infl_index))) %>% 
  group_by("year" = year(Rel_date_TH)) %>% 
  summarize("ave" = mean(Infl_prod_budget), "count" = n())

as.yearmon(paste(Cons_time_spent_raw$HRYEAR4[1], Cons_time_spent_raw$HRMONTH[1], sep = '-'))

test_dum <- movies_scs_base_infl %>% 
  select(Title, Genre)

# this is really stupid
test_dum <- as.data.frame(test_dum)

test_dum2 <- dummy.data.frame(data = test_dum, names = "Genre")

test_dum2 <- dummy(x = "Genre", data = test_dum)


# do monthly analysis of parents consumption
parents_ATUS <- BLS_summary %>% 
  select(TUCASEID, TRCHILDNUM) %>% 
  filter(TRCHILDNUM > 0) %>% 
  inner_join(y = select(Cur_pop, TUCASEID, HRMONTH), by = "TUCASEID")

parents_Months <- Cur_pop %>% 
  select(TUCASEID, HRMONTH, HRYEAR4)

parents_ts <- parents_ATUS %>% 
  left_join(y = parents_resp, TRCODEP, TUACTDUR, by = "TUCASEID")

parents_resp <- Surv_resp %>%
  select(TUCASEID, TRCODEP, TUACTDUR) %>% 
  filter(TRCODEP == "120303" | TRCODEP == "120403") %>%
  inner_join(y = parents_ATUS, TRCHILDNUM, by = "TUCASEID") %>% 
  inner_join(y = parents_Months, by = "TUCASEID") %>% 
  distinct()

parents_resp_months <- parents_resp %>% 
  group_by(HRMONTH, TRCODEP) %>% 
  summarize("mean" = mean(TUACTDUR), "count" = n()) %>% 
  gather(key = "medium", value = "value", mean)

ggplot(parents_resp_months, aes(x = factor(HRMONTH), y = mean, group = TRCODEP, color = TRCODEP)) + geom_point(size = 1) + geom_line() + scale_x_discrete(name = "Month")

CTS_stats <- Cons_time_spent_raw %>% 
  group_by(HRMONTH, TRCODEP) %>% 
  summarize("ave" = mean(TUACTDUR), "stnd_dev" = sd(TUACTDUR), "count" = n())

CTS_stats_fm <- CTS_stats %>%
  filter(TRCODEP == "120403") 

CTS_stats_fm <- CTS_stats_fm %>% 
  mutate("TTS_raw" = count * ave) %>% 
  mutate("Movie_TTS_index" = (TTS_raw - mean_TTS) / sd_TTS,
         "movies_cons_count_index" = (count - mn_ct) / sd_ct)

ggplot(CTS_stats_fm, aes(x = HRMONTH, y = TTS_index)) + geom_line()

mean(Cons_time_spent_raw$TUACTDUR[Cons_time_spent_raw$TRCODEP == "120403"])
length(Cons_time_spent_raw$TUACTDUR[Cons_time_spent_raw$TRCODEP == "120403"])
sd(Cons_time_spent_raw$TUACTDUR[Cons_time_spent_raw$TRCODEP == "120403"])
summary(Cons_time_spent_raw$TUACTDUR[Cons_time_spent_raw$TRCODEP == "120403"])


mean(parents_resp$TUACTDUR[parents_resp$TRCODEP == "120403"])
length(parents_resp$TUACTDUR[parents_resp$TRCODEP == "120403"])
sd(parents_resp$TUACTDUR[parents_resp$TRCODEP == "120403"])
summary(parents_resp$TUACTDUR[parents_resp$TRCODEP == "120403"])

anim_style <- c("Animation/Live Action", "Digital Animation", "Hand Animation", "Stop-motion Animation")

movies_fam_animated <- movies_scs_base_infl %>% 
  filter(Prod_method %in% anim_style, MPAA_rating == "G" | MPAA_rating == "PG", !is.na(Infl_prod_budget))

movies_FA_month <- movies_fam_animated %>% 
  group_by(Rel_month, Is_comm_success) %>% 
  summarize("count" = n()) %>% 
  spread(key = Is_comm_success, value = count)

colnames(movies_FA_month) <- c("Rel_month", "Not_CS", "CS")

movies_FA_month <- movies_FA_month %>% 
  mutate("success_rate" = CS / (CS + Not_CS))

movies_fam_animated_log <- movies_scs_base_infl %>% 
  filter(Prod_method %in% anim_style, MPAA_rating == "G" | MPAA_rating == "PG") %>% 
  filter(between(as.Date(Rel_date_TH), as.Date("2002-09-01"), as.Date("2016-11-01")))  %>% 
  select(Title, Infl_prod_budget, Rel_month, Rel_yearmon, Is_comm_success) %>%
  mutate("Is_rel_prime" = ifelse(Rel_month %in% c(5,6,7,11,12),1,0)) %>% 
  left_join(y = select(YM_All_1, Movies_TH, yearmon), by = c("Rel_yearmon" = "yearmon")) %>% 
  left_join(y = select(mcount_YM, YM_count, Rel_yearmon), by = c("Rel_yearmon" = "Rel_yearmon")) %>% 
  select(Title, Infl_prod_budget, Is_rel_prime, Movies_TH, YM_count, Is_comm_success)

movies_FAL_scaled <- movies_fam_animated_log %>% 
  select(Infl_prod_budget, Movies_TH, YM_count) %>% 
  scale() %>%
  as.data.frame()

movies_FAL_fin <- movies_fam_animated_log %>% 
  select(Is_comm_success, Is_rel_prime) %>% 
  bind_cols(movies_FAL_scaled)

logR_anim <- glm(Is_comm_success ~ Is_rel_prime + Infl_prod_budget + YM_count, data = movies_FAL_fin, family = "binomial")

movies_FA_mm <- movies_fam_animated %>% 
  group_by(Rel_month) %>% 
  summarize("count" = n(), "mean_budg" = mean(Infl_prod_budget), "mean_bo" = mean(Infl_Dom_BO_FRED))

sept_FA <- movies_fam_animated %>% 
  filter(Rel_month == 9) %>%
  select(Title, Infl_prod_budget, Infl_Dom_BO_FRED, Is_comm_success,Genre)

cons_TTS <- Cons_time_spent_raw %>% 
  group_by(HRMONTH) %>% 
  summarize("count" = n(), "TTS" = sum(TUACTDUR)) %>% 
  mutate("TTS_index" = (TTS - mean(TTS)) / sd(TTS), "cons_count_index" = (count - mean(count))/sd(count))

an_movies_not_tidy2 <- an_movies_not_tidy %>% 
  bind_cols(as.data.frame(select(cons_TTS, TTS_index, cons_count_index))) %>% 
  bind_cols(as.data.frame(select(CTS_stats_fm, Movie_TTS_index, movies_cons_count_index))) %>% 
  select(-HRMONTH)

an_movies_tidy2 <- an_movies_not_tidy2 %>% 
  gather(key = "measure", value = "value", 2:9)

ggplot(an_movies_tidy2, aes(x = factor(month), y = value, group = measure, color = measure, fill = measure)) + 
  geom_point(size = 2, alpha = 0.7) + geom_line()

ggplot(filter(an_movies_tidy2, measure == "success_rate" | measure == "Movie_TTS_index"), 
       aes(x = factor(month), y = value, color = measure, group = measure)) + geom_point(size = 2) + 
  geom_line() + geom_point(size = 2)+
  ggtitle(label = "Success Rate vs. Total time spent (movies only)", subtitle = "Indexed by z-score")

ggplot(filter(an_movies_tidy2, measure == "success_rate" | measure == "TTS_index"), 
       aes(x = factor(month), y = value, color = measure, group = measure)) + geom_point(size = 2) + 
  geom_line() + geom_point(size = 2) +
  ggtitle(label = "Success Rate vs. Total time spent (all ent. categories)", subtitle = "Indexed by z-score")


cor(select(an_movies_not_tidy2, -month), use = 'complete.obs')

x <- lm(success_rate ~ supply + Movie_TTS_index + movies_cons_count_index + total_bo, data = an_movies_not_tidy2)
summary(x)

ggplot(filter(movies_fam_animated, MPAA_rating == "G"), aes(x = factor(Rel_month))) + geom_histogram(binwidth = 12, stat = "count") +
  scale_x_discrete(name = "Month of Release") + ggtitle(label = "Animated Film Release Dates", subtitle = "G Rated")

ggplot(movies_fam_animated, aes(x = factor(Rel_month))) + geom_histogram(binwidth = 12, stat = "count") +
  scale_x_discrete(name = "Month of Release") + ggtitle(label = "Animated Film Release Dates", subtitle = "G/PG Rated")

##############################
# LogR on Animated films
##############################
movies_anim_lm <- movies_scs_base_infl %>% 
  filter(between(as.Date(Rel_date_TH), as.Date("2002-09-01"), as.Date("2016-11-01")), Prod_method %in% anim_style,
         MPAA_rating == "PG" | MPAA_rating == "G") %>% 
  select(Title, Infl_prod_budget, Rel_month, Rel_yearmon, Is_comm_success) %>%
  mutate("Is_rel_prime" = ifelse(Rel_month %in% c(5,6,7,11,12),1,0)) %>% 
  left_join(y = select(YM_All_1, Movies_TH, yearmon), by = c("Rel_yearmon" = "yearmon")) %>% 
  left_join(y = select(mcount_YM, YM_count, Rel_yearmon), by = c("Rel_yearmon" = "Rel_yearmon")) %>% 
  select(Title, Infl_prod_budget, Is_rel_prime, Movies_TH, YM_count, Is_comm_success)

movies_anim_scaled <- movies_anim_lm %>% 
  select(Infl_prod_budget, YM_count, Movies_TH) %>% 
  scale() %>% 
  as.data.frame()

movies_anim_FIN <- movies_anim_lm %>% 
  select(Title, Is_rel_prime, Is_comm_success) %>% 
  bind_cols(movies_anim_scaled) %>% 
  select(-Title)

#############################
# Parents vs. Gen Pop Time Spent
#############################

parents <- BLS_summary %>% 
  filter(TRCHILDNUM > 0) %>% 
  select(TUCASEID)

parents <- as.vector(parents$TUCASEID)

gen_CTS <- Cons_time_spent_raw %>% 
  filter(!(TUCASEID %in% parents), TRCODEP == "120403") %>% 
  group_by(HRMONTH) %>% 
  summarize("GP_ave_ts" = mean(TUACTDUR), "GP_count" = n(), "GP_TTS" = sum(TUACTDUR))

parents_CTS <- Cons_time_spent_raw %>% 
  filter(TUCASEID %in% parents, TRCODEP == "120403") %>% 
  group_by(HRMONTH) %>% 
  summarize("parent_ave_ts" = mean(TUACTDUR), "parents_count" = n(), "parents_TTS" = sum(TUACTDUR))

CTS_plt <- gen_CTS %>% 
  left_join(parents_CTS, by = "HRMONTH") %>% 
  bind_cols(FA_success) %>% 
  gather(key = "measure", value = "value", 2:8)

CTS_plt_scaled <- gen_CTS %>% 
  left_join(parents_CTS, by = "HRMONTH") %>% 
  bind_cols(FA_success) %>% 
  select(-HRMONTH) %>% 
  scale() %>% 
  as.data.frame()

CTS_plt_scaled <- bind_cols(CTS_plt_scaled, as.data.frame(parents_CTS$HRMONTH))

CTS_plt_scaled_tidy <- CTS_plt_scaled %>% 
  gather(key = "measure", value = "value", 1:7)

CTS_parents_GP_TTS <- CTS_plt_scaled_tidy %>% 
  filter(measure == "parent_ave_ts" | measure == "success_rate")

ggplot(CTS_parents_GP_TTS, aes(x = factor(HRMONTH), y = value, group = measure, color = measure)) + 
  geom_point(size = 2, alpha = 0.7) + geom_line() +
  scale_x_discrete(name = "Month") +
  ggtitle(label = "Success Rate for Animation Films", subtitle = "Parents total time spent watching movies")

 logR_anim <- glm(Is_comm_success ~ ., data = movies_anim_FIN, family = "binomial")

movies_FA_mo_summ <- movies_fam_animated %>%
  select(Title, Rel_month, Is_comm_success) %>% 
  mutate("Is_comm_success_char" = ifelse(Is_comm_success == 1, "CS", "Not_CS")) %>% 
  group_by(Rel_month, Is_comm_success_char) %>% 
  summarize("mo_count" = n()) %>% 
  spread(key = Is_comm_success_char, value = mo_count) %>% 
  mutate("success_rate" = CS / (CS+Not_CS)) %>% 
  ungroup()
