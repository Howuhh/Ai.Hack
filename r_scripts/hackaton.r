library(tidyverse)
library(lubridate)
library(data.table)
library(psych)
library(stringr)

# Load data for tree and clust analysis
train_data = fread("../raw_data/train_data.csv", header = T, verbose=TRUE) %>% na.omit()
test_data = fread("../raw_data/test_data.csv", header = T, verbose=TRUE) %>% na.omit()

# Merge both datasets
all_data = rbind(train_data, test_data) %>% select(-c(V1, time, code, code1, code_azs, n_tr))

# Work with formats
all_data$date = ymd(all_data$date)
all_data$first_prch = dmy_hms(all_data$first_prch)

# New features:

# year_of_first_prch
all_data$year_of_first_prch = year(all_data$first_prch)

# is_holiday
all_data$is_holiday = ifelse(lubridate::wday(all_data$date, label = T) %in% c("сб", "вк"), 1, 0)

# is_supplementary
all_data$is_supplementary = ifelse(all_data$q > 0, 1, 0)

# sum_per_month
all_data = all_data %>% select(-c(first_prch, q, v_l))

df = all_data %>% group_by(year(date), month(date), id) %>% summarise(sum_per_month = sum(sum_b))

df$is_active = ifelse(df$sum_per_month == 0, 0, 1)

df1 = df %>% group_by(id) %>% summarise(n_month_active = sum(is_active), money_spent = sum(sum_per_month), sum_per_month = money_spent/n_month_active)

all_data = df1 %>% select(id, sum_per_month) %>% left_join(all_data, df1, by = "id")

all_data = all_data %>% dplyr::select(-date)

# load dataset with clusters for ids
clusters = read_csv("../data/k_means.csv") %>% select(-c(x,y))
colnames(clusters)[1] = "id"

# adding custers
all_data = left_join(all_data, clusters, by = "id")

# Save to csv
write.csv(all_data, "../data/data_for_clust_tree.csv")


