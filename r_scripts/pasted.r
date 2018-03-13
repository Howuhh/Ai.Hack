# for load data
library(data.table)
# work with date
library(lubridate)
# work with data
library(dplyr)
# work with strings
library(stringr)



# LOAD DATA
train <- fread("../raw_data/train_data.csv")
test <- fread("../raw_data/test_data.csv")
full <- rbind(train, test)

# DATE FORMAT
full$date <- ymd(full$date)

# PREPARING THE DATSET FOR DOC2VEC
full <- full %>% dplyr::select(date, id, code1, v_l) %>% dplyr::filter(v_l == 0) %>% dplyr::select(-v_l) %>% na.omit()

# sentence - code1 per day
full <- full %>% group_by(date, id) %>% mutate(goods = paste0(code1, collapse = " ")) 
full <- full[, -3]

full_dist <- distinct(full)

full_dist$detected <- str_detect(full_dist$goods, " ")
full_dist <- full_dist %>% filter(detected == TRUE) %>% select(-detected) 
full_dist <- select(as.data.frame(full_dist), -date)

full_dist <- full_dist %>% group_by(id) %>% mutate(goods_sentence = paste0(as.character(goods), collapse=" ")) %>% select(-goods)
full_dist <- distinct(full_dist)
#full_dist$goods_sentence <- paste(full_dist$goods_sentence, sep=".")
df_full <- as.data.frame(full_dist)

# SAVE TO CSV
write.csv(df_full, file="../data/data_for_doc2vec.csv", row.names = F)
