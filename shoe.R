library(magrittr)

df <- readr::read_csv("C:/Users/cory/Desktop/shoe/Master_DT.txt")

colnames(df)

str(df)

# Control Group size
table(df$Control_Grp_Ind)

# Gender
table(df$gender)

# Loyalty
table(df$Lylty_Prog_Tier_Cd)

# Version
table(df$version)

table(df$version, df$Control_Grp_Ind)

# dupes
dupes <- duplicated(df)
table(dupes)
# no dupes

#df2 <- naniar::replace_with_na(df, replace = list(x = '?'))
df2 <- dplyr::as_tibble(apply(df, 2, function(y) gsub("?", "NA", y, fixed = TRUE)))

table(df2$gender)
str(df2)
# change structure
df2$user_id <- as.character(df2$user_id)
df2$frst_trans_dt <- as.Date(df2$frst_trans_dt, "%m/%d/%Y")
df2$rec_trans_dt <- as.Date(df2$rec_trans_dt, "%m/%d/%Y")
df2$frequency_trans <- as.numeric(df2$frequency_trans)
df2$monetary_net <- as.numeric(df2$monetary_net)
df2$quantity <- as.numeric(df2$quantity)
df2$sends <- as.numeric(df2$sends)
df2$opens <- as.numeric(df2$opens)
df2$clicks <- as.numeric(df2$clicks)
df2$redemption_trans <- as.numeric(df2$redemption_trans)
df2$redemption_demand <- as.numeric(df2$redemption_demand)
df2$none_redemption_trans <- as.numeric(df2$none_redemption_trans)
df2$none_redemption_demand <- as.numeric(df2$none_redemption_demand)

str(df2)

readr::write_csv(df2, 'shoes.csv')

# missing value exploration ----------------------------------------------------------

na_count <-
  sapply(df2, function(y)
    sum(length(which(is.na(
      y
    )))))

na_df <- data.frame(na_count)
View(na_df)

which(is.na(df2[, 'quantity']))

df2[1067, ]

#874 problem NAs.

df2 %>%
  dplyr::group_by(Control_Grp_Ind) %>%
  sjmisc::descr() -> descr_stats

 descr_stats[[1]]

df2_cat <-
  df2[, sapply(df2, class) == 'character']

df2_cat %>%
  dplyr::summarize_all(dplyr::funs(dplyr::n_distinct(.)))

# zero Var
feature_variance <- caret::nearZeroVar(df2, saveMetrics = TRUE)

feature_variance

# code missing ?

# code days since
df2$time1 <- "2018-04-22"

df2$time1 <- as.Date(as.character(df2$time1), format="%Y-%m-%d")
df2$time2 <- as.Date(as.character(df2$rec_trans_date), format = '%Y-%m-%d')
df2$days_first_trans <- as.numeric(df2$time1 - df2$frst_trans_date)

hist(df2$days_first_trans)

df2$days_rec_trans <- as.numeric(df2$time1 - df2$rec_trans_date)

# code average trans demand

df2 <- df2 %>%
  dplyr::mutate(avg_redemption = redemption_demand / redemption_trans,
                avg_none_redemption = none_redemption_demand / none_redemption_trans)

# code those nulls zero

df2 <-
  df2 %>% dplyr::mutate(avg_redemption = tidyr::replace_na(avg_redemption, 0))
df2 <-
  df2 %>% dplyr::mutate(avg_none_redemption = tidyr::replace_na(avg_none_redemption, 0))

# plot this as well and filter for greater than 0
df2 %>%
  dplyr::group_by(Control_Grp_Ind) %>%
  dplyr::summarise(
    x1 = mean(avg_redemption),
    x2 = sd(avg_redemption),
    y1 = mean(avg_none_redemption),
    y2 = sd(avg_none_redemption)
  )

# What percent responded
df2$redemption <- ifelse(df2$redemption_trans > 0, 1, 0)

df2$none_redemption <- ifelse(df2$none_redemption_trans > 0, 1, 0)

df2 %>%
  dplyr::group_by(Control_Grp_Ind) %>%
  dplyr::summarise(Total = dplyr::n()) 

library(dplyr)

df2 %>%
  count(Control_Grp_Ind, redemption) %>%
  group_by(Control_Grp_Ind) %>%          
  mutate(prop = prop.table(n))

df2 %>%
  count(Control_Grp_Ind, none_redemption) %>%
  group_by(Control_Grp_Ind) %>%          
  mutate(prop = prop.table(n))

# total unique responses
df2$any_purchase <- ifelse(df2$redemption > 0 | df2$none_redemption > 0, 1, 0)

df2 %>%
  count(Control_Grp_Ind, any_purchase) %>%
  group_by(Control_Grp_Ind) %>%          
  mutate(prop = prop.table(n))

df2 %>%
  dplyr::filter(redemption > 0) %>%
  dplyr::group_by(Control_Grp_Ind) %>%
  dplyr::summarise(
    Redeemers = dplyr::n(),
    Total_Redemption = sum(redemption_demand),
    Average_Redemption = mean(redemption_demand),
    Stdev_Redemption = sd(redemption_demand)
    
  )


# Vtreat
