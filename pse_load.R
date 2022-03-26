library(boot)
library(M3)
library(hms)
library(data.table)
library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(zoo)
library(readr)
library(shiny)
library(ggthemr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(wesanderson)
library(scales)
library(viridis)
library(ggh4x)
library(hrbrthemes)

raspberry <- "#DB2955"
babyblue <- "#47AFFF"
prussian <- "#113255"
sapphire <- "#255F85"
mint <- "#B2EDC5"
celadon <- "#5F8DBF"
viridian <- "#5A876F"
khaki <- "#9fae84"
turq <- "#76E7CD"
violet <- "#AA78A6"
sparkle <- "#2F5075"
pale_mint <- "#73BFB0"
jungle <- "#193832"
amazon <- "#317658"
bedazzled <- "#1E569B"
tumbleweed <- '#D0A98F'
french <- '#8DBCF7'
mountbatten <- '#957D95'
cranberry <-  '#c10534'
lapis <- '#38618C'
tuscan <- "#804E49"
dark_emerald <- '#2d6d66'
english_violet <- '#3D314A'
eggplant <- '#684756'
raspberry <- '#DB2955'
saffron <- '#F49D37'
sugar_plum <- '#8C547C'
solid_pink <- '#873D48'
turquoise <- '#76E7CD'
cherry <- '#B4415C'
lilac <- '#6F5A87'
maroon <- '#90353B'
onyx <- '#313435'
charcoal <- '#646867'
slate <- '#595F5A'
burnished.brown <- '#AB8476'


ggthemr(palette = 'pale')

files = list.files(path = './data/PSE/', pattern = '.csv', full.names = TRUE)
data <-  do.call(rbind, lapply(files, read_csv)) %>% janitor::clean_names()
weather <- read_csv('./data/temps_seattle.csv') %>% select(date, precp, tmax, tmin)
data <- select(data, date, time = start_time, usage)
data <- mutate(data, date = mdy(date))
weather <- mutate(weather, date = mdy(date))


days <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')

#' need to reformat the time column to include seconds;
#' this is necessary for getting the proper format
#' seconds <- ':00'
#' data <- mutate(data, time = paste(time, seconds)) %>% 
#' remove white spacing from pasting the strings together
#'  mutate(time = gsub('[[:space:]]', '', time))
#' get a proper date-time column using lubridate
data <- mutate(data, datetime = with(data, ymd(date) + hms(time)))

#' this can be in two other ways:
#' first is with the as.POSIXct function in base R
#' data <- mutate(data, datetime = as.POSIXct(paste(date, start_time), format = '%Y-%m-%d %h:%m:%s'))
#' the other option is with the M3 package
#' data <- mutate(data, datetime  = M3::combine.date.and.time(date, time))

#' extract the hour convert to nearest whole hour
#' 

data <- mutate(data, hour = hour(datetime))
data <- mutate(data, monthday = format(date, '%m-%d'))
data <- mutate(data, time_local = as_hms(floor_date(datetime, unit = 'hour'))) %>% 
  mutate(month = months(date),
         weekday = weekdays(date))
#
# data <-
#   mutate(
#     data,
#     time_local = case_when(
#       hour == 0 ~ '12:00 AM',
#       hour == 1 ~ '1:00 AM',
#       hour == 2 ~ '2:00 AM',
#       hour == 3 ~ '3:00 AM',
#       hour == 4 ~ '4:00 AM',
#       hour == 5 ~ '5:00 AM',
#       hour == 6 ~ '6:00 AM',
#       hour == 7 ~ '7:00 AM',
#       hour == 8 ~ '8:00 AM',
#       hour == 9 ~ '9:00 AM',
#       hour == 10 ~ '10:00 AM',
#       hour == 11 ~ '11:00 AM',
#       hour == 12 ~ '12:00 PM',
#       hour == 13 ~ '1:00 PM',
#       hour == 14 ~ '2:00 PM',
#       hour == 15 ~ '3:00 PM',
#       hour == 16 ~ '4:00 PM',
#       hour == 17 ~ '5:00 PM',
#       hour == 18 ~ '6:00 PM',
#       hour == 19 ~ '7:00 PM',
#       hour == 20 ~ '8:00 PM',
#       hour == 21 ~ '9:00 PM',
#       hour == 22 ~ '10:00 PM',
#       hour == 23 ~ '11:00 PM'
#     )
#   ) %>% 
#   mutate(month = months(date),
#          weekday = weekdays(date))


data <- mutate(
  data,
  season =
    case_when(
      month %in% c('December', 'January', 'February') ~ 'Winter',
      month %in% c('March', 'April', 'May') ~ 'Spring',
      month %in% c('June', 'July', 'August') ~ 'Summer',
      month %in% c('September', 'October', 'November') ~ 'Fall'
    )
) %>%
  mutate(season = factor(season, levels = c('Fall', 'Winter', 'Spring', 'Summer')),
         weekday = factor(weekday, levels = days))

data <- relocate(data, 'usage', .after = 'season')

local_tz <- c(
      '12:00 AM',
      '1:00 AM',
      '2:00 AM',
      '3:00 AM',
      '4:00 AM',
      '5:00 AM',
      '6:00 AM',
      '7:00 AM',
      '8:00 AM',
      '9:00 AM',
      '10:00 AM',
      '11:00 AM',
      '12:00 PM', 
      '1:00 PM', 
      '2:00 PM',
      '3:00 PM', 
      '4:00 PM', 
      '5:00 PM', 
      '6:00 PM', 
      '7:00 PM', 
      '8:00 PM', 
      '9:00 PM', 
      '10:00 PM',
      '11:00 PM',
      '12:00 AM')


usage_data <- dplyr::group_by(data, date, hour, time_local, month, weekday, season) %>% 
  dplyr::summarize(usage = sum(usage, na.rm = TRUE)) %>%
  left_join(weather, by = 'date') %>% 
  mutate(year = year(date))

write_csv(usage_data, './data/usage_data.csv')

usage <- dplyr::group_by(data, date, month, weekday, season) %>% 
  dplyr::summarize(usage = sum(usage, na.rm = TRUE)) %>%
  left_join(weather, by = 'date') %>% 
  mutate(year = year(date))

write_csv(usage, './data/usage_summarized.csv')

breaks <- unique(as.POSIXct(data$time_local))

dplyr::group_by(usage_data, date) %>%
  summarize(usage = sum(usage, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = date, y = usage)) +
  geom_col(aes(color = 'Usage'),
           fill = french,
           width = 0.75) +
  geom_line(aes(y = zoo::rollmean(usage, k = 14, fill = NA),  color = 'Rolling Average'), size = 0.75) +
  theme_minimal() +
  theme(
    text = element_text(
      family = 'Roboto Condensed',
      color = 'black',
      size = 12
    ),
    plot.caption = element_text(hjust = 0.5, size = 10.5),
    legend.position = 'bottom',
    axis.ticks.length = unit(.25, 'cm')
  ) +
  scale_color_manual('', values = c('Usage' = french, 'Rolling Average' = prussian)) +
  scale_x_date(date_labels = '%B, %Y') + 
  scale_y_continuous(labels = number_format(), breaks = seq(0, 120, by = 20)) +
  labs(x = '', y = 'Usage (Kilowatt per hour)\n')


dplyr::group_by(usage_data, date) %>%
  summarize(usage = sum(usage, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(weather, by = 'date') %>% 
  ggplot(aes(x = tmax, y = usage)) +
  geom_jitter(color = sapphire, size = 1) +
  stat_smooth(color = sapphire, size = 0.75, se = FALSE) +
  theme_minimal() +
  theme(
    text = element_text(
      family = 'Roboto Condensed',
      color = 'black',
      size = 12
    ),
    plot.caption = element_text(hjust = 0.5, size = 10.5),
    legend.position = 'bottom',
    axis.ticks.length = unit(.25, 'cm')
  ) +
  scale_x_continuous(breaks = seq(0, 120, by = 10)) + 
  scale_y_continuous(labels = number_format(), breaks = seq(0, 120, by = 20)) +
  labs(x = '\nDaily max temperature', y = 'Usage (Kilowatt per hour)\n')


dplyr::group_by(usage_data, hour, time_local, season) %>%
  summarize(usage = mean(usage, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.POSIXct(time_local, tz = 'America/Los_Angeles'), y = usage, color = factor(season))) +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_minimal() +
  theme(
    text = element_text(
      family = 'Roboto Condensed',
      color = 'black',
      size = 12
    ),
    plot.caption = element_text(hjust = 0.5, size = 10.5),
    legend.position = 'bottom',
    axis.ticks.length = unit(.25, 'cm')
  ) +
  scale_x_datetime(date_labels = '%l') +
  scale_y_continuous(labels = number_format()) +
  scale_color_manual('', values = bupr) +
  labs(x = '', y = 'Usage (Kilowatt per hour\n')


dplyr::group_by(usage_data, hour, time_local, weekday) %>%
  summarize(usage = mean(usage, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = as.POSIXct(time_local, tz = 'America/Los_Angeles'), y = usage, color = factor(weekday))) +
  geom_smooth(size = 0.5, se = FALSE) +
  theme_minimal() +
  theme(
    text = element_text(
      family = 'Roboto Condensed',
      color = 'black',
      size = 12
    ),
    plot.caption = element_text(hjust = 0.5, size = 10.5),
    legend.position = 'bottom',
    axis.ticks.length = unit(.25, 'cm')
  ) +
  scale_x_datetime(date_labels = '%l') +
  scale_y_continuous(labels = number_format()) +
  scale_color_manual('', values = bupr) +
  labs(x = '', y = 'Usage (Kilowatt per hour\n')


dplyr::group_by(usage_data, hour, time_local, weekday, season) %>%
  summarize(usage = mean(usage, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(x = factor(weekday, levels = days), y = usage, color = season)) +
  geom_jitter(size = 1.5, width = 0.15) +
  theme_minimal() +
  theme(
    text = element_text(
      family = 'Roboto Condensed',
      color = 'black',
      size = 12
    ),
    plot.caption = element_text(hjust = 0.5, size = 10.5),
    legend.position = 'bottom',
    axis.ticks.length = unit(.25, 'cm')
  ) +
  scale_y_continuous(labels = number_format()) +
  scale_color_manual('', values = bupr) +
  labs(x = '', y = 'Usage (Kilowatt per hour)\n')


ggplot(usage, aes(
  x = factor(weekday, levels = days),
  y = usage,
  color = factor(season, levels = c('Fall', 'Winter', 'Spring', 'Summer'))
)) +
  geom_jitter(size = 1.5, width = 0.15) +
  theme_minimal() +
  theme(
    text = element_text(
      family = 'Roboto Condensed',
      color = 'black',
      size = 12
    ),
    plot.caption = element_text(hjust = 0.5, size = 10.5),
    legend.position = 'bottom',
    axis.ticks.length = unit(.25, 'cm')
  ) +
  scale_y_continuous(labels = number_format(), breaks = seq(0, 120, by = 20)) +
  scale_color_manual('', values = c(french, prussian, lapis, english_violet)) +
  labs(x = '', y = 'Usage (Kilowatt per hour\n')


# ggplot(usage, aes(x = factor(weekday, levels = days), y = usage)) +
#   geom_segment(aes(xend = weekday, yend = 0), color = 'dark grey', width = 0.5) +
#   geom_point(size = 2, color = lapis) +
#   coord_flip() +
#   theme_tinyhand() +
#   theme(
#     text = element_text(
#       family = 'Roboto Condensed',
#       color = 'black',
#       size = 12
#     ),
#     plot.caption = element_text(hjust = 0.5, size = 10.5),
#     legend.position = 'bottom',
#     axis.ticks.length = unit(.25, 'cm')
#   ) + 
#   labs(x = '', y = '\nUsage (Kw H)')
  
# ggplot(data, aes(x = season, y = usage, fill = season)) +
#   geom_violin() +
#   geom_boxplot(width = 0.1, color = 'grey', alpha = 0.2) +
#   scale_fill_viridis(discrete = TRUE) +
#   theme_minimal() +
#   theme(
#     text = element_text(
#       family = 'Roboto Condensed',
#       color = 'black',
#       size = 11
#     ),
#     plot.caption = element_text(hjust = 0.5, size = 10.5),
#     legend.position = 'bottom',
#     axis.ticks.length = unit(.25, 'cm')
#   ) 


ggplot(usage, aes(
  x = factor(season, levels = c('Fall', 'Winter', 'Spring', 'Summer')),
  y = usage,
  fill = season,
  color = factor(weekday, levels = days))) +
  #  geom_boxplot(width = 0.4) +
  stat_boxplot(geom = 'errorbar',
               color = 'black',
               width = 0.4) +
  geom_jitter(width = 0.15) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis('', discrete = TRUE) +
  theme_minimal() +
  theme(
    text = element_text(
      family = 'Roboto Condensed',
      color = 'black',
      size = 12
    ),
    plot.caption = element_text(hjust = 0.5, size = 10.5),
    legend.position = 'bottom',
    axis.ticks.length = unit(.25, 'cm')
  ) +
  scale_y_continuous(breaks = seq(0, 120, by = 20)) +
  guides(fill = 'none') + 
  labs(x = '', y = 'Usage (Kilowatt per hour)\n')

ggplot(usage, aes(
  x = factor(season, levels = c('Fall', 'Winter', 'Spring', 'Summer')),
  y = usage,
  fill = factor(year))) +
  scale_fill_manual('', values = c(french, prussian, english_violet)) +
  geom_violin(
    alpha = 0.5,
    size = 1,
    color = 'black',
    position = position_dodge(width = 0.75)
  ) +
  geom_boxplot(
    notch = FALSE,
    outlier.size = -1,
    color = 'black',
    width = 0.25,
    position = position_dodge(width = 0.75),
    lwd = 0.5,
    alpha = 0.7
  ) +
  geom_point(
    shape = 21,
    size = 0.5,
    position = position_jitterdodge(),
    color = 'black'
  ) +
  theme_minimal() +
  theme(
    text = element_text(
      family = 'Roboto Condensed',
      color = 'black',
      size = 11
    ),
    plot.caption = element_text(hjust = 0.5, size = 10.5),
    legend.position = 'bottom',
    axis.ticks.length = unit(.25, 'cm')
  ) +
  scale_y_continuous(breaks = seq(0, 120, by = 20)) + 
  labs(x = '', y = 'Usage (Kilowatt per hour)\n') 



n_per_sim <- nrow(usage)
num_sims <- 100
mu <- mean(usage$usage)
sigma <- sd(usage$usage)

set.seed(12345) #seed is necessay for reproducibility
sim_output <- rep(NA, times = num_sims) #This creates a vector of 10 NA values

for (sim_number in 1:num_sims){ #this starts your for loop
  data <- rnorm(n = n_per_sim, mean = mu, sd = sigma) #generate your data
  sim_output[sim_number] <- data #this is where you store your output for each simulation
}

simulated_results <- NULL
for (i in seq_len(num_sims)) {
  # reproducible sampled data frames
  set.seed(i)
  df <- select(usage, usage) %>% ungroup()
  index <- sample(seq_len(nrow(df)), nrow(df), replace = TRUE)
  df_sim <- df[index, ]
  # simulate results
  df_sim$usage <- rnorm(n = n_per_sim, mean = mu, sd = sigma) 
  df_sim$ob <- index
  simulated_results <- rbind(simulated_results, df_sim)
}


usage_df <- usage %>% ungroup() %>% 
  select(date, usage)


bootstrap_n <- 100
bootstrap_results <- NULL
for(i in seq_len(bootstrap_n)) {
  # reproducible sampled data frames
  set.seed(i)
  index <- sample(seq_len(nrow(usage_df)), nrow(usage_df), replace = TRUE)
  df_sim <- usage_df[index, ]
  df_sim <- df_sim[, 2]
  df_sim <- ts(df_sim, frequency = 7)
  # fit model and add predictions to results data frame
  fit <- lm(usage ~ lag(usage, 1), data = df_sim)
  fit <- auto.arima(df_sim, seasonal = TRUE, d = 1, xreg = fourier(df_sim, K = 1))
  df_sim$predictions <- predict(fit, df_sim)
  df_sim$model <- paste0("customer", i)
  bootstrap_results <- rbind(bootstrap_results, df_sim)
}

usage_df <- mutate(usage_df, date = as.Date(date))
index <- sample(seq_len(nrow(usage_df)), nrow(usage_df), replace = TRUE)
df_sim <- usage_df[index, ]
df_sim <- df_sim[, 2]

df_sim <-  ts(df_sim, frequency = 7)


ar_fit <- auto.arima(df_sim, seasonal = TRUE, d = 1, xreg = fourier(df_sim, K = 3))


ar_fit <- Arima(df_sim, order = c(1, 1, 0), seasonal = c(0, 1, 7), lambda = 0)

summary(ar_fit)

yhat <-  as.data.frame(fitted(ar_fit)) %>% rename(usage_fitted = 1) 
usage_df <- bind_cols(usage_df, yhat)



ggplot(usage_df, aes(x = date)) + 
  geom_line(aes(y = usage, color = 'Actual')) + 
  geom_line(aes(y = usage_fitted, color = 'Fitted')) + 
  scale_color_manual('', values = c('Actual' = sapphire, 'Fitted' = lilac))



plots <- list()
for (i in seq(3)) {
  fit <- auto.arima(
    df_sim,
    xreg = fourier(df_sim, K = i),
    seasonal = FALSE,
    lambda = 0
  )
  plots[[i]] <- autoplot(forecast(fit,
                                  xreg = fourier(df_sim, K = i, h = 24))) +
    xlab(paste("K=", i, "   AICC=", round(fit[["aicc"]], 2))) +
    ylab("")
}

gridExtra::grid.arrange(plots[[1]], plots[[2]], plots[[3]], nrow = 3)



rep(1:7,7)

# Create matrix of numeric predictors
xreg <-
  cbind(
    weekday = model.matrix(~as.factor(usage$weekday)),
    season = model.matrix(~as.factor(usage$season))
  )

# Remove intercept
xreg <- xreg[, -c(1, 8)]
