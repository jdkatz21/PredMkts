library(tidyverse)
library(lubridate)
library(av)

setwd('/Users/jaredkatz/Documents/Research/PredictionMarkets')

# loads trade level data
read_data <- function() {
  
  df <- read_csv("data/recession_probs.csv")
  
  # Convert datetime and extract date
  df <- df %>%
    mutate(created_time = as.POSIXct(created_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
           date = as.Date(created_time))
  
  # Extract contract_preamble (e.g., FED-22DEC) and strike price
  df <- df %>%
    mutate(
      contract_preamble = str_extract(ticker, "^[^-]+(?:-[^-]+)*"),
      strike = 'prob_no_rec'
    ) %>% 
    arrange(contract_preamble, strike, date)
  
  return(df)
}

# takes the last trade of every day as the daily value
convert_to_daily <- function(df) {
  
  df <- df %>% group_by(date, contract_preamble, strike) %>%
    reframe(date=date, 
            contract_preamble = contract_preamble,
            strike = strike,
            yes_price = weighted.mean(yes_price, count),
            daily_volume = sum(count)) %>% distinct() %>%
    arrange(contract_preamble, strike, date)
  
}

# fill in the gaps between missing days
fill_dataless_days <- function(df) {
  
  # Determine the full date range
  strikes = df$strike %>% unique()
  preambles = df$contract_preamble %>% unique()
  dates =  seq(min(df$date), max(df$date), by = "day")
  
  full_date_range <- expand.grid(date = dates, strike = strikes, contract_preamble = preambles)
  
  # Merge with current df
  df <- df %>% full_join(full_date_range) %>% 
    arrange(contract_preamble, strike, date)
  
  # get the contract expiry date
  df <- df %>%
    group_by(contract_preamble) %>%
    mutate(
      expiry_date = if (all(is.na(yes_price))) NA_Date_ else max(date[!is.na(yes_price)])
    ) %>%
    ungroup()
  
  # fill NA rows with last price and fill in 0 for daily volume on these days
  df <- df %>%
    group_by(strike) %>%
    fill(yes_price, .direction = "down") %>%
    ungroup() %>% mutate(
      daily_volume = ifelse(is.na(daily_volume), 0, daily_volume)
    )
  
  # remove the rows at the start with no price, rows after the expiry date, and
  # rows for bins that never existed
  df <- df %>% na.omit() %>%
    filter(
      date <= expiry_date
    )
  
  return(df)
}

# clean the data
clean_data <- function(df) {
  
  # filter 6 months before contract expiry
  # df <- df %>%
  #   filter(
  #     date >= expiry_date - months(6),
  #   ) %>%
  #   arrange(contract_preamble, strike, date)
  
  df <- df %>%
    group_by(contract_preamble, date) %>%
    arrange(desc(strike), .by_group = TRUE) %>%
    mutate(
      adjusted_yes_price = cummax(yes_price),  # enforce monotonic non-decreasing prices
    ) %>%
    ungroup()
  
  get_next_distinct <- function(x) {
    sapply(seq_along(x), function(i) {
      rest <- x[(i + 1):length(x)]
      next_val <- rest[rest != x[i]]
      if (length(next_val) > 0) next_val[1] else x[i]
    })
  }
  
  # if bin is equal to bin before it and price is less than 50 (below median), set bin equal to next different bin
  # df <- df %>%
  #   group_by(contract_preamble, date) %>%
  #   arrange(strike, .by_group = TRUE) %>%
  #   mutate(
  #     next_distinct_price = get_next_distinct(adjusted_yes_price),
  #     flat_bins = adjusted_yes_price == lag(adjusted_yes_price),
  #     adjusted_yes_price = ifelse(adjusted_yes_price < 50 & adjusted_yes_price == lag(adjusted_yes_price), next_distinct_price, adjusted_yes_price)
  #   )
  
  return(df)
  
}

# convert to probabilities
convert_to_probabilities <- function(df) {
  
  # Start with 99 on the left-most bin, then keep subtracting left minus right
  df <- df %>% group_by(contract_preamble, date) %>% arrange(strike) %>%
    mutate(probability = 
             98 - adjusted_yes_price
             )
  
  return(df)
}


# create animation
animate_distribution_mov <- function(df, contract, start_date, end_date, output_file = "distribution_evolution.mov", col_to_graph = 'cdf') {
  
  # replace if we should use the adjusted
  if(col_to_graph == 'cdf') {
    df <- df %>% mutate(graph_col = adjusted_yes_price)
    label <- 'CDF'
  } else if (col_to_graph == 'pdf') {
    df <- df %>% mutate(graph_col = probability)
    label <- 'PDF'
  } else if (col_to_graph == 'prices') {
    df <- df %>% mutate(graph_col = yes_price)
    label <- 'Prices'
  }
  
  # Filter the data for contract and date range
  filtered_df <- df %>%
    filter(contract_preamble == contract,
           date >= as.Date(start_date),
           date <= as.Date(end_date))
  
  
  # Create the bar chart animation
  p <- ggplot(filtered_df, aes(x = factor(strike), y = graph_col)) +
    geom_col(fill = "steelblue") +
    labs(title = 'Average Yes Price per Strike: {frame_time}',
         x = 'Strike',
         y = label) +
    transition_time(date) +
    ease_aes('linear') +
    theme_minimal() + 
    scale_y_continuous(limits = c(0, 100))
  
  # Animate and save as .mov
  animate(p, renderer = av_renderer(output_file), fps = 10, width = 800, height = 600)
}


df <- read_data()
df <- convert_to_daily(df)
df <- fill_dataless_days(df)
df <- clean_data(df)
df <- convert_to_probabilities(df)

animate_distribution_mov(df, contract = "RECSSNBER-23", start_date = "2023-01-01", end_date = "2024-01-01", output_file = "output/evolution_movs/recession/cdf/cdf23.mov")
animate_distribution_mov(df, contract = "RECSSNBER-23", start_date = "2023-01-01", end_date = "2024-01-01", output_file = "output/evolution_movs/recession/pdf/pdf23.mov", col_to_graph = 'pdf')

animate_distribution_mov(df, contract = "RECSSNBER-24", start_date = "2024-01-01", end_date = "2025-01-01", output_file = "output/evolution_movs/recession/cdf/cdf24.mov")
animate_distribution_mov(df, contract = "RECSSNBER-24", start_date = "2024-01-01", end_date = "2025-01-01", output_file = "output/evolution_movs/recession/pdf/pdf24.mov", col_to_graph = 'pdf')

animate_distribution_mov(df, contract = "RECSSNBER-25", start_date = "2024-08-01", end_date = "2026-01-01", output_file = "output/evolution_movs/recession/cdf/cdf25.mov")
animate_distribution_mov(df, contract = "RECSSNBER-25", start_date = "2024-08-01", end_date = "2026-01-01", output_file = "output/evolution_movs/recession/pdf/pdf25.mov", col_to_graph = 'pdf')


