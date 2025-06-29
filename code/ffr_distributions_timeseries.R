library(tidyverse)
library(lubridate)
library(av)
library(matrixStats)


setwd('/Users/jaredkatz/Documents/Research/PredictionMarkets')

# median helper function
closest_strike <- function(x, levels) {
  levels[which.min(abs(levels - x))]
}

# loads trade level data
read_data <- function() {
  
  df <- read_csv("data/distributions_all.csv")

  # Convert datetime and extract date
  df <- df %>%
    mutate(created_time = as.POSIXct(created_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
           date = as.Date(created_time))
  
  # Extract contract_preamble (e.g., FED-22DEC) and strike price
  df <- df %>%
    mutate(
      contract_preamble = str_extract(ticker, "^[^-]+(?:-[^-]+)*"),
      contract_preamble = str_replace(contract_preamble, "-T\\d+\\.?\\d*$", ""),
      contract_preamble = ifelse(contract_preamble == 'FED-22JULY', 'FED-22JUL', contract_preamble),
      strike = as.numeric(str_extract(ticker, "(?<=-T)\\d+\\.?\\d*"))
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
              yes_price = last(yes_price),
              daily_volume = sum(count)) %>% distinct() %>%
    arrange(contract_preamble, strike, date)
  
}

# fill in the gaps between missing days
fill_dataless_days <- function(df) {
  
  # Get unique strike-preamble combinations that actually exist
  valid_combos <- df %>% select(contract_preamble, strike) %>% distinct()
  
  # Get full date range
  dates <- seq(min(df$date), max(df$date), by = "day")
  
  # Create full date range for only valid strike-preamble combos
  full_date_range <- valid_combos %>%
    crossing(date = dates)
  
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
  df <- df %>%
    filter(
      date >= expiry_date - months(6),
    ) %>%
    arrange(contract_preamble, strike, date)
  
  # get the next highest bin
  df <- df %>% group_by(contract_preamble, date) %>% arrange(strike) %>%
    mutate(
      bin_high = lead(strike)
    )

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
  
  # Add low bins
  all_cols <- names(df)

  # Create one row per unique contract_preamble/date/expiry_date combo
  # new_rows <- df %>%
  #   distinct(contract_preamble, date, expiry_date) %>%
  #   mutate(strike = 0)
  
  new_rows <- df %>%
    group_by(contract_preamble, date, expiry_date) %>%
    summarise(strike = min(strike) - 0.25, .groups = "drop")
  
  df <- bind_rows(df, new_rows)
  
  # Start with 99 on the left-most bin, then keep subtracting left minus right
  df <- df %>% group_by(contract_preamble, date) %>% arrange(strike) %>%
            mutate(probability = 
             ifelse(is.na(lag(strike)), 99 - lead(adjusted_yes_price), 
                    # lag(adjusted_yes_price) - adjusted_yes_price
                    ifelse(!is.na(lead(strike)), adjusted_yes_price - lead(adjusted_yes_price), adjusted_yes_price - 1)
                    ))
  
  
  # swap the probabilities
  swap_probabilities <- function(df_group) {
    print(df_group$contract_preamble[1])
    # Ensure sorted order
    df_group <- df_group %>% arrange(strike) %>% mutate(swapped = FALSE)
    
    # Convert to regular data.frame to update values by reference
    
    nrows <- nrow(df_group) - 1
    
    if (nrows > 2) {
      for (i in 2:nrows) {
        
        # push the low end of the distribution towards the right
        if (
          df_group$adjusted_yes_price[i] > 50 &&
          df_group$probability[i] == 0 &&
          df_group$probability[i - 1] != 0
        ) {
          # Swap x[i] and x[i-1]
          df_group$probability[i] <- df_group$probability[i - 1]
          df_group$probability[i - 1] <- 0
          df_group$swapped[i] <- TRUE
          
        }
        
        # push the high end of the distribution towards the left
        if (
          df_group$adjusted_yes_price[i] < 50 &&
          df_group$probability[i] == 0 &&
          df_group$probability[i + 1] != 0
        ) {
          # Swap x[i] and x[i-1]
          df_group$probability[i] <- df_group$probability[i + 1]
          df_group$probability[i + 1] <- 0
          df_group$swapped[i] <- TRUE
          
        }
        
      }
      
    }
    
    
    return(df_group)
  }
  
  # Now apply to each group until 
  still_need_to_swap <- TRUE
  
  while(still_need_to_swap) {
    
    df <- df %>%
      group_by(contract_preamble, date) %>%
      group_split() %>%
      map_dfr(swap_probabilities)
    
    print(df %>% filter(swapped == TRUE))
    still_need_to_swap <- any(df$swapped)
  }

  df <- df %>% group_by(contract_preamble, date) %>% 
    filter(!any(probability == 98)) %>%
    ungroup()
  
  
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

# return a new dataframe with the day and contract preamble and
# mean, median, mode, variance, skewness, kurtosis
weightedGMSkew <- function(x, w, na.rm = TRUE) {
  if (na.rm) {
    sel <- !is.na(x) & !is.na(w)
    x <- x[sel]; w <- w[sel]
  }
  w <- w / sum(w)
  mu <- sum(w * x)
  # weighted median
  ord <- order(x); x_o <- x[ord]; w_o <- w[ord]
  cumw <- cumsum(w_o)
  m_w <- x_o[min(which(cumw >= 0.5))]
  mad <- sum(w * abs(x - m_w))
  (mu - m_w) / mad
}

get_moments <- function(df) {
  
  df <- df %>%
    group_by(date, contract_preamble, expiry_date) %>%
      summarise(
        mean     = sum(probability * strike, na.rm = TRUE) / sum(probability, na.rm = TRUE),
        median   = weightedMedian(strike, w = probability, na.rm = TRUE, interpolate = FALSE),
        mode = fmode(strike, w = probability, na.rm = TRUE, ties='first'),
        skewness = weightedGMSkew(strike, w = probability, na.rm = TRUE),
        kurtosis = DescTools::Kurt(strike, w = probability, na.rm = TRUE),
        variance = sum(probability * (strike - (sum(probability * strike) / sum(probability)))^2, na.rm = TRUE) / sum(probability, na.rm = TRUE),
        .groups = "drop"
      )
  return(df)
}


# plot the mean and variance over time for a contract
plot_contract_stats <- function(df, contract_name) {
  
  # Filter for the specified contract
  contract_data <- df %>%
    filter(contract_preamble == contract_name) %>%
    select(date, mean, variance, median) %>%
    arrange(date)

  # Rescale variance to mean range for plotting
  mean_range <- range(c(contract_data$mean, contract_data$median), na.rm = TRUE)
  var_range <- range(contract_data$variance, na.rm = TRUE)
  scale_factor <- diff(mean_range) / diff(var_range)
  variance_scaled <- (contract_data$variance - var_range[1]) * scale_factor + mean_range[1]
  
  # Adjust plot margins for dual axes
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(5, 4, 4, 6))  # Bottom, left, top, right
  
  # Plot mean
  plot(contract_data$date, contract_data$mean, type = "l", col = "dodgerblue",
       ylim = mean_range, xlab = "Date", ylab = "Mean / Median",
       main = paste("Mean, Median, and Variance for", contract_name))
  
  # Add median line
  lines(contract_data$date, contract_data$median, col = "darkred", lty = 1)
  
  # Add scaled variance on same plot
  lines(contract_data$date, variance_scaled, col = "aquamarine4", lty = 1)
  
  # Add right-side axis for variance
  axis(4, 
       at = seq(mean_range[1], mean_range[2], length.out = 5),
       labels = round(seq(var_range[1], var_range[2], length.out = 5), 2))
  mtext("Variance", side = 4, line = 3)
  
  # Add legend
  legend("topleft",
         legend = c("Mean", "Median", "Variance"),
         col = c("dodgerblue", "darkred", "aquamarine4"),
         lty = c(1, 1, 1),
         bty = "n", inset = c(0.02, 0.05))
}

df <- read_data()
df <- convert_to_daily(df)
df <- fill_dataless_days(df)
df <- clean_data(df)
df <- convert_to_probabilities(df)



moments_df <- get_moments(df)

# save moments df to export to secured computer for comparison with pdf
write_csv(moments_df, 'kalshi_ffr_moments.csv')
write_csv(df, 'kalshi_ffr_distributions.csv')


animate_distribution_mov(df, contract = "FED-25MAY", start_date = "2022-01-01", end_date = "2025-06-19", output_file = "output/evolution_movs/ffr/distribution_evolution_may2025.mov", col_to_graph = 'pdf')

# animate_distribution_mov(df, contract = "FED-22DEC", start_date = "2022-06-14", end_date = "2022-12-14", output_file = "output/evolution_movs/ffr/distribution_evolution_dec22.mov")
# animate_distribution_mov(df, contract = "FED-25JAN", start_date = "2025-01-01", end_date = "2025-06-09", output_file = "output/evolution_movs/ffr/distribution_evolution_june25.mov")
# animate_distribution_mov(df, contract = "FED-24JUN", start_date = "2024-06-01", end_date = "2024-07-01", output_file = "output/evolution_movs/ffr/distribution_evolution_jan25.mov")
# animate_distribution_mov(df, contract = "FED-25MAR", start_date = "2024-06-01", end_date = "2025-07-01", output_file = "output/evolution_movs/ffr/distribution_evolution_mar25.mov")
# animate_distribution_mov(df, contract = "FED-25MAY", start_date = "2024-06-01", end_date = "2025-07-01", output_file = "output/evolution_movs/ffr/distribution_evolution_may25.mov")
# 
# animate_distribution_mov(df, contract = "FED-25JAN", start_date = "2024-06-01", end_date = "2025-07-01", output_file = "output/evolution_movs/ffr/distribution_evolution_jan25_pdf.mov", col_to_graph = 'pdf')
# animate_distribution_mov(df, contract = "FED-25MAR", start_date = "2024-06-01", end_date = "2025-07-01", output_file = "output/evolution_movs/ffr/distribution_evolution_mar25_pdf.mov", col_to_graph = 'pdf')
# animate_distribution_mov(df, contract = "FED-25MAY", start_date = "2024-06-01", end_date = "2025-07-01", output_file = "output/evolution_movs/ffr/distribution_evolution_may25_pdf.mov", col_to_graph = 'pdf')
# 
# plot_contract_stats(moments_df, contract = "FED-22DEC")
# plot_contract_stats(moments_df, contract = "FED-25JUL")

