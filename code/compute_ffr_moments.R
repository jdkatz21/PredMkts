library(tidyverse)
library(lubridate)


setwd('/Users/jaredkatz/Documents/Research/PredictionMarkets')

# Read CSV
df <- read_csv("data/distributions_all.csv")

# Convert datetime and extract date
df <- df %>%
  mutate(created_time = as.POSIXct(created_time, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
         date = as.Date(created_time))

# Extract contract_preamble (e.g., FED-22DEC)
df <- df %>%
  mutate(contract_preamble = str_extract(ticker, "^FED-\\d{2}[A-Z]{3}"))

# Extract strike (final number after -T) and convert to numeric
df <- df %>%
  mutate(strike = as.numeric(str_extract(ticker, "-T(\\d+\\.?\\d*)") %>% str_replace("-T", ""))) %>%
  arrange(contract_preamble, date, strike)

# Step 1: Last trading date per contract
last_active_date <- df %>%
  group_by(contract_preamble) %>%
  summarise(last_date = max(date), .groups = "drop")

first_active_date <- df %>%
  group_by(contract_preamble) %>%
  summarise(first_date = min(date), .groups = "drop")

# Step 2: Get all strikes per contract
strike_map <- df %>%
  distinct(contract_preamble, strike)

# Step 3: Get all dates per contract up to last active date
date_grid <- last_active_date %>%
  rowwise() %>%
  mutate(date = list(seq.Date(from = min(df$date), to = last_date, by = "day"))) %>%
  unnest(cols = c(date))

# Step 4: Create full grid of contract × strike × date
full_grid <- strike_map %>%
  inner_join(date_grid, by = "contract_preamble", relationship = "many-to-many")

# Step 5: Compute last trade info
last_trades <- df %>%
  arrange(contract_preamble, strike, date, created_time) %>%
  group_by(contract_preamble, strike, date) %>%
  summarise(
    last_yes_price = last(yes_price),
    daily_volume = sum(count),
    .groups = "drop"
  )

# Step 6: Merge and apply LOCF only within active period
filled <- full_grid %>%
  left_join(last_trades, by = c("contract_preamble", "strike", "date")) %>%
  arrange(contract_preamble, strike, date) %>%
  group_by(contract_preamble, strike) %>%
  mutate(
    last_yes_price = zoo::na.locf(last_yes_price, na.rm = FALSE),
    daily_volume = replace_na(daily_volume, 0)
  ) %>%
  ungroup()

pdf_df <- filled %>%
  arrange(contract_preamble, date, strike) %>%
  group_by(contract_preamble, date) %>%
  mutate(
    raw_prob = pmax(0, last_yes_price - lead(last_yes_price)),
    total = sum(raw_prob, na.rm = TRUE),
    probability = ifelse(total > 1e-6, 100 * raw_prob / total, 0)
  ) %>%
  ungroup() %>%
  select(-raw_prob, -total) %>%
  rename(time.index = date)

trim_distribution_tails <- function(pdf_df) {
  pdf_df %>%
    arrange(contract_preamble, time.index, strike) %>%
    group_by(contract_preamble, time.index) %>%
    group_modify(~ {
      df <- .x
      
      # Clone for modification
      probs <- df$probability
      strikes <- df$strike
      median_strike <- median(strikes, na.rm = TRUE)
      
      for (i in seq_along(probs)) {
        if (!is.na(probs[i]) & probs[i] < 2) {
          # Determine target index to absorb the small prob
          if (i == 1) {
            target <- i + 1
          } else if (i == length(probs)) {
            target <- i - 1
          } else if (strikes[i] > median_strike) {
            target <- i - 1  # push left
          } else {
            target <- i + 1  # push right
          }
          
          # Add and zero out
          if (!is.na(probs[target])) {
            probs[target] <- probs[target] + probs[i]
          }
          probs[i] <- 0
        }
      }
      
      df$probability <- probs
      df
    }) %>%
    ungroup()
}

plot_probability_histogram <- function(pdf_df, contract, date) {
 
  # Define fixed strike bins
  fixed_strikes <- seq(0, 5, by = 0.25)
  
  # Filter and align with fixed strike range
  plot_data <- pdf_df %>%
    filter(contract_preamble == contract, time.index == as.Date(date)) %>%
    select(strike, probability) %>%
    filter(!is.na(probability)) %>%
    complete(strike = fixed_strikes, fill = list(probability = 0)) %>%
    arrange(strike)
  
  # Create histogram-style bar plot
  barplot(
    height = plot_data$probability,
    names.arg = plot_data$strike,
    col = "lightblue",
    border = "darkblue",
    main = paste("Probability Distribution for", contract, "on", date),
    xlab = "Strike Interval (lower bound)",
    ylab = "Probability",
    ylim = c(0, 100)  # fixed y-axis from 0 to 100
  )
  
}


plot_probability_histogram(pdf_df, 'FED-22DEC', '2022-04-14')
plot_probability_histogram(pdf_df, 'FED-22DEC', '2022-05-14')
plot_probability_histogram(pdf_df, 'FED-22DEC', '2022-06-14')
plot_probability_histogram(pdf_df, 'FED-22DEC', '2022-07-14')
plot_probability_histogram(pdf_df, 'FED-22DEC', '2022-08-14')
plot_probability_histogram(pdf_df, 'FED-22DEC', '2022-09-14')
plot_probability_histogram(pdf_df, 'FED-22DEC', '2022-10-14')
plot_probability_histogram(pdf_df, 'FED-22DEC', '2022-11-14')
plot_probability_histogram(pdf_df, 'FED-22DEC', '2022-12-14')


library(magick)

animate_distribution <- function(pdf_df, contract, start_date, end_date, output_file = "distribution_evolution_dec22.gif") {
  # Generate sequence of dates
  date_seq <- seq(as.Date(start_date), as.Date(end_date), by = "1 day")
  
  # Fixed strike bins
  fixed_strikes <- seq(0, 5, by = 0.25)
  
  # List to store plots
  img_list <- list()
  
  for (d in date_seq) {
    # Prepare plot data
    plot_data <- pdf_df %>%
      filter(contract_preamble == contract, time.index == d) %>%
      select(strike, probability) %>%
      filter(!is.na(probability)) %>%
      tidyr::complete(strike = fixed_strikes, fill = list(probability = 0)) %>%
      arrange(strike)
    
    # Skip frame if all probabilities are 0 (no data)
    if (all(plot_data$probability == 0 | is.na(plot_data$probability))) {
      next
    }
    
    # Draw plot to a temporary PNG file
    png_file <- tempfile(fileext = ".png")
    png(png_file, width = 600, height = 400)
    barplot(
      height = plot_data$probability,
      names.arg = plot_data$strike,
      col = "lightblue",
      border = "darkblue",
      main = paste("Distribution for", contract, "\n", format(as.Date(d), "%b %d, %Y")),
      xlab = "Strike Interval (lower bound)",
      ylab = "Probability",
      ylim = c(0, 100)
    )
    
    #  # Sum daily volume
    # total_volume <- sum(plot_data$daily_volume, na.rm = TRUE)
    # 
    # # Add total volume label in top-right
    # text(
    #   x = max(bar_positions) * 0.95,
    #   y = 95,
    #   labels = paste("Volume:", total_volume),
    #   adj = c(1, 0),
    #   cex = 0.9
    # )
    
    
    dev.off()
    
    # Read the PNG into magick
    img <- image_read(png_file)
    img_list[[length(img_list) + 1]] <- img
  }
  
  
  
  # Combine images into animation
  animation <- image_animate(image_join(img_list), fps = 4)
  image_write(animation, output_file)
  
  cat("Animation saved to:", output_file, "\n")
}


library(av)

animate_distribution_mov <- function(pdf_df, contract, start_date, end_date, output_file = "distribution_evolution.mov") {
  # Create temp dir for frames
  frame_dir <- tempfile()
  dir.create(frame_dir)
  
  # Fixed bins and dates
  date_seq <- seq(as.Date(start_date), as.Date(end_date), by = "1 day")
  fixed_strikes <- seq(0, 5, by = 0.25)
  
  frame_paths <- c()
  
  for (d in date_seq) {
    plot_data <- pdf_df %>%
      filter(contract_preamble == contract, time.index == d) %>%
      select(strike, probability, daily_volume) %>%
      tidyr::complete(strike = fixed_strikes, fill = list(probability = 0, daily_volume = 0)) %>%
      arrange(strike)

    
    # Sum volume
    total_volume <- sum(plot_data$daily_volume, na.rm = TRUE)
    
    # Save frame
    frame_path <- file.path(frame_dir, paste0("frame_", d, ".png"))
    png(frame_path, width = 800, height = 500)
    
    bar_positions <- barplot(
      height = plot_data$probability,
      names.arg = plot_data$strike,
      col = "lightblue",
      border = "darkblue",
      main = paste("Distribution for", contract, "\n", format(as.Date(d), "%b %d, %Y")),
      xlab = "Strike Interval (lower bound)",
      ylab = "Probability",
      ylim = c(0, 100)
    )
    
    text(
      x = max(bar_positions) * 0.95,
      y = 95,
      labels = paste("Volume:", total_volume),
      adj = c(1, 0),
      cex = 0.9
    )
    
    dev.off()
    frame_paths <- c(frame_paths, frame_path)
  }
  
  # Only create video if frames exist
  if (length(frame_paths) == 0) {
    cat("No frames to encode — no video created.\n")
    return(NULL)
  }
  
  # Use av to create .mov file
  av_encode_video(
    input = frame_paths,
    framerate = 4,
    output = output_file,
    vfilter = "scale=trunc(iw/2)*2:trunc(ih/2)*2"  # ensure even dimensions
  )
  
  cat("Video saved to:", output_file, "\n")
}

compute_distribution_stats <- function(pdf_df) {
  pdf_df %>%
    arrange(contract_preamble, time.index, strike) %>%
    group_by(contract_preamble, time.index) %>%
    mutate(
      total_prob = sum(probability, na.rm = TRUE),
      prob_norm = ifelse(total_prob > 0, probability / total_prob, 0),
      cum_prob = cumsum(prob_norm)
    ) %>%
    summarise(
      mean = sum(strike * prob_norm, na.rm = TRUE),
      median = strike[which.min(abs(cum_prob - 0.5))],
      mode = strike[which.max(prob_norm)],
      .groups = "drop"
    )
}



pdf_df <- trim_distribution_tails(pdf_df)
moments <- compute_distribution_stats(pdf_df)

write_csv(pdf_df, 'output/daily_timeseries.csv')
write_csv(moments, 'output/moments.csv')

animate_distribution_mov(pdf_df, contract = "FED-22DEC", start_date = "2021-12-14", end_date = "2022-12-14", output_file = "output/evolution_movs/distribution_evolution_dec22.mov")
animate_distribution_mov(pdf_df, contract = "FED-25JUN", start_date = "2024-06-09", end_date = "2025-06-09", output_file = "output/evolution_movs/distribution_evolution_june25.mov")
animate_distribution_mov(pdf_df, contract = "FED-25JUL", start_date = "2024-07-30", end_date = "2025-06-09", output_file = "output/evolution_movs/distribution_evolution_july25.mov")
animate_distribution_mov(pdf_df, contract = "FED-25SEP", start_date = "2024-09-17", end_date = "2025-06-09", output_file = "output/evolution_movs/distribution_evolution_sep25.mov")
animate_distribution_mov(pdf_df, contract = "FED-25OCT", start_date = "2024-10-29", end_date = "2025-06-09", output_file = "output/evolution_movs/distribution_evolution_oct25.mov")
animate_distribution_mov(pdf_df, contract = "FED-25DEC", start_date = "2024-12-10", end_date = "2025-06-09", output_file = "output/evolution_movs/distribution_evolution_dec25.mov")

