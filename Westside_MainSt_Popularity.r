library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(glmmTMB)
library(stringr)
library(scales)

# Read original/raw data

data <- read.csv(
  "cleaned_up_occt_data.csv",
  stringsAsFactors = FALSE
)


# Question at hand: Does Main Street Inbound become more attractive when Westside Inbound is crowded/full?




# Inbound


# Prep raw data

data_clean <- data %>%
  mutate(
    Hour   = as.integer(substr(Time, 1, 2)),
    Minute = as.integer(substr(Time, 4, 5)),
    
    Date = as.Date(as.character(Date), format = "%Y-%m-%d"),
    Year = as.integer(format(Date, "%Y")),
    Month = as.integer(format(Date, "%m")),
    
    Season = case_when(
      Month %in% c(12, 1, 2)  ~ "Winter",
      Month %in% c(3, 4, 5)   ~ "Spring",
      Month %in% c(6, 7, 8)   ~ "Summer",
      Month %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ NA_character_
    ),
    
    Season = factor(
      Season,
      levels = c("Winter", "Spring", "Summer", "Fall")
    ),
    
    DayOfWeek = factor(
      weekdays(Date),
      levels = c(
        "Monday",
        "Tuesday",
        "Wednesday",
        "Thursday",
        "Friday",
        "Saturday",
        "Sunday"
      )
    ),
    
    datetime = as.POSIXct(
      paste(Date, Time),
      format = "%Y-%m-%d %H:%M:%S"
    ),
    
    stop_base = StopName
  )

# Keep only inbound routes

inbound_raw <- data_clean %>%
  filter(CorridorName %in% c("Westside Inbound", "Main Street Inbound")) %>%
  arrange(Date, datetime, VehicleID)

# Add fullness for BOTH inbound routes

add_inbound_fullness <- function(data,
                                 bus_capacity = 51,
                                 reset_stop_pattern = "University Union",
                                 gap_reset_minutes = 25) {
  
  data <- data %>%
    mutate(
      onboard_count = NA_real_,
      fullness_decimal = NA_real_
    ) %>%
    arrange(Date, VehicleID, CorridorName, datetime)
  
  # One counter per Date + CorridorName + VehicleID
  current_counts <- list()
  last_times <- list()
  
  for (i in seq_len(nrow(data))) {
    
    this_route <- data$CorridorName[i]
    
    if (this_route %in% c("Westside Inbound", "Main Street Inbound")) {
      
      this_key <- paste(
        data$Date[i],
        this_route,
        data$VehicleID[i],
        sep = "_"
      )
      
      # Start new counter if this date/route/vehicle has not appeared yet
      if (is.null(current_counts[[this_key]])) {
        current_counts[[this_key]] <- 0
        last_times[[this_key]] <- data$datetime[i]
      }
      
      # Reset if same vehicle/route has a large time gap
      time_gap <- as.numeric(
        difftime(data$datetime[i], last_times[[this_key]], units = "mins")
      )
      
      if (!is.na(time_gap) && time_gap > gap_reset_minutes) {
        current_counts[[this_key]] <- 0
      }
      
      # Each raw row represents one boarding
      current_counts[[this_key]] <- current_counts[[this_key]] + 1
      
      # Store onboard count and decimal fullness
      data$onboard_count[i] <- current_counts[[this_key]]
      data$fullness_decimal[i] <- current_counts[[this_key]] / bus_capacity
      
      # Update last time for this route/vehicle/day
      last_times[[this_key]] <- data$datetime[i]
      
      # Reset after University Union if it appears in boarding data
      if (str_detect(data$stop_base[i], reset_stop_pattern)) {
        current_counts[[this_key]] <- 0
      }
    }
  }
  
  data <- data %>%
    arrange(Date, datetime, VehicleID) %>%
    select(
      any_of(c(
        "Date",
        "Time",
        "datetime",
        "Year",
        "Season",
        "DayOfWeek",
        "Hour",
        "Minute",
        "VehicleID",
        "CorridorName",
        "StopName",
        "stop_base",
        "onboard_count",
        "fullness_decimal"
      ))
    )
  
  return(data)
}

inbound_with_fullness <- add_inbound_fullness(
  data = inbound_raw,
  bus_capacity = 51,
  reset_stop_pattern = "University Union",
  gap_reset_minutes = 25
)

# Check fullness for both inbound routes

print(
  inbound_with_fullness %>%
    group_by(CorridorName) %>%
    summarise(
      n = n(),
      missing_fullness = sum(is.na(fullness_decimal)),
      min_fullness = min(fullness_decimal, na.rm = TRUE),
      median_fullness = median(fullness_decimal, na.rm = TRUE),
      mean_fullness = mean(fullness_decimal, na.rm = TRUE),
      max_fullness = max(fullness_decimal, na.rm = TRUE),
      over_capacity = sum(fullness_decimal > 1, na.rm = TRUE),
      .groups = "drop"
    )
)

# Aggregate to model level

model_base <- inbound_with_fullness %>%
  group_by(stop_base, Date, Hour, DayOfWeek, CorridorName) %>%
  summarise(
    boardings = n(),
    max_fullness_decimal = ifelse(
      all(is.na(fullness_decimal)),
      NA_real_,
      max(fullness_decimal, na.rm = TRUE)
    ),
    max_onboard_count = ifelse(
      all(is.na(onboard_count)),
      NA_real_,
      max(onboard_count, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Compare Westside Inbound and Main Street Inbound

run_inbound_test <- function(data) {
  
  df <- data %>%
    filter(CorridorName %in% c("Westside Inbound", "Main Street Inbound"))
  
  shared_stops <- df %>%
    distinct(CorridorName, stop_base) %>%
    count(stop_base) %>%
    filter(n == 2) %>%
    pull(stop_base)
  
  corridor_compare <- df %>%
    filter(stop_base %in% shared_stops) %>%
    pivot_wider(
      names_from = CorridorName,
      values_from = c(
        boardings,
        max_fullness_decimal,
        max_onboard_count
      ),
      values_fill = list(boardings = 0)
    ) %>%
    mutate(
      westside = .data[["boardings_Westside Inbound"]],
      main_st = .data[["boardings_Main Street Inbound"]],
      
      westside_fullness_decimal =
        .data[["max_fullness_decimal_Westside Inbound"]],
      
      main_st_fullness_decimal =
        .data[["max_fullness_decimal_Main Street Inbound"]],
      
      westside_onboard_count =
        .data[["max_onboard_count_Westside Inbound"]],
      
      main_st_onboard_count =
        .data[["max_onboard_count_Main Street Inbound"]]
    ) %>%
    group_by(stop_base, Hour, DayOfWeek) %>%
    mutate(
      westside_p75 = quantile(westside, 0.75, na.rm = TRUE),
      westside_high = westside >= westside_p75 & westside > 0
    ) %>%
    ungroup()
  
  model_data <- corridor_compare %>%
    filter(
      !is.na(main_st),
      !is.na(westside),
      !is.na(westside_fullness_decimal),
      !is.na(main_st_fullness_decimal),
      !is.na(stop_base),
      !is.na(Hour),
      !is.na(DayOfWeek)
    ) %>%
    mutate(
      main_st = as.integer(main_st),
      westside_high = factor(westside_high),
      stop_base = factor(stop_base),
      DayOfWeek = factor(DayOfWeek)
    )
  
  # Question: Is Main Street more popular than Westside overall?
  
  print(
    model_data %>%
      summarise(
        total_westside = sum(westside, na.rm = TRUE),
        total_main_st = sum(main_st, na.rm = TRUE),
        mean_westside = mean(westside, na.rm = TRUE),
        mean_main_st = mean(main_st, na.rm = TRUE),
        median_westside = median(westside, na.rm = TRUE),
        median_main_st = median(main_st, na.rm = TRUE)
      )
  )
  
  
  print("################################")
  
  print(
    model_data %>%
      summarise(
        n_rows = n(),
        n_stop_base = n_distinct(stop_base),
        n_DayOfWeek = n_distinct(DayOfWeek),
        n_westside_high = n_distinct(westside_high),
        n_Hour = n_distinct(Hour),
        max_westside_fullness = max(westside_fullness_decimal, na.rm = TRUE),
        max_main_st_fullness = max(main_st_fullness_decimal, na.rm = TRUE),
        westside_over_capacity = sum(westside_fullness_decimal > 1, na.rm = TRUE),
        main_st_over_capacity = sum(main_st_fullness_decimal > 1, na.rm = TRUE)
      )
  )
  
  # NOTE:
  # I am NOT putting main_st_fullness_decimal in the model below because
  # main_st_fullness_decimal is directly derived from Main Street boardings,
  # which are also the outcome variable.
  #
  # Including it would be circular/leaky.
  
  nb_model <- glmmTMB(
    main_st ~ westside_fullness_decimal + stop_base + Hour + DayOfWeek,
    family = nbinom2,
    data = model_data
  )
  
  print("################################")
  
  print(
    model_data %>%
      group_by(westside_high) %>%
      summarise(
        median_main_st = median(main_st, na.rm = TRUE),
        mean_main_st = mean(main_st, na.rm = TRUE),
        
        median_westside_fullness = median(westside_fullness_decimal, na.rm = TRUE),
        mean_westside_fullness = mean(westside_fullness_decimal, na.rm = TRUE),
        
        median_main_st_fullness = median(main_st_fullness_decimal, na.rm = TRUE),
        mean_main_st_fullness = mean(main_st_fullness_decimal, na.rm = TRUE),
        
        median_westside_onboard = median(westside_onboard_count, na.rm = TRUE),
        mean_westside_onboard = mean(westside_onboard_count, na.rm = TRUE),
        
        median_main_st_onboard = median(main_st_onboard_count, na.rm = TRUE),
        mean_main_st_onboard = mean(main_st_onboard_count, na.rm = TRUE),
        
        n = n(),
        .groups = "drop"
      )
  )
  
  print(summary(nb_model))
  
  print("################################")
  
  print(exp(fixef(nb_model)$cond))
  
  fullness_plot <- model_data %>%
    filter(westside_fullness_decimal <= 1) %>%
    ggplot(aes(x = westside_fullness_decimal, y = main_st)) +
    geom_jitter(width = 0.01, height = 0.15, alpha = 0.25) +
    geom_smooth(method = "glm", se = TRUE, method.args = list(family = "poisson")) +
    labs(
      x = "Estimated Westside Inbound fullness",
      y = "Main Street Inbound boardings",
      title = "Main Street Inbound Boardings by Westside Inbound Fullness",
      caption = "Observations above 100% estimated fullness were excluded from the visualization."
    ) +
    scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = c(0, 1)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  print(fullness_plot)
  
  print("################################")
  
  return(list(
    data = model_data,
    model = nb_model,
    plot = fullness_plot,
    shared_stops = shared_stops
  ))
}

inbound_results <- run_inbound_test(model_bfullness_binned <- inbound_results$data) %>%
  filter(westside_fullness_decimal >= 0,
         westside_fullness_decimal <= 1) %>%
  mutate(
    fullness_bin = cut(
      westside_fullness_decimal,
      breaks = seq(0, 1, by = 0.10),
      include.lowest = TRUE,
      labels = c(
        "0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
        "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"
      )
    )
  ) %>%
  group_by(fullness_bin) %>%
  summarise(
    mean_main_st = mean(main_st, na.rm = TRUE),
    median_main_st = median(main_st, na.rm = TRUE),
    n = n(),
    .groups = "drop"
)

ggplot(fullness_binned, aes(x = fullness_bin, y = mean_main_st, fill = mean_main_st)) +
  geom_col() +
  geom_text(
    aes(label = paste0("n=", n)),
    vjust = -0.3,
    size = 3
  ) +
  scale_fill_gradient(
    low = "#B3E5FC", 
    high = "#1565C0"
  ) +
  labs(
    x = "Estimated Westside Inbound fullness",
    y = "Mean Main Street Inbound boardings per stop-hour observation",
    title = "Mean Main Street Boardings by Westside Fullness Bin",
    caption = "Observations above 100% estimated fullness were excluded from the visualization.",
    fill = "Mean boardings"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# Outbound

outbound_raw <- data_clean %>%
  filter(CorridorName %in% c("Westside Outbound", "Main Street Outbound")) %>%
  arrange(Date, datetime, VehicleID)

# Add fullness for BOTH outbound routes

add_outbound_fullness <- function(data,
                                  bus_capacity = 51,
                                  reset_stop_pattern = "University Downtown Center",
                                  gap_reset_minutes = 25) {
  
  data <- data %>%
    mutate(
      onboard_count = NA_real_,
      fullness_decimal = NA_real_
    ) %>%
    arrange(Date, VehicleID, CorridorName, datetime)
  
  # One counter per Date + CorridorName + VehicleID
  current_counts <- list()
  last_times <- list()
  
  for (i in seq_len(nrow(data))) {
    
    this_route <- data$CorridorName[i]
    
    if (this_route %in% c("Westside Outbound", "Main Street Outbound")) {
      
      this_key <- paste(
        data$Date[i],
        this_route,
        data$VehicleID[i],
        sep = "_"
      )
      
      # Start new counter if this date/route/vehicle has not appeared yet
      if (is.null(current_counts[[this_key]])) {
        current_counts[[this_key]] <- 0
        last_times[[this_key]] <- data$datetime[i]
      }
      
      # Reset if same vehicle/route has a large time gap
      time_gap <- as.numeric(
        difftime(data$datetime[i], last_times[[this_key]], units = "mins")
      )
      
      if (!is.na(time_gap) && time_gap > gap_reset_minutes) {
        current_counts[[this_key]] <- 0
      }
      
      # Each raw row represents one boarding
      current_counts[[this_key]] <- current_counts[[this_key]] + 1
      
      # Store onboard count and decimal fullness
      data$onboard_count[i] <- current_counts[[this_key]]
      data$fullness_decimal[i] <- current_counts[[this_key]] / bus_capacity
      
      # Update last time for this route/vehicle/day
      last_times[[this_key]] <- data$datetime[i]
      
      # Reset after the outbound route reaches its endpoint
      if (str_detect(data$stop_base[i], reset_stop_pattern)) {
        current_counts[[this_key]] <- 0
      }
    }
  }
  
  data <- data %>%
    arrange(Date, datetime, VehicleID) %>%
    select(
      any_of(c(
        "Date",
        "Time",
        "datetime",
        "Year",
        "Season",
        "DayOfWeek",
        "Hour",
        "Minute",
        "VehicleID",
        "CorridorName",
        "StopName",
        "stop_base",
        "onboard_count",
        "fullness_decimal"
      ))
    )
  
  return(data)
}

outbound_with_fullness <- add_outbound_fullness(
  data = outbound_raw,
  bus_capacity = 51,
  reset_stop_pattern = "University Downtown Center",
  gap_reset_minutes = 25
)

# Check fullness for both outbound routes

print(
  outbound_with_fullness %>%
    group_by(CorridorName) %>%
    summarise(
      n = n(),
      missing_fullness = sum(is.na(fullness_decimal)),
      min_fullness = min(fullness_decimal, na.rm = TRUE),
      median_fullness = median(fullness_decimal, na.rm = TRUE),
      mean_fullness = mean(fullness_decimal, na.rm = TRUE),
      max_fullness = max(fullness_decimal, na.rm = TRUE),
      over_capacity = sum(fullness_decimal > 1, na.rm = TRUE),
      .groups = "drop"
    )
)

# Aggregate to model level

model_base_outbound <- outbound_with_fullness %>%
  group_by(stop_base, Date, Hour, DayOfWeek, CorridorName) %>%
  summarise(
    boardings = n(),
    max_fullness_decimal = ifelse(
      all(is.na(fullness_decimal)),
      NA_real_,
      max(fullness_decimal, na.rm = TRUE)
    ),
    max_onboard_count = ifelse(
      all(is.na(onboard_count)),
      NA_real_,
      max(onboard_count, na.rm = TRUE)
    ),
    .groups = "drop"
  )

# Compare Westside Outbound and Main Street Outbound

run_outbound_test <- function(data) {
  
  df <- data %>%
    filter(CorridorName %in% c("Westside Outbound", "Main Street Outbound"))
  
  shared_stops <- df %>%
    distinct(CorridorName, stop_base) %>%
    count(stop_base) %>%
    filter(n == 2) %>%
    pull(stop_base)
  
  corridor_compare <- df %>%
    filter(stop_base %in% shared_stops) %>%
    pivot_wider(
      names_from = CorridorName,
      values_from = c(
        boardings,
        max_fullness_decimal,
        max_onboard_count
      ),
      values_fill = list(boardings = 0)
    ) %>%
    mutate(
      westside = .data[["boardings_Westside Outbound"]],
      main_st = .data[["boardings_Main Street Outbound"]],
      
      westside_fullness_decimal =
        .data[["max_fullness_decimal_Westside Outbound"]],
      
      main_st_fullness_decimal =
        .data[["max_fullness_decimal_Main Street Outbound"]],
      
      westside_onboard_count =
        .data[["max_onboard_count_Westside Outbound"]],
      
      main_st_onboard_count =
        .data[["max_onboard_count_Main Street Outbound"]]
    ) %>%
    group_by(stop_base, Hour, DayOfWeek) %>%
    mutate(
      westside_p75 = quantile(westside, 0.75, na.rm = TRUE),
      westside_high = westside >= westside_p75 & westside > 0
    ) %>%
    ungroup()
  
  model_data <- corridor_compare %>%
    filter(
      !is.na(main_st),
      !is.na(westside),
      !is.na(westside_fullness_decimal),
      !is.na(main_st_fullness_decimal),
      !is.na(stop_base),
      !is.na(Hour),
      !is.na(DayOfWeek)
    ) %>%
    mutate(
      main_st = as.integer(main_st),
      westside = as.integer(westside),
      westside_high = factor(westside_high),
      stop_base = factor(stop_base),
      DayOfWeek = factor(DayOfWeek),
      
      # Cap impossible fullness values at 1
      westside_fullness_decimal = pmin(westside_fullness_decimal, 1),
      main_st_fullness_decimal = pmin(main_st_fullness_decimal, 1)
    )
  
  # Data diagnostic summary
  
  print(
    model_data %>%
      summarise(
        n_rows = n(),
        n_stop_base = n_distinct(stop_base),
        n_DayOfWeek = n_distinct(DayOfWeek),
        n_westside_high = n_distinct(westside_high),
        n_Hour = n_distinct(Hour),
        max_westside_fullness = max(westside_fullness_decimal, na.rm = TRUE),
        max_main_st_fullness = max(main_st_fullness_decimal, na.rm = TRUE),
        westside_over_capacity = sum(westside_fullness_decimal > 1, na.rm = TRUE),
        main_st_over_capacity = sum(main_st_fullness_decimal > 1, na.rm = TRUE)
      )
  )
  
  # Descriptive comparison: Westside vs Main Street
  
  print(
    model_data %>%
      summarise(
        total_westside = sum(westside, na.rm = TRUE),
        total_main_st = sum(main_st, na.rm = TRUE),
        mean_westside = mean(westside, na.rm = TRUE),
        mean_main_st = mean(main_st, na.rm = TRUE),
        median_westside = median(westside, na.rm = TRUE),
        median_main_st = median(main_st, na.rm = TRUE)
      )
  )
  
  # Compare possible models
  
  model_high <- glmmTMB(
    main_st ~ westside_high + stop_base + Hour + DayOfWeek,
    family = nbinom2,
    data = model_data
  )
  
  model_fullness <- glmmTMB(
    main_st ~ westside_fullness_decimal + stop_base + Hour + DayOfWeek,
    family = nbinom2,
    data = model_data
  )
  
  model_both <- glmmTMB(
    main_st ~ westside_high + westside_fullness_decimal + stop_base + Hour + DayOfWeek,
    family = nbinom2,
    data = model_data
  )
  
  print(AIC(model_high, model_fullness, model_both))
  
  # Use fullness-only model as main model for consistency
  nb_model <- model_fullness
  
  # If you decide to use the AIC-best model for outbound instead, use:
  # nb_model <- model_both
  
  # Summary table by westside_high
  
  print(
    model_data %>%
      group_by(westside_high) %>%
      summarise(
        median_main_st = median(main_st, na.rm = TRUE),
        mean_main_st = mean(main_st, na.rm = TRUE),
        
        median_westside_fullness = median(westside_fullness_decimal, na.rm = TRUE),
        mean_westside_fullness = mean(westside_fullness_decimal, na.rm = TRUE),
        
        median_main_st_fullness = median(main_st_fullness_decimal, na.rm = TRUE),
        mean_main_st_fullness = mean(main_st_fullness_decimal, na.rm = TRUE),
        
        median_westside_onboard = median(westside_onboard_count, na.rm = TRUE),
        mean_westside_onboard = mean(westside_onboard_count, na.rm = TRUE),
        
        median_main_st_onboard = median(main_st_onboard_count, na.rm = TRUE),
        mean_main_st_onboard = mean(main_st_onboard_count, na.rm = TRUE),
        
        n = n(),
        .groups = "drop"
      )
  )
  
  print(summary(nb_model))
  print(exp(fixef(nb_model)$cond))

  # Plot
  
  fullness_plot <- ggplot(model_data, aes(x = westside_fullness_decimal, y = main_st)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      x = "Estimated Westside Outbound fullness, decimal",
      y = "Main Street Outbound boardings",
      title = "Main Street Outbound ridership by estimated Westside Outbound fullness"
    )
  
  print(fullness_plot)
  
  return(list(
    data = model_data,
    model_high = model_high,
    model_fullness = model_fullness,
    model_both = model_both,
    model = nb_model,
    plot = fullness_plot,
    shared_stops = shared_stops
  ))
}


# Run outbound test


outbound_results <- run_outbound_test(model_base_outbound)

fullness_binned <- outbound_results$data %>%
  filter(westside_fullness_decimal >= 0,
         westside_fullness_decimal <= 1) %>%
  mutate(
    fullness_bin = cut(
      westside_fullness_decimal,
      breaks = seq(0, 1, by = 0.10),
      include.lowest = TRUE,
      labels = c(
        "0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
        "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"
      )
    )
  ) %>%
  group_by(fullness_bin) %>%
  summarise(
    mean_main_st = mean(main_st, na.rm = TRUE),
    median_main_st = median(main_st, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

ggplot(fullness_binned, aes(x = fullness_bin, y = mean_main_st, fill = mean_main_st)) +
  geom_col() +
  geom_text(
    aes(label = paste0("n=", n)),
    vjust = -0.3,
    size = 3
  ) +
  scale_fill_gradient(
    low = "#B3E5FC", 
    high = "#1565C0"
  ) +
  labs(
    x = "Estimated Westside Outbound fullness",
    y = "Mean Main Street Outbound boardings per stop-hour observation",
    title = "Mean Main Street Boardings by Westside Fullness Bin",
    fill = "Mean boardings"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

