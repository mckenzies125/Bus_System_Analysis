library(dplyr)

data_for_tableau <- read.csv("data_with_long_lat.csv",
                 stringsAsFactors = FALSE)

source("Code_for_GeoGraph_Vis.r")


corridor_data <- data_for_tableau %>%
  filter(CorridorName == "Campus Shuttle")

unique(corridor_data$StopName)

# 1. Haversine distance function

haversine_m <- function(lat1, lon1, lat2, lon2) {
  R <- 6371000  # Earth radius in meters
  
  to_rad <- pi / 180
  
  lat1 <- lat1 * to_rad
  lon1 <- lon1 * to_rad
  lat2 <- lat2 * to_rad
  lon2 <- lon2 * to_rad
  
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  
  R * c
}

# 2. Keep only rows where we have enough info for matching

rides_with_exit <- corridor_data %>%
  filter(
    !is.na(PERSON_UID),
    !is.na(CorridorName),
    !is.na(StopName),
    !is.na(Date)
  ) %>%
  arrange(PERSON_UID, Date, tod_min) %>%
  group_by(PERSON_UID, Date) %>%
  mutate(
    next_route = lead(CorridorName),
    next_stop  = lead(StopName),
    exit_stop  = if_else(CorridorName == next_route, next_stop, NA_character_),
    exit_hour  = lead(Hour),
    exit_route = if_else(CorridorName == next_route, CorridorName, NA_character_)
  ) %>%
  ungroup()

exits <- rides_with_exit %>%
  filter(!is.na(exit_stop), !is.na(exit_hour), !is.na(exit_route)) %>%
  mutate(
    exit_hour = as.integer(exit_hour),
    exit_stop_base = make_stop_base(exit_stop)
  ) %>%
  left_join(
    stops_geo_fixed_unique %>%
      rename(
        exit_lat = lat,
        exit_lon = lon
      ),
    by = c("exit_stop_base" = "stop_base")
  ) %>%
  rename(
    board_lat = lat,
    board_lon = lon
  )

trips <- exits %>%
  filter(
    !is.na(PERSON_UID),
    !is.na(Date),
    !is.na(StopName),
    !is.na(exit_stop),
    !is.na(tod_min),
    !is.na(board_lat), !is.na(board_lon),
    !is.na(exit_lat), !is.na(exit_lon)
  ) %>%
  arrange(PERSON_UID, Date, tod_min) %>%
  group_by(PERSON_UID, Date) %>%
  mutate(trip_id = row_number()) %>%
  ungroup()

# 3. Self-join trips within the same rider/day to compare trip pairs
#    trip 1 must happen before trip 2

trip_pairs <- trips %>%
  select(
    PERSON_UID, Date, trip_id, tod_min,
    origin = StopName,
    dest = exit_stop,
    origin_lat = board_lat,
    origin_lon = board_lon,
    dest_lat = exit_lat,
    dest_lon = exit_lon
  ) %>%
  inner_join(
    trips %>%
      select(
        PERSON_UID, Date, trip_id, tod_min,
        origin = StopName,
        dest = exit_stop,
        origin_lat = board_lat,
        origin_lon = board_lon,
        dest_lat = exit_lat,
        dest_lon = exit_lon
      ),
    by = c("PERSON_UID", "Date"),
    suffix = c("_1", "_2"),
    relationship = "many-to-many"
  ) %>%
  filter(trip_id_1 < trip_id_2)

# 4. Measure whether trip 2 looks like the reverse of trip 1:
#    origin_2 near dest_1, and dest_2 near origin_1

reverse_candidates <- trip_pairs %>%
  mutate(
    dist_origin2_to_dest1 = haversine_m(
      origin_lat_2, origin_lon_2,
      dest_lat_1, dest_lon_1
    ),
    dist_dest2_to_origin1 = haversine_m(
      dest_lat_2, dest_lon_2,
      origin_lat_1, origin_lon_1
    ),
    direct_AB_distance = haversine_m(
      origin_lat_1, origin_lon_1,
      dest_lat_1, dest_lon_1
    ),
    time_gap = tod_min_2 - tod_min_1
  )

# 5. Pick a threshold for "nearby"
#    Example: 200 meters

distance_threshold <- 200

reverse_trips <- reverse_candidates %>%
  filter(
    dist_origin2_to_dest1 <= distance_threshold,
    dist_dest2_to_origin1 <= distance_threshold
  )

# 6. View the matched reverse-trip pairs

reverse_trip_results <- reverse_trips %>%
  select(
    PERSON_UID, Date,
    trip_id_1, time_1 = tod_min_1, origin_1, dest_1,
    trip_id_2, time_2 = tod_min_2, origin_2, dest_2,
    dist_origin2_to_dest1,
    dist_dest2_to_origin1,
    direct_AB_distance,
    time_gap
  ) %>%
  arrange(PERSON_UID, Date, time_1)

reverse_trip_results


# Q: How many reverse-trip matches did you find?
nrow(reverse_trip_results)
# 18794

# Q: For how many rider + date combinations did at least one reverse trip occur?
reverse_trip_results %>%
  distinct(PERSON_UID, Date) %>%
  nrow()
# 11897

# approx vs exact matches for reverse trips
reverse_trip_results %>%
  mutate(
    reverse_type = case_when(
      dist_origin2_to_dest1 < 1 & dist_dest2_to_origin1 < 1 ~ "Exact reverse",
      TRUE ~ "Approximate reverse"
    )
  ) %>%
  count(reverse_type)
#  reverse_type            n
# <chr>               <int>
# 1 Approximate reverse   323
# 2 Exact reverse       18471

#summary of distances
reverse_trip_results %>%
  summarise(
    n = n(),
    mean_AB_m = mean(direct_AB_distance, na.rm = TRUE),
    median_AB_m = median(direct_AB_distance, na.rm = TRUE),
    min_AB_m = min(direct_AB_distance, na.rm = TRUE),
    max_AB_m = max(direct_AB_distance, na.rm = TRUE)
  )
# A tibble: 1 × 5
# n mean_AB_m median_AB_m min_AB_m max_AB_m
# <int>     <dbl>       <dbl>    <dbl>    <dbl>
# 18794      379.        313.        0    6495.


#How far a part were the stops (in bins)
reverse_trip_results %>%
  mutate(
    AB_distance_band = case_when(
      direct_AB_distance <= 100 ~ "0-100 m",
      direct_AB_distance <= 200 ~ "101-200 m",
      direct_AB_distance <= 400 ~ "201-400 m",
      direct_AB_distance <= 800 ~ "401-800 m",
      TRUE ~ "800+ m"
    )
  ) %>%
  count(AB_distance_band, sort = TRUE)

# A tibble: 5 × 2
#AB_distance_band     n
#<chr>            <int>
#1 201-400 m        11508
#2 0-100 m           4959
#3 401-800 m         1504
#4 800+ m             603
#5 101-200 m          220


#most common pairs
reverse_trip_results %>%
  count(origin_1, dest_1, origin_2, dest_2, sort = TRUE)

#grand summary
stop_pair_summary <- reverse_trip_results %>%
  group_by(origin_1, dest_1) %>%
  summarise(
    n_reverse_matches = n(),
    n_unique_riders = n_distinct(PERSON_UID),
    n_unique_rider_days = n_distinct(paste(PERSON_UID, Date)),
    mean_AB_distance_m = mean(direct_AB_distance, na.rm = TRUE),
    median_AB_distance_m = median(direct_AB_distance, na.rm = TRUE),
    mean_time_gap_mins = mean(time_gap, na.rm = TRUE),
    median_time_gap_mins = median(time_gap, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_reverse_matches))

stop_pair_summary




# ONLY KEEP STOP PAIRS THAT APPEAR MORE THAN FOUR TIMES
repeated_pairs_only <- reverse_trip_results %>%
  group_by(origin_1, dest_1) %>%
  filter(n() >= 5) %>%
  ungroup()


# Q: How many reverse-trip matches did you find?
nrow(repeated_pairs_only)
# 18728


repeated_pairs_only %>%
  distinct(PERSON_UID, Date) %>%
  nrow()


repeated_pairs_only %>%
  mutate(
    reverse_type = case_when(
      dist_origin2_to_dest1 < 1 & dist_dest2_to_origin1 < 1 ~ "Exact reverse",
      TRUE ~ "Approximate reverse"
    )
  ) %>%
  count(reverse_type)


#summary of distances
repeated_pairs_only %>%
  summarise(
    n = n(),
    mean_AB_m = mean(direct_AB_distance, na.rm = TRUE),
    median_AB_m = median(direct_AB_distance, na.rm = TRUE),
    min_AB_m = min(direct_AB_distance, na.rm = TRUE),
    max_AB_m = max(direct_AB_distance, na.rm = TRUE)
  )



#How far a part were the stops (in bins)
repeated_pairs_only %>%
  mutate(
    AB_distance_band = case_when(
      direct_AB_distance <= 100 ~ "0-100 m",
      direct_AB_distance <= 200 ~ "101-200 m",
      direct_AB_distance <= 400 ~ "201-400 m",
      direct_AB_distance <= 800 ~ "401-800 m",
      TRUE ~ "800+ m"
    )
  ) %>%
  count(AB_distance_band, sort = TRUE)


