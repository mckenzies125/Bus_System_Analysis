library(dplyr)
library(stringr)
library(tidygeocoder)

# 0) READ DATA + CLEAN CORRIDOR NAMES

data <- read.csv(
  "cleaned_up_occt_data.csv",
  stringsAsFactors = FALSE
)

# 1) KEEP THE COLUMNS YOU NEED

keep_cols <- c(
  "PERSON_UID",
  "CorridorName",
  "StopName",
  "Date",
  "Time"
)

data_drops <- data[, intersect(keep_cols, names(data))]

# 2) PARSE TIME + DATE + WEEKDAY

data_drops <- data_drops %>%
  mutate(
    Hour   = as.integer(substr(Time, 1, 2)),
    Minute = as.integer(substr(Time, 4, 5)),
    
    Date   = as.Date(as.character(Date), format = "%Y-%m-%d"),
    Year   = as.integer(format(Date, "%Y")),
    Month  = as.integer(format(Date, "%m")),
    
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
    )
  )

# 3) UNICODE CLEANUP

decode_unicode <- function(x) {
  x <- as.character(x)
  
  str_replace_all(
    x,
    "_x[0-9A-Fa-f]{4}_",
    function(m) {
      vapply(
        m,
        function(one) {
          hex <- str_sub(one, 3, 6)
          intToUtf8(strtoi(hex, base = 16))
        },
        character(1)
      )
    }
  )
}

clean_corridor <- function(x) {
  x_raw <- str_squish(as.character(x))
  x_norm <- toupper(x_raw)
  
  case_when(
    x_norm %in% c("WESTSIDE (IB)", "WESTSIDE INBOUND") ~ "Westside Inbound",
    x_norm %in% c("WESTSIDE (OB)", "WESTSIDE OUTBOUND") ~ "Westside Outbound",
    
    x_norm %in% c("DOWNTOWN CENTER LEROY (IB)", "DOWNTOWN CENTER LEROY INBOUND") ~ "Downtown Center Leroy Inbound",
    x_norm %in% c("DOWNTOWN CENTER LEROY (OB)", "DOWNTOWN CENTER LEROY OUTBOUND") ~ "Downtown Center Leroy Outbound",
    
    x_norm == "ITC - UCLUB" ~ "ITC - UCLUB",
    
    x_norm %in% c("INNOVATIVE TECHNOLOGY CENTER", "INNOVATIVE TECHNOLOGY COMPLEX") ~ "Innovative Technology Center",
    
    x_norm %in% c("RES - ITC - UCLUB", "RESIDENTIAL SHUTTLE - ITC - UCLUB") ~ "Innovative Technology Center",
    
    TRUE ~ x_raw
  )
}

data_drops_unicode <- data_drops %>%
  mutate(across(
    where(is.character),
    ~ {
      y <- decode_unicode(.x)
      y <- str_replace_all(y, "_+", " ")
      str_squish(y)
    }
  )) %>%
  filter(!is.na(StopName), StopName != "Unknown") %>%
  mutate(
    Corridor_clean = clean_corridor(CorridorName)
  )

# 4) BUILD stop_base + GEOCODE

make_stop_base <- function(x) {
  x %>%
    as.character() %>%
    str_remove("\\s*\\((IB|OB|LN|Late\\s*Nite|CS)\\)\\s*$") %>%
    str_squish()
}

make_query <- function(stop_base) {
  q <- ifelse(
    str_detect(stop_base, "&"),
    str_replace_all(stop_base, "\\s*&\\s*", " and "),
    stop_base
  )
  
  campus_keywords <- c(
    "University Union",
    "UCLUB",
    "ITC",
    "Innovative Technology Center",
    "East Gym",
    "West Gym",
    "Engineering",
    "Dickinson",
    "Newing",
    "Hinman",
    "Mountainview",
    "Susquehanna",
    "Hillside",
    "Hayes",
    "Couper",
    "Academic",
    "Lot"
  )
  
  is_campus <- str_detect(q, str_c(campus_keywords, collapse = "|"))
  
  ifelse(
    is_campus,
    paste0(q, ", Binghamton University, Vestal, NY"),
    paste0(q, ", Binghamton, NY")
  )
}

stops_to_geocode <- data_drops_unicode %>%
  mutate(
    stop_base = make_stop_base(StopName),
    address = make_query(stop_base)
  ) %>%
  distinct(stop_base, address)

g_osm <- stops_to_geocode %>%
  geocode(
    address = address,
    method = "osm",
    lat = lat1,
    long = lon1
  )

g_census <- stops_to_geocode %>%
  geocode(
    address = address,
    method = "census",
    lat = lat2,
    long = lon2
  )

g_arcgis <- stops_to_geocode %>%
  geocode(
    address = address,
    method = "arcgis",
    lat = lat3,
    long = lon3
  )

# Helper function to safely get first non-missing coordinate
first_nonmissing <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  } else {
    return(first(x[!is.na(x)]))
  }
}

g_osm_unique <- g_osm %>%
  group_by(stop_base) %>%
  summarise(
    lat1 = first_nonmissing(lat1),
    lon1 = first_nonmissing(lon1),
    .groups = "drop"
  )

g_census_unique <- g_census %>%
  group_by(stop_base) %>%
  summarise(
    lat2 = first_nonmissing(lat2),
    lon2 = first_nonmissing(lon2),
    .groups = "drop"
  )

g_arcgis_unique <- g_arcgis %>%
  group_by(stop_base) %>%
  summarise(
    lat3 = first_nonmissing(lat3),
    lon3 = first_nonmissing(lon3),
    .groups = "drop"
  )

stops_geo <- g_osm_unique %>%
  full_join(g_census_unique, by = "stop_base") %>%
  full_join(g_arcgis_unique, by = "stop_base") %>%
  transmute(
    stop_base,
    lat = coalesce(lat1, lat2, lat3),
    lon = coalesce(lon1, lon2, lon3)
  )

# 5) APPLY MANUAL LAT/LON FIXES

stop_to_change <- "Mohawk"
new_lat <- 42.08676046211194
new_lon <- -75.96601751111898

manual_path <- "stops_to_fix_manual_coords.csv"

if (file.exists(manual_path)) {
  manual_coords <- read.csv(manual_path, stringsAsFactors = FALSE)
} else {
  manual_coords <- data.frame(
    stop_base = character(),
    lat_manual = numeric(),
    lon_manual = numeric(),
    stringsAsFactors = FALSE
  )
}

if (stop_to_change %in% manual_coords$stop_base) {
  manual_coords$lat_manual[manual_coords$stop_base == stop_to_change] <- new_lat
  manual_coords$lon_manual[manual_coords$stop_base == stop_to_change] <- new_lon
} else {
  manual_coords <- rbind(
    manual_coords,
    data.frame(
      stop_base = stop_to_change,
      lat_manual = new_lat,
      lon_manual = new_lon,
      stringsAsFactors = FALSE
    )
  )
}

write.csv(manual_coords, manual_path, row.names = FALSE)

stops_geo_fixed <- stops_geo %>%
  left_join(manual_coords, by = "stop_base") %>%
  mutate(
    lat = ifelse(!is.na(lat_manual), lat_manual, lat),
    lon = ifelse(!is.na(lon_manual), lon_manual, lon)
  ) %>%
  select(stop_base, lat, lon)

stops_geo_fixed_unique <- stops_geo_fixed %>%
  group_by(stop_base) %>%
  summarise(
    lat = first(lat),
    lon = first(lon),
    .groups = "drop"
  )

# 6) BUILD FINAL EVENT-LEVEL DATASET

data_for_tableau <- data_drops_unicode %>%
  mutate(
    stop_base = make_stop_base(StopName)
  ) %>%
  left_join(stops_geo_fixed_unique, by = "stop_base") %>%
  mutate(
    tod_min = 60L * as.integer(Hour) + as.integer(Minute)
  )

# Optional check for rows missing coordinates
data_for_tableau %>%
  filter(is.na(lat) | is.na(lon)) %>%
  count(StopName, stop_base, sort = TRUE)

# 7) BOARDINGS: DAILY COUNTS FOR TABLEAU

daily_counts <- data_for_tableau %>%
  filter(!is.na(lat), !is.na(lon), !is.na(Corridor_clean)) %>%
  group_by(
    CorridorName = Corridor_clean,
    StopName,
    stop_base,
    lat,
    lon,
    Year,
    Season,
    DayOfWeek,
    Date,
    Hour
  ) %>%
  summarise(
    daily_count = n(),
    .groups = "drop"
  )

# Check final exported corridor names
daily_counts %>%
  count(CorridorName, sort = TRUE)

write.csv(
  daily_counts,
  "enter_reg.csv",
  row.names = FALSE
)

# 8) EXITS: INFERRED DAILY COUNTS FOR TABLEAU

rides_with_exit <- data_for_tableau %>%
  filter(
    !is.na(PERSON_UID),
    !is.na(Corridor_clean),
    !is.na(StopName),
    !is.na(Date)
  ) %>%
  arrange(PERSON_UID, Date, tod_min) %>%
  group_by(PERSON_UID, Date) %>%
  mutate(
    next_route = lead(Corridor_clean),
    next_stop  = lead(StopName),
    exit_stop  = if_else(Corridor_clean == next_route, next_stop, NA_character_),
    exit_hour  = lead(Hour),
    exit_route = if_else(Corridor_clean == next_route, Corridor_clean, NA_character_)
  ) %>%
  ungroup()

exits <- rides_with_exit %>%
  filter(
    !is.na(exit_stop),
    !is.na(exit_hour),
    !is.na(exit_route)
  ) %>%
  mutate(
    exit_hour = as.integer(exit_hour),
    exit_stop_base = make_stop_base(exit_stop)
  ) %>%
  left_join(
    stops_geo_fixed_unique,
    by = c("exit_stop_base" = "stop_base"),
    suffix = c(".board", ".exit")
  ) %>%
  mutate(
    lat = coalesce(lat.exit, lat.board),
    lon = coalesce(lon.exit, lon.board)
  ) %>%
  select(
    -lat.board,
    -lon.board,
    -lat.exit,
    -lon.exit
  )

exit_daily_counts <- exits %>%
  group_by(
    CorridorName = exit_route,
    StopName = exit_stop,
    stop_base = exit_stop_base,
    lat,
    lon,
    Year,
    Season,
    DayOfWeek,
    Date,
    Hour = exit_hour
  ) %>%
  summarise(
    daily_count = n(),
    .groups = "drop"
  )

# Check final exported corridor names
exit_daily_counts %>%
  count(CorridorName, sort = TRUE)

write.csv(
  exit_daily_counts,
  "exit_reg.csv",
  row.names = FALSE
)

# 9) DROP UNION EXPORTS

daily_counts_no_union <- daily_counts %>%
  filter(stop_base != "University Union")

write.csv(
  daily_counts_no_union,
  "enter_no_union.csv",
  row.names = FALSE
)

exit_daily_counts_no_union <- exit_daily_counts %>%
  filter(stop_base != "University Union")

write.csv(
  exit_daily_counts_no_union,
  "exit_no_union.csv",
  row.names = FALSE
)

# 10) OPTIONAL: EXPORT MAP DATA WITHOUT OUTBOUND ROUTES

# Use these if the outbound routes still create no bubbles
# but shrink the visible inbound bubbles in Tableau.

daily_counts_no_outbound <- daily_counts %>%
  filter(!str_detect(CorridorName, "Outbound|\\(OB\\)"))

write.csv(
  daily_counts_no_outbound,
  "enter_no_outbound.csv",
  row.names = FALSE
)

exit_daily_counts_no_outbound <- exit_daily_counts %>%
  filter(!str_detect(CorridorName, "Outbound|\\(OB\\)"))

write.csv(
  exit_daily_counts_no_outbound,
  "exit_no_outbound.csv",
  row.names = FALSE
)