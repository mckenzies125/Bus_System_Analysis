library(dplyr)
library(stringr)
library(tidygeocoder)

# 0) READ DATA + CLEAN CORRIDOR NAMES

data <- read.csv(
  "cleaned_up_occt_data.csv",
  stringsAsFactors = FALSE
)

# 2) PARSE TIME + DATE + WEEKDAY

data_drops <- data %>%
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

write.csv(
  data_drops_unicode,
  "Cleaned_up_og_data.csv",
  row.names = FALSE
)
