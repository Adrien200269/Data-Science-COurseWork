=

library(tidyverse)
library(lubridate)

base_path <- "./CourseWork_Ds_sabjaShrestha"
crime_folder <- file.path(base_path, "Obtained Data/CrimeRate")



load_all_crime_data <- function(crime_folder) {
  # Get all month folders
  month_folders <- list.dirs(crime_folder, recursive = FALSE, full.names = TRUE)
  
  all_crime_list <- list()
  
  for (folder in month_folders) {
    csv_files <- list.files(folder, pattern = "\\.csv$", full.names = TRUE)
    
    for (csv_file in csv_files) {
      tryCatch({
        crime_df <- read_csv(csv_file, show_col_types = FALSE)
        
        if (grepl("cheshire", basename(csv_file), ignore.case = TRUE)) {
          crime_df$source_region <- "Cheshire"
        } else if (grepl("cumbria", basename(csv_file), ignore.case = TRUE)) {
          crime_df$source_region <- "Cumberland"
        } else {
          crime_df$source_region <- "Unknown"
        }
        
        crime_df$source_file <- basename(csv_file)
        
        all_crime_list[[length(all_crime_list) + 1]] <- crime_df
        
      }, error = function(e) {
        message("Error reading file: ", csv_file, " - ", e$message)
      })
    }
  }
  
  combined <- bind_rows(all_crime_list)
  return(combined)
}



cat("Loading crime data from all folders...\n")
crime_data_raw <- load_all_crime_data(crime_folder)

cat("Total records loaded:", nrow(crime_data_raw), "\n")

crime_data_clean <- crime_data_raw %>%
  # Parse date components
  mutate(
    year = as.integer(str_sub(Month, 1, 4)),
    month_num = as.integer(str_sub(Month, 6, 7)),
    date = ym(Month)
  ) %>%
  rename(region = source_region) %>%
  mutate(
    lsoa_district = str_extract(`LSOA name`, "^[^\\s]+"),
    lsoa_area_code = str_extract(`LSOA name`, "[0-9]+[A-Z]$")
  ) %>%
  filter(
    !is.na(`Crime type`),
    !is.na(region),
    region != "Unknown"
  ) %>%
  select(
    crime_id = `Crime ID`,
    month = Month,
    year,
    month_num,
    date,
    region,
    lsoa_code = `LSOA code`,
    lsoa_name = `LSOA name`,
    lsoa_district,
    crime_type = `Crime type`,
    longitude = Longitude,
    latitude = Latitude,
    location = Location,
    outcome = `Last outcome category`
  )

crime_summary_by_type <- crime_data_clean %>%
  group_by(region, crime_type) %>%
  summarise(
    total_incidents = n(),
    .groups = "drop"
  ) %>%
  arrange(region, desc(total_incidents))

crime_summary_by_year <- crime_data_clean %>%
  group_by(region, year) %>%
  summarise(
    total_incidents = n(),
    unique_locations = n_distinct(lsoa_code),
    .groups = "drop"
  )

crime_summary_by_month <- crime_data_clean %>%
  group_by(region, date, year, month_num) %>%
  summarise(
    total_incidents = n(),
    .groups = "drop"
  ) %>%
  arrange(date)

drug_offense_summary <- crime_data_clean %>%
  filter(crime_type == "Drugs") %>%
  group_by(region, year, month_num, lsoa_district) %>%
  summarise(
    drug_incidents = n(),
    .groups = "drop"
  )

vehicle_crime_summary <- crime_data_clean %>%
  filter(crime_type == "Vehicle crime") %>%
  group_by(region, year, month_num) %>%
  summarise(
    vehicle_crime_incidents = n(),
    .groups = "drop"
  )

robbery_summary <- crime_data_clean %>%
  filter(crime_type == "Robbery") %>%
  group_by(region, year, month_num) %>%
  summarise(
    robbery_incidents = n(),
    .groups = "drop"
  )

cleaned_dir <- file.path(base_path, "CleanedData")
if (!dir.exists(cleaned_dir)) {
  dir.create(cleaned_dir, recursive = TRUE)
}

write_csv(crime_data_clean, file.path(cleaned_dir, "crime_data_cleaned.csv"))
write_csv(crime_summary_by_type, file.path(cleaned_dir, "crime_summary_by_type.csv"))
write_csv(crime_summary_by_year, file.path(cleaned_dir, "crime_summary_by_year.csv"))
write_csv(crime_summary_by_month, file.path(cleaned_dir, "crime_summary_by_month.csv"))
write_csv(drug_offense_summary, file.path(cleaned_dir, "drug_offense_summary.csv"))
write_csv(vehicle_crime_summary, file.path(cleaned_dir, "vehicle_crime_summary.csv"))
write_csv(robbery_summary, file.path(cleaned_dir, "robbery_summary.csv"))


cat("\n========================================\n")
cat("CRIME DATA CLEANING COMPLETE\n")
cat("========================================\n\n")

cat("Total cleaned records:", nrow(crime_data_clean), "\n\n")

cat("Records by Region:\n")
print(table(crime_data_clean$region))

cat("\nRecords by Year:\n")
print(table(crime_data_clean$year))

cat("\nTop Crime Types:\n")
print(head(crime_summary_by_type %>% arrange(desc(total_incidents)), 15))

cat("\nDate Range:", min(crime_data_clean$date), "to", max(crime_data_clean$date), "\n")

cat("\nFiles saved to:", cleaned_dir, "\n")
cat("  - crime_data_cleaned.csv\n")
cat("  - crime_summary_by_type.csv\n")
cat("  - crime_summary_by_year.csv\n")
cat("  - crime_summary_by_month.csv\n")
cat("  - drug_offense_summary.csv\n")
cat("  - vehicle_crime_summary.csv\n")
cat("  - robbery_summary.csv\n")