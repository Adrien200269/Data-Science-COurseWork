
library(tidyverse)

base_path <- "./CourseWork_Ds_sabjaShrestha"

coverage_file <- file.path(base_path, "Obtained Data/Internet Speed/201809_fixed_pc_coverage_r01.csv")


broadband_coverage <- read_csv(coverage_file, show_col_types = FALSE)


print(colnames(broadband_coverage))


broadband_coverage_filtered <- broadband_coverage %>%
  mutate(
    postcode_area = str_extract(postcode, "^[A-Z]+")
  ) %>%
  filter(postcode_area %in% c("CH", "CW", "CA", "WA", "SK"))

coverage_clean <- broadband_coverage_filtered %>%
  select(
    postcode,
    postcode_area,
    contains("SFBB"),
    contains("UFBB"),
    contains("superfast"),
    contains("ultrafast"),
    contains("availability")
  )

coverage_clean <- coverage_clean %>%
  mutate(
    region = case_when(
      postcode_area %in% c("CH", "CW", "WA", "SK") ~ "Cheshire",
      postcode_area == "CA" ~ "Cumberland",
      TRUE ~ "Unknown"
    )
  )


write_csv(coverage_clean, file.path(base_path, "CleanedData/broadband_coverage_cleaned.csv"))

coverage_summary <- coverage_clean %>%
  mutate(
    postcode_district = str_extract(postcode, "^[A-Z]+[0-9]+")
  ) %>%
  group_by(postcode_district, region) %>%
  summarise(
    num_postcodes = n(),
    across(where(is.numeric), ~mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

write_csv(coverage_summary, file.path(base_path, "CleanedData/broadband_coverage_summary.csv"))

print("Broadband coverage data cleaned and saved!")
print(head(coverage_summary))