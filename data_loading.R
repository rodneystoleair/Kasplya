library(tidyverse)
source('R/functions.R')

# Enter core name
# core_name = readline('Enter exact core name: ')
core_name = 'Kas-17_top'

# Load tables from a directory
# Read all file names from a 'data' directory
all_files = list.files(
  paste0('data/', core_name, sep = ''),
  pattern = paste0('.*\\.xlsx$'),
  full.names = TRUE
) |>
  append(list.files(
    paste0('data/', core_name, sep = ''),
    pattern = paste0('.*\\.txt$'),
    full.names = TRUE
  ))

# Check if there any Excel temp file within "all_files" 
condition = any(grepl("/~\\$", all_files))
if (length(all_files) == 0) {
  stop("No files found in the specified directory.")
} else if (condition == T) {
  all_files = all_files[!grepl("/~\\$", all_files)]
  print('Excel temporary files found and excluded')
} else if (condition == F) {
  print('No Excel temporary files found')
}

# Age-depth model loading
age_match = grep('ages', all_files, ignore.case = TRUE)
ages = readxl::read_excel(all_files[age_match]) |>
  select(depth, median, accrate.median) |>
  mutate(depth = as.numeric(depth),
         accrate = round(accrate.median, 3)) |>
  select(-accrate.median)

# Summary table loading
summary_match = grep('summary', all_files, ignore.case = T)
summary = readxl::read_excel(all_files[summary_match]) |> 
  remove_na()

# Grain size data loading
gran_match = grep('gran', all_files, ignore.case = T)
gran_data = readxl::read_excel(all_files[gran_match])
summary_gran = standardize_columns(gran_data) |> 
  arrange(depth) |> 
  mutate(depth = round(depth, 0))

# CNHS data loading
# Check if there is a CNHS file in the directory
if (length(grep('cnhs', all_files, ignore.case = T)) == 0) {
  print('No CHNS detected')
} else {
  print('CHNS detected')
  cnhs = igras_specific_CNHS_loading(all_files)
}

# Parse variables
variables = c('550', '950', '500HZ', 'volweight')
final_summary = find_depth_column(summary) |> 
  mutate(depth = round(depth, 0))

for (i in variables) {
  # name = parse_name(summary, i)
  # assign(name, parse_variable(summary, i))
  variable = parse_variable(summary, i)
  final_summary = left_join(final_summary, variable, by = 'depth')
}
final_summary = left_join(final_summary, summary_gran, by = 'depth')

# Assign the depth column to a variable and rename it into "depth"
depth_col = find_depth_column(summary)
colnames(depth_col) = 'depth'

# If there are new ages (case of Kas-17 core)
if (length(grep('oldDepths_to_new', all_files, ignore.case = T)) == 1) {
  print('Old to new depths file detected')
  newDepths_match = grep('oldDepths_to_new', all_files, ignore.case = T)
  old_depths_to_new = readxl::read_excel(all_files[newDepths_match]) |>
    mutate(old = round(as.numeric(old / 100), 2),
           new = as.numeric(as.integer(new))) |>
    rename(depth = old)
} else {
  print('No old to new depths file detected')
}

# Left join new depths (in case if there are any) with parsed depths
if (exists('old_depths_to_new')) {
  # Subtract water table depth (if exists) from the depth column
  is_water = readline('Is there a water table? (Y/N): ')
  if (is_water == 'Y') {
    water_depth = readline('Enter water table depth (if exists, in m): ')
    if (water_depth != '') {
      water_depth = as.numeric(water_depth)
      depth_col$depth = depth_col$depth - water_depth
      final_summary = final_summary |> 
        mutate(depth = depth - water_depth)
      old_depths_to_new = old_depths_to_new |> 
        mutate(depth = depth - water_depth)
    } else {
      warning('Water table depth is not provided, skipping subtraction')
    }
  } else if (is_water == 'N') {
    print('Water table depth is not subtracted')
  } else {
    stop('Invalid input for water table depth')
  }
  final_summary = final_summary |> 
    left_join(old_depths_to_new, by = 'depth')
}

# Read XRF intensity data
xrf_match = grep('XRF', all_files, ignore.case = T)
xrf = readxl::read_excel(all_files[xrf_match], sheet = 'intensity',
                         col_names = c('depth', 'analyte', 'unit', 'value',
                                       'bias', 'intensity'),
                         col_types = c('numeric', 'text', 'text', 'numeric',
                                       'numeric', 'numeric')) |> 
  fill(depth)

if (exists('water_depth') == T) {
  xrf = xrf |> 
    mutate(depth = depth - water_depth)
}

xrf = xrf |>
  na.omit() |> 
  mutate(depth = round(depth, 2)) |> 
  # left_join(mutate(old_depths_to_new, depth = round(depth, 2)), by = 'depth') |> 
  # mutate(old_depth = depth,
  #        depth = new) |>
  left_join(ages, by = 'depth') |>
  relocate(depth)

# Final summary table adjustments
if (exists('old_depths_to_new')) {
  final_summary = final_summary |> 
    rename(depth = new,
           old_depth = depth) |> 
    select(-new)
}

xrf_wide = xrf |> 
  rowwise() |> 
  mutate(type = paste0(analyte, ', ', unit)) |> 
  relocate(unit, .before = analyte) |> 
  select(-analyte, -unit, -intensity, -bias) |> 
  pivot_wider(names_from = type, values_from = value)

xrf_summary = left_join(xrf_wide, final_summary, by = c('depth'))
