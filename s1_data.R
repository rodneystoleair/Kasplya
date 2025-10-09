library(tidyverse)

# Новые глубины
kas_old_depths_to_new = readxl::read_excel(
  'data/Kas-17_oldDepths_to_new.xlsx'
) |> 
  mutate(old = as.character(old / 100),
         new = as.character(as.integer(new))) |> 
  rename(depth = old)

# Хронология
ages = readxl::read_excel('data/Kas-17_NewDepths_accrate_ages.xlsx') |> 
  select(depth, median, accrate.median) |>
  mutate(depth = as.character(depth),
         accrate = round(accrate.median, 3)) |> 
  select(-accrate.median)

# Сводная таблица
kas_summary1 = readxl::read_excel("data/Каспля-сводный – для анализа РФА.xlsx", 
                                  sheet = "sheet1") |> 
  select(`Глубина от льда, м`:`кумулятивная масса, гр`)
colnames(kas_summary1) = c('depth', 'loi550', 'loi950', 'CO2', 'ms500hz',
                           'moisture', 'volweight', 'massrate', 'cummass')
kas_summary1 = kas_summary1 |> 
  mutate(depth = round(depth, 2)) |> 
  mutate(depth = as.character(depth)) |> 
  select(depth, ms500hz, volweight, loi550, loi950) |> 
  left_join(kas_old_depths_to_new, by = 'depth') |> 
  mutate(old_depth = depth,
         depth = new) |> 
  left_join(ages, by = 'depth') |> 
  select(-new) |> 
  relocate(depth) |> 
  mutate(depth = as.numeric(depth))
kas_summary1[is.na(kas_summary1)] = 0

# Таблица, содержащая данные по грансоставу
kas_gran = readxl::read_excel("data/Каспля-сводный – для анализа РФА.xlsx", 
                              sheet = "sheet2") |> 
  transform(depth = as.numeric(depth)) |> 
  transform(depth = round(depth, 2))
colnames(kas_gran) = c('depth', 'clay',
                       'very_fine_silt',
                       'fine_silt',
                       'medium_silt',
                       'coarse_silt',
                       'very_coarse_silt',
                       'very_fine_sand',
                       'fine_sand',
                       'medium_sand',
                       'coarse_sand',
                       'very_coarse_sand',
                       'gravel')
kas_gran = kas_gran |> 
  select(-very_coarse_sand, -gravel) |> 
  left_join(mutate(kas_old_depths_to_new, depth = as.numeric(depth)),
            by = 'depth') |> 
  select(-depth) |> 
  mutate(depth = as.numeric(new)) |> 
  relocate(depth) |> 
  select(-new)

# CNHS
kas_cnhs = readxl::read_excel("data/Konstantinov_032024CNHS.xlsx", 
                               sheet = "ведомость", skip = 3) |> 
  select('Шифр на пробирке', 'Глубина отбора, м')
colnames(kas_cnhs) = c('Name', 'depth')
kas_cnhs = arrange(kas_cnhs, depth)

kas_cnhs2 = readxl::read_excel("data/Konstantinov_032024CNHS.xlsx",
                               sheet = "CN data", skip = 1)
colnames(kas_cnhs2) = c('Name', 'C', 'N', 'H', 'S_cnhs', 'C/N', 'CNatm')

kas_cnhs = kas_cnhs |> 
  left_join(kas_cnhs2, by = 'Name') |> 
  select(depth, C, N, H, S_cnhs, `C/N`) |> 
  mutate(depth = as.character(depth))

kas_cnhs = kas_cnhs |> 
  left_join(kas_old_depths_to_new, by = 'depth') |> 
  relocate(new, .after = 'depth') |>
  select(-depth) |>
  rename(depth = new) |> 
  mutate(depth = as.numeric(depth))

# Интенсивность
kas = readxl::read_excel('data/Kas-17_XRF_fixed_intensity.xlsx',
                         sheet = 'интенсивность',
                         col_names = c('depth', 'analyte', 'unit', 'value',
                                       'bias', 'intensity'),
                         col_types = c('numeric', 'text', 'text', 'numeric',
                                       'numeric', 'numeric')) |> 
  fill(depth) |> 
  na.omit() |> 
  mutate(depth = round(depth, 2)) |> 
  transform(depth = as.character(depth)) |> 
  left_join(kas_old_depths_to_new, by = 'depth') |> 
  mutate(old_depth = depth,
         depth = new) |> 
  left_join(ages, by = 'depth') |> 
  select(-new) |> 
  relocate(depth)

# По элементам
kas_wide = kas |> 
  rowwise() |> 
  mutate(type = paste0(analyte, ', ', unit)) |> 
  relocate(unit, .before = analyte) |> 
  select(-analyte, -unit, -intensity, -bias) |> 
  pivot_wider(names_from = type, values_from = value)

kas_summary = kas_wide |> 
  mutate(depth = as.numeric(depth)) |> 
  left_join(select(kas_summary1, -old_depth, -median, -accrate),
            by = 'depth') |> 
  left_join(kas_cnhs, by = 'depth') |> 
  left_join(select(kas_martotal_plot, depth, MAR_total), by = 'depth')


# Kas-17_top data loading
kas_top = readxl::read_excel('data/Kas17_top_XRF.xlsx',
                             sheet = 1) |> 
  na.omit()

# Remove all rows that come after the first NA row
kas_top_summary = readxl::read_excel('data/Kas-17 Top_ сведенные результаты_испр.xlsx',
                                     skip = 1)
kas_top_summary = filter(kas_top_summary, 
                         cumsum(rowSums(!is.na(kas_top_summary)) == 0) == 0)

# Remove all columns that are completely NA with dplyr
kas_top_summary = kas_top_summary |> 
  select(where(~ !all(is.na(.))))

kas_top_realDepths = kas_top_summary |> 
  select(contains('№'), `...7`) |> 
  rename(depth = `...7`, 
         n = contains('№')) |>
  mutate(depth = round(depth, 0))

kas_top_int = readxl::read_excel(
  'data/Kas17_top_XRF.xlsx',
  sheet = 'интенсивность',
  col_names = c('n', 'analyte', 'unit', 'value',
                'bias', 'intensity'),
  col_types = c('numeric', 'text', 'text', 'numeric',
                'numeric', 'numeric')
) |> 
  fill(n) |>
  na.omit() |> 
  mutate(n = as.character(n)) |> 
  mutate(depth = as.character(kas_top_realDepths$depth[match(n, kas_top_realDepths$n)])) |> 
  left_join(ages, by = 'depth')

kas_top_wide = kas_top_int |> 
  rowwise() |> 
  mutate(type = paste0(analyte, ', ', unit)) |> 
  relocate(unit, .before = analyte) |> 
  select(-analyte, -unit, -intensity, -bias) |> 
  pivot_wider(names_from = type, values_from = value)
