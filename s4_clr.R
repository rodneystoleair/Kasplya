library(tidyverse)

# Расчёт centered log-ratio (clr) 
xrf_int = xrf |> 
  rowwise() |> 
  mutate(type = analyte) |> 
  select(-analyte, -unit, -value, -bias, -accrate) |> 
  pivot_wider(names_from = type, values_from = intensity) |> 
  select(-`S`, -`Th`) |> 
  ungroup() |> 
  mutate(depth = as.numeric(depth))

colnames(xrf_int) = colnames(xrf_int) |> 
  sapply(function (x) {
    if (str_detect(x, ',')) {
      x = unlist(strsplit(x, ',',fixed = T))[1]
    } else {
      x = x
    }
  }) |> 
  as.vector()

xrf_clr = xrf_int |> 
  select(-U, -Nb, -As) |> 
  rowwise() |> 
  mutate(g_mean = prod(across(Na2O:Pb)) ^ (1 / ncol(across(Na2O:Pb)))) |> 
  mutate(log(across(Na2O:Pb) / g_mean)) |> 
  ungroup()

# kas_clr = kas_int |>
#   select(depth, age, Na2O:TiO2, MnO, Fe2O3, Ba) |>
#   rowwise() |>
#   mutate(g_mean = prod(across(Na2O:Ba)) ^ (1 / ncol(across(Na2O:Ba)))) |>
#   mutate(log(across(Na2O:Ba) / g_mean)) |>
#   ungroup()

ggplot(xrf_clr) +
  geom_line(aes(x = depth, y = SiO2)) +
  geom_line(aes(x = depth, y = SiO2 / 10000, color = SiO2), data = xrf_int)
