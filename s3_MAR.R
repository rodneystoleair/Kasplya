library(tidyverse)

# MAR
xrf_mar = xrf_norm |>
  left_join(tibble(depth = final_summary$depth,
                   volweight = final_summary$volweight),
            by = 'depth') |> 
  mutate(depth = as.numeric(depth)) |> 
  relocate(volweight, .before = S)

# kas_mar[is.na(kas_mar)] = 0

xrf_mar = xrf_mar |> 
  mutate(across(S:P) * volweight * accrate * 100)

ggplot(data = xrf_mar) +
  geom_line(aes(x = depth, y = Si)) +
  geom_line(aes(x = as.numeric(depth) - 3, y = Si, color = Si), data = xrf_norm) +
  scale_y_log10()

martotal_plot = xrf_summary |> 
  mutate(MAR_total = volweight * accrate)
