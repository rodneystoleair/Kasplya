library(tidyverse)
library(tidypaleo)

# Read the CSV file
climate_url = 'https://download.pangaea.de/reference/111444/attachments/LegacyClimate_csv_files.zip'
temp_zip = tempfile(fileext = ".zip")
download.file(climate_url, destfile = temp_zip, mode = "wb")
climate_tab_path = 'climate_reconstruction_europe.csv'
unzip(temp_zip, files = climate_tab_path, exdir = tempdir())
extracted_path = file.path(tempdir(), climate_tab_path)

climate_europe = read_csv(extracted_path) |> 
  filter(Latitude >= 50, Latitude <= 60,
         `Age [ka BP] (median)` >= 0, `Age [ka BP] (median)` <= 14) |> 
  select(`Age [ka BP] (median)`, contains('T air (July)')) |> 
  mutate(`Age BP` = `Age [ka BP] (median)` * 1000) |> 
  select(-`Age [ka BP] (median)`)

intervals <- paste0(seq(0, 13950, by = 50), "-", seq(50, 14000, by = 50))

summary_curve = climate_europe |> 
  mutate(
    interval = cut(
      `Age BP`,
      breaks = seq(0, 14000, by = 50),
      include.lowest = TRUE,
      labels = intervals,
      right = FALSE  # Интервалы вида [a, b), чтобы 100 попало в [100-200)
    ) |> 
      fct_relevel(intervals)  # Явно задаем порядок уровней
  ) |> 
  group_by(interval) |> 
  summarise(median = median(`T air (July) [°C] (WA-PLS)`)) |> 
  mutate(
    ceiling = as.numeric(str_remove(as.character(interval), ".*-"))
    )

europe = ggplot(summary_curve, aes(x = median, y = ceiling, color = median)) +
  geom_lineh(size = 0.5) +
  scale_y_reverse() +
  scale_y_reverse(breaks = age_breaks) +
  geom_hline(yintercept = strat_age,
             col = 'black', size = 0.3, alpha = 0.5) +
  labs(title = "Summary Curve",
       x = "Median T air (July) [°C] (WAPLS)",
       y = "Age Interval [ka BP]") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
europe

# NGRIP
ngrip_url = "http://iceandclimate.nbi.ku.dk/data/NGRIP_d18O_and_dust_5cm.xls"
temp = tempfile(fileext = ".xls")
download.file(ngrip_url, destfile = temp, mode = "wb")

ngrip1 = readxl::read_excel(temp, sheet = 2) |> 
  rename(depth = 1,
         dO18 = 2,
         age = 3) |> 
  select(-4)

ngrip2 = readxl::read_excel(temp, sheet = 3) |> 
  select(-3) |>
  rename(depth = 1,
         dO18 = 2,
         age = 3) |> 
  select(-4) |> 
  filter(age > max(ngrip1$age))

ngrip = bind_rows(ngrip1, ngrip2) |> 
  filter(age < 14000) |> 
  mutate(smooth = 
           predict(loess(dO18 ~ age, ngrip, span = 0.01))
           )

ngrip_plot = ggplot(ngrip, aes(x = smooth, y = age, color = dO18)) +
  geom_lineh(size = 0.5) +
  scale_y_reverse() +
  scale_y_reverse(breaks = age_breaks) +
  geom_hline(yintercept = strat_age,
             col = 'black', size = 0.3, alpha = 0.5) +
  labs(title = "NGRIP",
       x = "dO18",
       y = "Age") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ngrip_plot

# Summary plot
kas_sum_plot = kas_pca |> 
  left_join(prcomps, by = 'depth') |> 
  left_join(ages, by = 'depth') |> 
  select(depth, median, PC1, PC2, PC3, C, `C/N`, fine_silt) |> 
  left_join(select(kas_ratios, depth, `Rb/Sr`, CIA, `Mn/Fe`),
            by = 'depth') |> 
  left_join(select(kas_martotal_plot, depth, MAR_total), by = 'depth')

kas_sum_plot = kas_sum_plot |> 
  pivot_longer(3:length(colnames(kas_sum_plot)), names_to = 'variable',
               values_to = 'value')

sum_plot = wrap_plots(
  pc_plot_age(kas_sum_plot),
  europe,
  ngrip_plot,
  widths = c(10, 1, 1)
)
sum_plot
  
ggsave('output/plots/sum.svg', plot = sum_plot, device = 'svg', width = 3200,
       height = 1400, units = 'px')
