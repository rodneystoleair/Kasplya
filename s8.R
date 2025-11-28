# writexl::write_xlsx(final_summary, 
#                     path = paste0('output/', core_name, '_summary.xlsx'))
# 
# writexl::write_xlsx(xrf_summary, 
#                     path = paste0('output/', core_name, '_XRF_wide.xlsx'))
# 
# writexl::write_xlsx(xrf_clr, 
#                     path = paste0('output/', core_name, '_XRF_clr.xlsx'))
# 
# writexl::write_xlsx(xrf_mar, 
#                     path = paste0('output/', core_name, '_XRF_mar.xlsx'))

age_breaks = c(seq(-80, 2500, 250))
depth_breaks = c(seq(0, 180, 30))
strat = c(27, 77)

additional_xrf_clr = readxl::read_excel('output/Kas-17_top_XRF_clr.xlsx')

additional_xrf_clr_plot = additional_xrf_clr |> 
  select(depth, median, Na2O:TiO2, MnO, Fe2O3, Cu, Sr, Pb, Rb, Zr) |> 
  pivot_longer(Na2O:Zr, names_to = 'analyte',
               values_to = 'value') |> 
  mutate(analyte = fct_relevel(analyte, 'Na2O', 'MgO', 'Al2O3', 'SiO2', 'P2O5', 'K2O',
                               'CaO', 'TiO2', 'MnO', 'Fe2O3', 'Cu',
                               'Sr', 'Pb', 'Rb', 'Zr'))

xrf_clr_plot = xrf_clr |> 
  select(depth, median, Na2O:TiO2, MnO, Fe2O3, Cu, Sr, Pb, Rb, Zr) |> 
  pivot_longer(Na2O:Zr, names_to = 'analyte',
               values_to = 'value') |> 
  filter(depth <= 180) |> 
  mutate(analyte = fct_relevel(analyte, 'Na2O', 'MgO', 'Al2O3', 'SiO2', 'P2O5', 'K2O',
                               'CaO', 'TiO2', 'MnO', 'Fe2O3', 'Cu',
                               'Sr', 'Pb', 'Rb', 'Zr'))

clr_plot = ggplot(xrf_clr_plot, aes(x = value, y = depth, color = analyte)) +
  geom_lineh(size = 0.5,
             linetype = 'dashed') +
  geom_lineh(data = additional_xrf_clr_plot,
             aes(x = value, y = depth, color = analyte),
             size = 0.5,
             alpha = 0.8) +
  scale_y_reverse() +
  facet_geochem_gridh(vars(analyte)) +
  labs(x = 'Clr',
       y = 'Глубина, см') +
  theme_paleo() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = strat,
             col = 'black', size = 0.3, alpha = 0.5) +
  scale_y_depth_age(adm, age_name = 'Кал. лет назад', age_breaks = age_breaks)
clr_plot

ggsave(
  'output/plots/Kas-17_top_comparison_clr.svg',
  plot = clr_plot,
  device = 'svg',
  width = 3200,
  height = 1000,
  units = 'px'
)

additional_summary = readxl::read_excel('output/Kas-17_top_summary.xlsx')

additional_summary_plot = additional_summary |> 
  select(-very_coarse_sand, -gravel, -accrate) |> 
  relocate(loi550, .after = depth) |> 
  pivot_longer(loi550:coarse_sand, names_to = 'type',
               values_to = 'value') |> 
  mutate(type = fct_relevel(type, 'loi550', 'loi950', '500HZ',
                               'volweight', 'clay', 'very_fine_silt',
                               'fine_silt', 'medium_silt', 'coarse_silt',
                               'very_coarse_silt', 'very_fine_sand', 
                               'fine_sand', 'medium_sand', 'coarse_sand'))

summary_plot = final_summary |> 
  select(-very_coarse_sand, -gravel, -old_depth) |> 
  rename(loi550 = `LOI 550`,
         loi950 = `LOI d950`,
         `500HZ` = `УМВ 500HZ`) |>
  filter(depth <= 180) |> 
  pivot_longer(loi550:coarse_sand, names_to = 'type',
               values_to = 'value') |> 
  mutate(type = fct_relevel(type, 'loi550', 'loi950', '500HZ',
                               'volweight', 'clay', 'very_fine_silt',
                               'fine_silt', 'medium_silt', 'coarse_silt',
                               'very_coarse_silt', 'very_fine_sand', 
                               'fine_sand', 'medium_sand', 'coarse_sand'))

sum_plot = ggplot(summary_plot,
                  aes(x = value, y = depth, color = type)) +
  geom_lineh(linetype = 'dashed') +
  geom_lineh(data = additional_summary_plot,
             aes(x = value, y = depth, color = type),
             size = 0.5,
             alpha = 0.8) +
  scale_y_reverse() +
  facet_geochem_gridh(vars(type)) +
  labs(x = '',
       y = 'Глубина, см') +
  theme_paleo() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = strat,
             col = 'black', size = 0.3, alpha = 0.5) +
  scale_y_depth_age(adm, age_name = 'Кал. лет назад', age_breaks = age_breaks)
sum_plot

ggsave(
  'output/plots/Kas-17_top_comparison_summary.svg',
  plot = sum_plot,
  device = 'svg',
  width = 3200,
  height = 1000,
  units = 'px'
)
