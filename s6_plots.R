library(tidyverse)
library(tidypaleo)
library(lattice)
library(ggpmisc)
library(patchwork)

data = c('Na', 'Mg', 'Al', 'Si', 'P', 'S', 'K', 'Ca', 'Ti', 'Mn', 'Fe', 'Cu', 'C',
         'N', 'Co', 'Ni')

age_breaks = c(seq(-80, 2600, 40))
depth_breaks = c(seq(0, 180, 10))

adm_data = tibble(depth = as.numeric(ages$depth), age = ages$median)
# adm_data[adm_data < 0] = 0
adm = age_depth_model(adm_data,
                      depth = depth,
                      age = age)

strat = c(27, 80, 310, 725, 963, 1526, 1547, 1690, 1735)
strat = c(27, 77, 307, 722, 862, 1482, 1547, 1687, 1732)
strat = c(27, 77)
strat_age = tibble(depth = strat) |> 
  left_join(ages, by = 'depth')
strat_age = strat_age$median

## 1. MAR plot
xrf_mar_plot = xrf_mar |> 
  pivot_longer(S:P, names_to = 'analyte',
               values_to = 'value') |> 
  filter(analyte %in% data) |> 
  mutate(analyte = fct_relevel(analyte, 'Na', 'Mg', 'Al', 'Si', 'P', 'S', 'K',
                               'Ca', 'Ti', 'Mn', 'Fe', 'Cu'))

mar_plot = ggplot(xrf_mar_plot, aes(x = value, y = depth,
                                    color = analyte)) + 
  geom_lineh(size = 0.5) +
  scale_y_reverse() +
  facet_geochem_gridh(vars(analyte)) +
  labs(x = 'MAR, г/м2 * год',
       y = 'Глубина, см') +
  theme_paleo() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = strat,
             col = 'black', size = 0.3, alpha = 0.5) +
  scale_y_depth_age(adm, age_name = 'Кал. лет назад', age_breaks = age_breaks)
mar_plot

ggsave('output/plots/MAR2.png', plot = mar_plot, device = 'png', width = 4800,
       height = 800, units = 'px')

sdata = c('Na2O', 'MgO', 'Al2O3', 'SiO2', 'P2O5', 'CaO', 'Ti2O', 'MnO', 'Fe2O3',
          'Cu')

mar_plot_standalone = ggplot(martotal_plot, aes(x = MAR_total,
                                                y = depth)) + 
  geom_lineh(size = 0.5) +
  geom_lineh(aes(x = accrate), color = 'red') +
  scale_y_reverse() +
  labs(x = 'MAR, г/м2 * год',
       y = 'Кал. л.н.') +
  theme_paleo() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7),
        legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_hline(yintercept = strat,
             col = 'black', size = 0.3, alpha = 0.5) +
  scale_y_depth_age(adm, age_name = 'Кал. лет назад', age_breaks = age_breaks)

mar_plot_standalone

ggsave('output/plots/top_MAR_total.svg',
       plot = mar_plot_standalone, device = 'svg', width = 600,
       height = 800, units = 'px')

## 2. Clr plot
xrf_clr_plot = xrf_clr |> 
  select(depth, median, Na2O:TiO2, MnO, Fe2O3, Cu, Sr, Pb, Rb, Co, Ni, Cr, Zr) |> 
  pivot_longer(Na2O:Zr, names_to = 'analyte',
               values_to = 'value')

clr_plot = ggplot(xrf_clr_plot, aes(x = value, y = depth, color = analyte)) +
  geom_lineh(size = 0.5) +
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

ggsave('output/plots/top_clr_plot.svg', plot = clr_plot, device = 'svg',
       width = 3200, height = 800, units = 'px')

xyplot(age ~ value | factor(analyte), data = kas_clr_plot, layout = c(11, 1),
       scales = list(x = 'free'), type = 'l')

# PCA график
xrf_pcs_plot = xrf_pca |> 
  left_join(prcomps, by = 'depth') |> 
  pivot_longer(Na2O:PC3, names_to = 'variable',
               values_to = 'value')

xrf_pcs_plot = prcomps |> 
  pivot_longer(PC1:PC3, names_to = 'PC',
               values_to = 'value')

pcs_plot = ggplot(xrf_pcs_plot, aes(x = value, y = depth, color = PC),
                  breaks = depth_breaks) +
  geom_vline(xintercept = 0, linetype = 6, alpha = 0.5, size = 0.2) +
  geom_lineh(size = 0.5) +
  scale_y_reverse() +
  scale_x_continuous(limits = symmetric_limits) +
  facet_geochem_gridh(vars(PC)) +
  labs(x = 'Главная компонента',
       y = 'Глубина, м') +
  theme_paleo() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.7),
        legend.position = 'none') +
  # geom_hline(yintercept = strat,
  #            col = 'black', size = 0.3, alpha = 0.5) +
  scale_y_depth_age(adm, age_name = 'Кал. лет назад', age_breaks = age_breaks)
pcs_plot

ggsave('output/plots/top_PCs.svg', plot = pcs_plot, device = 'svg', width = 2400,
       height = 800, units = 'px')

# PC plotting function
pc_plot = function(pc_dataset) {
  plot = ggplot(pc_dataset, aes(x = value, y = depth, color = variable)) +
    geom_lineh(size = 0.5) +
    scale_y_reverse() +
    facet_geochem_gridh(vars(variable)) +
    labs(x = NULL,
         y = 'Глубина, м') +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.7),
          legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_hline(yintercept = strat,
               col = 'black', size = 0.3, alpha = 0.5) +
    scale_y_depth_age(adm, age_name = 'Кал. лет назад', age_breaks = age_breaks)
  return(plot)
}

pc_plot_age = function(pc_dataset) {
  plot = ggplot(pc_dataset, aes(x = value, y = median, color = variable)) +
    geom_lineh(size = 0.5) +
    scale_y_reverse(breaks = age_breaks) +
    facet_geochem_gridh(vars(variable)) +
    labs(x = NULL,
         y = 'Кал. лет назад') +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.7),
          legend.position = 'none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    geom_hline(yintercept = strat_age,
               col = 'black', size = 0.3, alpha = 0.5)
  return(plot)
}

# Additional variables plotting
add = kas_pca |> 
  left_join(prcomps, by = 'depth') |> 
  select(depth, ms500hz)
  
add = add |> 
  pivot_longer(2:length(colnames(add)), names_to = 'variable',
               values_to = 'value')

add_plot = pc_plot(add) +
  scale_x_log10()
ggsave('output/plots/add.svg', plot = add_plot, device = 'svg', width = 800,
       height = 1400, units = 'px')

# PC1 plot
kas_pc1_plot = kas_pca |> 
  left_join(prcomps, by = 'depth') |> 
  left_join(ages, by = 'depth') |> 
  select(depth, PC1, S_cnhs, Fe2O3, MnO, Cu, P2O5) |> 
  left_join(select(kas_ratios, depth, CIA, `Mn/Fe`),
            by = 'depth')
  # left_join(select(kas_martotal_plot, depth, MAR_total), by = 'depth')
kas_pc1_plot = kas_pc1_plot |> 
  pivot_longer(2:length(colnames(kas_pc1_plot)), names_to = 'variable',
               values_to = 'value')

pc1_plot = pc_plot(kas_pc1_plot)
temp = wrap_plots(
  pc1_plot,
  add_plot,
  widths = c(8, 1)
)
temp
ggsave('output/plots/PC1.svg', plot = pc1_plot, device = 'svg', width = 2400,
       height = 1400, units = 'px')

# PC2 plot
kas_pc2_plot = kas_pca |> 
  left_join(prcomps, by = 'depth') |> 
  select(depth, PC2, CaO, Cu, Pb, S_cnhs) |> 
  left_join(select(kas_ratios, depth, `Ca/Fe`, CIA, `Si/Al`), by = 'depth') |> 
  left_join(select(kas_martotal_plot, depth, MAR_total), by = 'depth')
kas_pc2_plot = kas_pc2_plot |> 
  pivot_longer(2:length(colnames(kas_pc2_plot)), names_to = 'variable',
               values_to = 'value')

pc2_plot = pc_plot(kas_pc2_plot)
pc2_plot
ggsave('output/plots/PC2.svg', plot = pc2_plot, device = 'svg', width = 2400,
       height = 1400, units = 'px')

# PC3 plot
kas_pc3_plot = kas_pca |> 
  left_join(prcomps, by = 'depth') |> 
  select(depth, PC3, C, `C/N`, P2O5) |> 
  left_join(select(kas_ratios, depth, `Si/Ti`), by = 'depth') |> 
  left_join(select(kas_martotal_plot, depth), by = 'depth')
kas_pc3_plot = kas_pc3_plot |> 
  pivot_longer(2:length(colnames(kas_pc3_plot)), names_to = 'variable',
               values_to = 'value')

pc3_plot = pc_plot(kas_pc3_plot)
pc3_plot
ggsave('output/plots/PC3.svg', plot = pc3_plot, device = 'svg', width = 2400,
       height = 1400, units = 'px')

# PC4 plot
kas_pc4_plot = kas_pca |> 
  left_join(prcomps, by = 'depth') |> 
  select(depth, PC4, MgO, medium_sand) |> 
  left_join(select(kas_ratios, depth, `Ti/K`, `Zr/K`), by = 'depth')
kas_pc4_plot = kas_pc4_plot |> 
  pivot_longer(2:length(colnames(kas_pc4_plot)), names_to = 'variable',
               values_to = 'value')

pc4_plot = pc_plot(kas_pc4_plot)
ggsave('output/plots/PC4.svg', plot = pc4_plot, device = 'svg', width = 2400,
       height = 1400, units = 'px')

# PC5 plot
kas_pc5_plot = kas_pca |> 
  left_join(prcomps, by = 'depth') |> 
  select(depth, PC5, clay) |> 
  left_join(select(kas_ratios, depth, `K/Ti`),
            by = 'depth')
kas_pc5_plot = kas_pc5_plot |> 
  pivot_longer(2:length(colnames(kas_pc5_plot)), names_to = 'variable',
               values_to = 'value')

pc5_plot = pc_plot(kas_pc5_plot)
ggsave('output/plots/PC5.svg', plot = pc5_plot, device = 'svg', width = 2400,
       height = 1400, units = 'px')
