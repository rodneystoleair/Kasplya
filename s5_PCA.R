library(tidyverse)
library(factoextra)
library(ggfortify)
library(gridExtra)
library(ggcorrplot)

# Сводная таблица
# Создание сводной таблицы
xrf_pca = xrf_clr |> 
  left_join(final_summary, by = c('depth')) |> 
  select(-g_mean, -median, -loi550, -loi950, -gravel) |> 
  # select(-contains('sand')) |> 
  # select(-contains('silt')) |>
  mutate(depth = depth)
  # missMDA::MIPCA()
xrf_pca = as_tibble(xrf_pca$res.imputePCA)

xrf_corr = round(cor(xrf_pca[,-1]), 2)
pvalue = cor_pmat(xrf_pca[,-1])

ggcorrplot(xrf_corr, hc.order = T, type = 'lower', p.mat = pvalue,
           legend.title = 'Коэффициент
корреляции r', insig = 'blank')

# PCA
model_PCA = prcomp(xrf_pca[,-1], scale = T)
summary(model_PCA)
fviz_eig(model_PCA)
model_PCA_var = get_pca_var(model_PCA)
model_PCA_var$cos2[,1:5]

ggcorrplot(model_PCA_var$cos2[,1:5], legend.title = 'Факторная нагрузка,
первые 5 компонент,
cos2', 
           colors = c('red', 'white','darkgreen'))

fviz_pca_var(model_PCA, col.var = 'cos2', legend.title = 'Факторная нагрузка,
PC1',
             gradient.cols = c('red', 'yellow','darkgreen'), repel = T,
             title = 'МГК - переменные (PC1 v. PC2)')

fviz_pca_var(model_PCA, axes = c(1, 3), col.var = 'cos2',
             legend.title = 'Факторная нагрузка,
PC1',
             gradient.cols = c('red', 'yellow','darkgreen'), repel = T,
             title = 'МГК - переменные (PC1 v. PC3)')

fviz_pca_var(model_PCA, axes = c(2, 3), col.var = 'cos2',
             legend.title = 'Факторная нагрузка,
PC3',
             gradient.cols = c('red', 'yellow','darkgreen'), repel = T,
             title = 'МГК - переменные (PC2 v. PC3)')

prcomps = as_tibble(model_PCA[["x"]][,1:5]) |> 
  cbind(xrf_pca$depth) |> 
  rename(depth = `xrf_pca$depth`) |> 
  relocate(depth, .before = 'PC1')
