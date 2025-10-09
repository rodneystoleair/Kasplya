library(tidyverse)

# Нормализованные по сумме элементов значения
xrf_norm = xrf_wide |> 
  mutate(across(contains('мг/кг')) / 10000) |> 
  mutate(Si = `SiO2, %` / 2.14,
         Al = `Al2O3, %` / 1.89,
         Fe = `Fe2O3, %` / 1.43,
         Mg = `MgO, %` / 1.66,
         Ca = `CaO, %` / 1.40,
         Na = `Na2O, %` / 1.35,
         K  = `K2O, %` / 1.21,
         Mn = `MnO, мг/кг` / 1.58,
         Ti = `TiO2, %` / 1.67,
         P  = `P2O5, %` / 2.29) |> 
  select(-contains('%'), -median)
  
kas_norm[kas_norm < 0] = 0
kas_norm = kas_norm |> 
  missMDA::MIPCA()
kas_norm = as_tibble(kas_norm$res.imputePCA) |> 
  left_join(select(mutate(ages, depth = as.numeric(depth)), -accrate),
            by = 'depth') |> 
  relocate(median, .after = depth)

colnames(xrf_norm) = colnames(xrf_norm) |> 
  sapply(function (x) {
    if (str_detect(x, ',')) {
      x = unlist(strsplit(x, ',',fixed = T))[1]
    } else if (str_detect(x, 'O')) {
      x = unlist(strsplit(x, 'O',fixed = T))[1]
    } else {
      x = x
    }
  }) |> 
  as.vector()

xrf_norm = xrf_norm |> 
  select(-MnO)
xrf_norm = xrf_norm |> 
  rowwise() |> 
  mutate(sum = sum(across(S:P))) |> 
  mutate(across(S:P) / sum * 100) |> 
  mutate(sum2 = sum(across(S:P)))
xrf_norm

xrf_norm = xrf_norm |> 
  select(-contains('sum'))

# Геохимические коэффициенты
xrf_ratios = xrf_norm |>
  mutate(`Al/Si` = Al / Si,
         `Ca/Fe` = Ca / Fe,
         `Ca/Mg` = Ca / Mg,
         `Fe/Mn` = Fe / Mn,
         `Fe/Si` = Fe / Si,
         `K/Al`  = K  / Al,
         `K/Ti`  = K  / Ti,
         `Mn/Fe` = Mn / Fe,
         `Rb/Sr` = Rb / Sr,
         `S/Ti`  = S  / Ti,
         `Si/Ti` = log(Si / Ti),
         `Sr/Ca` = Sr / Ca,
         `Ti/K`  = Ti / K,
         `Zr/Fe` = Zr / Fe,
         `Zr/K`  = Zr / K,
         `Ca/Ti` = log(Ca / Ti),
         `Si/Al` = Si / Al,
         CIA_rat = Al + Ca + Na + K,
         Silica = Si,
         Aluminum = Al,
         .keep = 'none') |> 
  mutate(BioSi_temp = Aluminum * `Si/Al`) |> 
  mutate(CIA = Aluminum / CIA_rat,
         BioSi = Silica - BioSi_temp) |> 
  select(-CIA_rat, -Silica, -Aluminum)
