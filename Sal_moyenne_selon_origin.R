source("nouvelles_variables.R")
library(viridis)

# Variations des moyennes de salaire selon Origin (ref = ouvrier)
moyennes_EE <- EE |> 
    group_by(ANby5, AG5, SEXE, Origin) |> 
    summarise(logsal = mean(logsal, na.rm = TRUE), 
              gen5 = first(gen5), 
              n = n(), .groups = "drop")

moyennes_EE_ouvrier <- moyennes_EE |> 
    filter(Origin == "Ouvrier") |> 
    rename(logsal_ref = logsal) |> 
    select(-Origin)

moyennes_EE |> 
    filter(Origin != "Agriculteur" & Origin != "Ouvrier") |> 
    left_join(moyennes_EE_ouvrier, by = c("ANby5", "AG5", "SEXE")) |> 
    mutate(diff_logsal = logsal - logsal_ref) |> 
    ggplot(aes(x = ANby5, y = AG5, fill = diff_logsal)) +
    geom_tile(color = "white", size = 0.5) +
    facet_grid(SEXE ~ Origin) +
    scale_fill_viridis(option = "viridis", direction = -1, name = "diff logsal") +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(color = "black")
    ) + 
    labs(title = "Diff logsal selon origine sociale (ref = ouvrier)")

moyennes_EE |> 
    filter(Origin != "Ouvrier") |> 
    left_join(moyennes_EE_ouvrier, by = c("ANby5", "AG5", "SEXE")) |> 
    mutate(diff_logsal = logsal - logsal_ref) |> 
    glm(diff_logsal ~ (ANby5 + AG5)*SEXE, data = _) |> 
    summary() # il y a une tendance linéaire qui diminue l'écart selon l'origine au cours des périodes et une tendance inverse d'accroissement selon l'age.

moyennes_EE |> 
    filter(Origin != "Ouvrier") |> 
    left_join(moyennes_EE_ouvrier, by = c("ANby5", "AG5", "SEXE")) |> 
    mutate(diff_logsal = logsal - logsal_ref) |> 
    glm(diff_logsal ~ ANby5 + AG5, data = _) |> 
    summary()