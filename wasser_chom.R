library(tidyverse)
library(transport)
library(WRS2)
library(boot)
library(viridis)

EE <- arrow::read_parquet("EE82_23.parquet")
indice_prix <- read.csv2("serie_011813530_03032026/valeurs_annuelles.csv", skip = 3) |> 
    as_tibble() |> 
    transmute(ANNEE = Période, ipc = as.numeric(X))

EE <- EE |> 
    filter(between(AG, 30, 60), gen5 >= "(1940,1945]") |> 
    mutate(AG5 = cut(AG, breaks = c(30, 35, 40, 45, 50, 55, 60), include.lowest = TRUE, ordered = TRUE),
           AG10 = cut(AG, breaks = c(30, 40, 50, 60), include.lowest = TRUE, ordered = TRUE),
           id = 1:n())

# On ne vire pas les NA de SAL pour garder les chômeurs
EE <- EE |> 
    filter(ANNEE >= 1990) |> 
    left_join(indice_prix, by = "ANNEE") |> 
    mutate(AN11  = cut(ANNEE, breaks = seq(1990, 2023, 11), include.lowest = TRUE, 
                       labels = c("90-00", "01-11", "12-23"), ordered = TRUE),
           GEN = (as.integer(AN11) - as.integer(AG10)) * 10 + 1960,
           GEN = factor(GEN))

# On filtre sur les 30-60 ans, on met à 1 le salaire des chomeurs, au foyer et autres inactifs.
# il faut également redresser les données des non réponses
# Pour cela on supprime au sein des statuts non en emploi autant de lignes que ce qui est drop_na(SAL) dans l'emploi.

sal_inconnu <- EE |> 
    filter(statut == "En emploi") |> 
    group_by(ANNEE) |> 
    summarise(n_inconnu = sum(is.na(SAL))/ n())


EE <- EE |> 
    drop_na(statut) |> 
    #filter(statut %in% c("En emploi", "Chômeur", "Au foyer", "Autres inactifs")) |> 
    filter(statut %in% c("En emploi", "Chômeur")) |> 
    mutate(SAL = if_else(statut != "En emploi" & is.na(SAL), 1, SAL)) |> 
    nest(data = -c(ANNEE, statut)) |> 
    left_join(sal_inconnu, by = "ANNEE") |> 
    mutate(data = pmap(list(data, n_inconnu, statut), \(x, y, z) {
        if (z != "En emploi") slice_sample(x, prop = 1 - y) else x
    })) |> 
    unnest(cols = data) |> 
    drop_na(SAL) |> 
    mutate(sal2025 = SAL * ipc / 100,
           logsal = log(sal2025))


csp_origin <- levels(EE$CSPP1)[-7] 
names(csp_origin) <- c("Agriculteur", "ACCE", "Cadre", "Pinter", "Employé", "Ouvrier")

EE <- EE |> 
    filter(CSPP1 != "Inactifs (n'ayant jamais travaillé)") |> 
    mutate(Origin = CSPP1 |> fct_recode(!!!csp_origin))

# ON FAIT AVEC diplome7
dip_levels <- EE$diplome7 |> levels()
names(dip_levels) <- c(paste0("Niveau", 6:0), "Non réponse")
EE <- EE |> 
    mutate(dip7 = fct_recode(diplome7, !!!dip_levels))

#
EE_nested <- EE |> 
    nest(data = -c(SEXE, AN11, AG10))


wass_boot <- function(data, indices, ori, var = "logsal") {
    d <- data[indices, ] |> filter(is.finite(.data[[var]]))
    x <- d |> filter(Origin == ori)      |> pull(.data[[var]])
    y <- d |> filter(Origin == "Ouvrier") |> pull(.data[[var]])
    if (length(x) < 2 || length(y) < 2) return(NA_real_)
    wasserstein1d(x, y, p = 2)
}
wass_fct <- function(df_list, var = "logsal", R = 100) {
    
    # Filtrage unique en amont
    df_list <- map(df_list, \(x) filter(x, is.finite(.data[[var]])))
    
    les_estimations <- reduce(c("Agriculteur", "ACCE", "Cadre", "Pinter", "Employé"), \(df, ori) {
        df |> mutate(
            "w_{ori}"  := map_dbl(df_list, \(x) wasserstein1d(
                a = x |> filter(Origin == ori)       |> pull(.data[[var]]),
                b = x |> filter(Origin == "Ouvrier") |> pull(.data[[var]]), p = 2)),
            "m_{ori}"  := map_dbl(df_list, \(x) mean(x[[var]][x$Origin == ori],        na.rm = TRUE)),
            "sd_{ori}" := map_dbl(df_list, \(x) sd(x[[var]][x$Origin == ori],          na.rm = TRUE))
        )
    }, .init = tibble(data = df_list)) |> 
        mutate(
            m_Ouvrier  = map_dbl(df_list, \(x) mean(x[[var]][x$Origin == "Ouvrier"], na.rm = TRUE)),
            sd_Ouvrier = map_dbl(df_list, \(x) sd(x[[var]][x$Origin == "Ouvrier"],  na.rm = TRUE))
        )
    
    les_boots <- reduce(c("Agriculteur", "ACCE", "Cadre", "Pinter", "Employé"), \(df, ori) {
        df |> mutate(
            "boot_{ori}" := map(df_list, \(x) boot(
                data = x,
                statistic = \(d, i) wass_boot(d, i, ori = ori, var = var),
                R = R
            ))
        )
    }, .init = tibble(data = df_list))
    
    les_ic <- les_boots |> 
        transmute(across(
            .cols = starts_with("boot_"),
            .fns  = \(x) map(x, \(y) {
                valid <- y$t[!is.na(y$t)]
                if (length(valid) < 10) return(c(NA_real_, NA_real_))
                y$t <- matrix(valid, ncol = 1)
                y$R  <- length(valid)
                tryCatch(
                    as.numeric(boot.ci(y, type = "perc")$percent[4:5]),
                    error = \(e) c(NA_real_, NA_real_)
                )
            }),
            .names = "{.col}_ci"
        )) |> 
        unnest_wider(
            col = ends_with("_ci"),
            names_sep = "_"
        )
    
    bind_cols(les_estimations, les_ic) |> select(-data)
}

EE_nested <- EE_nested |> 
    bind_cols(wass_fct(EE_nested$data, R=100))

# Premier graphique des inégalités de destin
ggplot(EE_nested, aes(x = AN11, y = AG10, fill = w_Cadre)) +
    geom_tile(color = "white", size = 0.5) +
    geom_text(aes(label = round(w_Cadre, 2)), color = "white", fontface = "bold") +
    geom_text(aes(label = paste0("[", round(boot_Cadre_ci_1, 3), ", ", round(boot_Cadre_ci_2, 3), "]")), 
              color = "white", size = 2.8, vjust = 2.5) +
    facet_wrap(~SEXE) +
    scale_fill_viridis(option = "viridis", direction = -1, name = "Indices de\nWasserstein") +
    labs(
        title = "Évolution de l'inégalité de destin (Distance de Wasserstein)",
        subtitle = "Comparaison Origine Ouvrière vs Cadre sur le log du salaire",
        x = "Période",
        y = "Tranche d'âge",
        caption = "Source : Enquête Emploi"
    ) +
    theme_minimal() +
    theme(
        panel.grid = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(color = "black")
    )

EE_nested |>
    mutate(
        trans_sq = (m_Ouvrier - m_Cadre)^2, 
        disp_sq = (sd_Ouvrier - sd_Cadre)^2,
        total_sq = trans_sq + disp_sq,
        part_translation = trans_sq / total_sq,
        part_dispersion = disp_sq / total_sq
    ) |>
    pivot_longer(cols = c(part_translation, part_dispersion), 
                 names_to = "Composante", values_to = "Valeur") |> 
    ggplot(aes(x = AN11, y = Valeur, fill = Composante)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_grid(SEXE ~ AG10) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "Set1", labels = c("Dispersion (Forme)", "Translation (Moyenne)")) +
    labs(title = "Décomposition de l'inégalité (W2) : Moyenne vs Dispersion",
         y = "Part de la distance totale", x = "Période") +
    theme_minimal()


 EE_nested |> 
     select(1:4) |> 
     mutate(tx_chom_ouv = map_dbl(data, \(x) {
         z = x |> 
             filter(Origin == "Ouvrier") |> 
             group_by(statut) |> 
             summarise(n = n()) |>
             pull(n, name = statut)
         z["Chômeur"] / (z["En emploi"] + z["Chômeur"])
         }),
         tx_chom_cad = map_dbl(data, \(x) {
             z = x |> 
                 filter(Origin == "Cadre") |> 
                 group_by(statut) |> 
                 summarise(n = n()) |>
                 pull(n, name = statut)
             z["Chômeur"] / (z["En emploi"] + z["Chômeur"])
         }),
         OR = tx_chom_ouv * (1 - tx_chom_cad) / tx_chom_cad / (1 - tx_chom_ouv))


 
 df_reg <- EE_nested |> 
     mutate(
         n_cadre = map_int(data, \(x) sum(x$Origin == "Cadre")),
         n_ouv   = map_int(data, \(x) sum(x$Origin == "Ouvrier")),
         n_min   = pmin(n_cadre, n_ouv)
     ) |> 
     select(SEXE, AG10, AN11, w_Cadre, n_min)

fit_pondere <- df_reg |> 
     filter(!is.na(w_Cadre)) |> 
     lm(w_Cadre ~ SEXE + AG10 + AN11,
        data    = _,
        weights = n_min)   # les grandes cellules comptent plus
 
 summary(fit_pondere)
 
 
 
 