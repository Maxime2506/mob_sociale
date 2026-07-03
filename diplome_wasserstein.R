library(tidyverse)
library(transport)
library(WRS2)
library(boot)

EE <- arrow::read_parquet("EE82_23.parquet")
indice_prix <- read.csv2("serie_011813530_03032026/valeurs_annuelles.csv", skip = 3) |> 
    as_tibble() |> 
    transmute(ANNEE = Période, ipc = as.numeric(X))


EE <- EE |> 
    filter(between(AG, 30, 60), gen5 >= "(1940,1945]") |> 
    mutate(AG5 = cut(AG, breaks = c(30, 35, 40, 45, 50, 55, 60), include.lowest = TRUE, ordered = TRUE),
           AG10 = cut(AG, breaks = c(30, 40, 50, 60), include.lowest = TRUE, ordered = TRUE),
           id = 1:n())

EE <- EE |> 
    filter(ANNEE >= 1990, SAL > 0) |> 
    drop_na(SAL) |> 
    left_join(indice_prix, by = "ANNEE") |> 
    mutate(sal2025 = SAL * ipc / 100,
           logsal = log(sal2025),
           AN11  = cut(ANNEE, breaks = seq(1990, 2023, 11), include.lowest = TRUE, 
                       labels = c("90-00", "01-11", "12-23"), ordered = TRUE),
           GEN = (as.integer(AN11) - as.integer(AG10)) * 10 + 1960,
           GEN = factor(GEN))

# ON FAIT AVEC diplome7
dip_levels <- EE$diplome7 |> levels()
names(dip_levels) <- c(paste0("Niveau", 6:0), "Non réponse")
EE <- EE |> 
    mutate(dip7 = fct_recode(diplome7, !!!dip_levels))


EE_dip7 <- EE |> 
    drop_na(diplome7) |> 
    filter(diplome7 != "Non réponse") |> 
    nest(data = -c(SEXE, AN11, AG10))

# Fonction de transport optimal pour diplome13 - pour produire un contrefactuel de rendement égal.
# fonction de mapping pour transporter une distribution d'une catégorie vers une autre
quantile_map <- function(source_sal, target_sal) {
    
    percentiles <- ecdf(source_sal)(source_sal)
    percentiles <- pmin(pmax(percentiles, 0.001), 0.999)
    
    # Le mapping : on cherche quelle valeur correspond à ce percentile chez les cadres
    quantile(target_sal, probs = percentiles, type = 7) |> as.numeric()
}
   

EE_dip7 <- EE_dip7 |> 
    mutate(data = map(data, \(x) {
        sal_bac <- x |> filter(dip7 == "Niveau0") |> pull(logsal)
        if(length(sal_bac) == 0) return(x |> mutate(logsal_contrefactuel = logsal))
        
        mapping_table <- x |> 
            filter(dip7 != "Niveau0") |> 
            group_by(dip7) |> 
            mutate(logsal_contrefactuel = quantile_map(logsal, sal_bac)) |> 
            ungroup() |> 
            select(id, logsal_contrefactuel) 
        
        # 3. Fusionner le résultat avec le tibble d'origine
        x |> 
            left_join(mapping_table, by = "id") |> 
            mutate(logsal_contrefactuel = coalesce(logsal_contrefactuel, logsal))
    }))

# Mesure de wasserstein
wass_diplome <- function(data, niv, ref = 0, var = "logsal") {
    wasserstein1d(
        a = data |> filter(dip7 == paste0("Niveau", ref)) |> pull(!!var), 
        b = data |> filter(dip7 == paste0("Niveau", niv)) |> pull(!!var), 
        p = 2)
}   

res_EE7 <- reduce(0:6, \(df, niv) {
    df |> mutate(
        "w_{niv}"  := map_dbl(data, \(x) wass_diplome(x, niv = niv)),
        "m_{niv}"  := map_dbl(data, \(x) mean(x$logsal[x$dip7 == paste0("Niveau", niv)], na.rm = TRUE)),
        "sd_{niv}" := map_dbl(data, \(x) sd(x$logsal[x$dip7   == paste0("Niveau", niv)], na.rm = TRUE))
    )
}, .init = EE_dip7)

res_EE7 <- res_EE7 |>
    select(-data) |> 
    pivot_longer(
        cols = -c(SEXE, AG10, AN11),
        names_to  = "variable",
        values_to = "valeur"
    ) |> 
    mutate(
        mesure  = case_match(str_sub(variable, 1,1),
                             "w" ~ "wasserstein",
                             "m" ~ "moyenne",
                             "s" ~ "sd"),
        diplome = paste0("Niveau", gsub("^w_|^m_|^sd_", "", variable))
    ) |> 
    select(-variable) |> 
    pivot_wider(names_from = mesure, values_from = valeur) |> 
    mutate(diplome = ordered(diplome, levels = paste0("Niveau", 0:6)))

# Calcul de IC
# Fonction à passer à boot()
w2_stat <- function(data, indices, niv) {
    d <- data[indices, ]
    x <- d$logsal[d$dip7 == paste0("Niveau", niv)]
    y <- d$logsal[d$dip7 == "Niveau0"]
    if (length(x) < 2 | length(y) < 2) return(NA)
    wasserstein1d(x, y, p = 2)
}

res_ci7 <- reduce(1:6, \(df, niv) {
    df |> mutate("boot_{niv}" := map(data, \(x) boot(data = x, statistic = \(d,i) w2_stat(d,i, niv = niv), R = 100)))
}, .init = EE_dip7)

res_ci7 <- res_ci7 |> 
    mutate(across(.cols = boot_1:boot_6, .fns = \(x) map(x, \(y) {
        temp <- boot.ci(y, type = "perc")$percent[4:5]
        })))
res_ci7 <- res_ci7 |> 
    select(-data) |> 
    pivot_longer(
        cols = -c(SEXE, AG10, AN11),
        names_to = "diplome",
        values_to = "boot",
    ) |> 
    mutate(diplome = paste0("Niveau", str_sub(diplome, -1)),
           bmin95 = map_dbl(boot, \(x) pluck(x, 1)),
           bmax95 = map_dbl(boot, \(x) pluck(x, 2))) |> 
    select(-boot)

res_EE7 <- res_EE7 |> 
    left_join(res_ci7, by = c("SEXE", "AG10", "AN11", "diplome"))

res_EE7 |> 
    ggplot(aes(x = AN11, y = wasserstein, col = AG10, group = AG10)) + 
    geom_line() +
    geom_point(alpha = .5, size = 3)+
    facet_grid(diplome~SEXE)



# avec l'origine sociale
pcs_list <- c("Cadres et professions intellectuelles", "Professions intermédiaires", "Employés", "Ouvriers")

EE_OD <- EE |> 
    drop_na(diplome7, CSPP1) |> 
    filter(diplome7 != "Non réponse", CSPP1 %in% pcs_list) |> 
    nest(data = -c(SEXE, AN11, AG10, diplome7))

# Mesure de wasserstein
wass_cspp <- function(data, ref = "Ouvriers", niv, var = "logsal", wa = NULL, wb = NULL) {
    a = data |> filter(CSPP1 == ref) |> pull(!!var)
    if (!is.null(wa)) { wa = data |> filter(CSPP1 == ref) |> pull(w_origin)}
    b = data |> filter(CSPP1 == niv) |> pull(!!var)
    if (!is.null(wb)) { wb = data |> filter(CSPP1 == niv) |> pull(w_origin)}
    if (length(a) ==0 | length(b) == 0) return(NA)
    wasserstein1d(
        a=a, b=b, wa = wa, wb = wb,
        p = 2)
}   

EE_OD <- EE_OD |> 
    mutate(data = map(data, \(x) {
        sal_ouv <- x |> filter(CSPP1 == "Ouvriers") |> pull(logsal)
        if(length(sal_ouv) == 0) return(x |> mutate(logsal_contrefactuel = logsal))
        
        mapping_table <- x |> 
            filter(CSPP1 == "Ouvriers") |> 
            group_by(CSPP1) |> 
            mutate(logsal_contrefactuel = quantile_map(logsal, sal_ouv)) |> 
            ungroup() |> 
            select(id, logsal_contrefactuel) 
        
        # 3. Fusionner le résultat avec le tibble d'origine
        x |> 
            left_join(mapping_table, by = "id") |> 
            mutate(logsal_contrefactuel = coalesce(logsal_contrefactuel, logsal))
    }))


res_OD <- EE_OD |> 
    mutate(w_cadres = map_dbl(data, \(x) wass_cspp(x, niv = "Cadres et professions intellectuelles")),
           m_cadres = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Cadres et professions intellectuelles"])),
           sd_cadres = map_dbl(data, \(x) sd(x$logsal[x$CSPP1 == "Cadres et professions intellectuelles"])),
           n_cadres = map_dbl(data, \(x) sum(x$CSPP1 == "Cadres et professions intellectuelles")),
           w_pi = map_dbl(data, \(x) wass_cspp(x, niv = "Professions intermédiaires")),
           m_pi = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Professions intermédiaires"])),
           sd_pi = map_dbl(data, \(x) sd(x$logsal[x$CSPP1 == "Professions intermédiaires"])),
           n_pi = map_dbl(data, \(x) sum(x$CSPP1 == "Professions intermédiaires")),
           w_emp = map_dbl(data, \(x) wass_cspp(x, niv = "Employés")),
           m_emp = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Employés"])),
           sd_emp = map_dbl(data, \(x) sd(x$logsal[x$CSPP1 == "Employés"])),
           n_emp = map_dbl(data, \(x) sum(x$CSPP1 == "Employés")),
           w_ouvrier = map_dbl(data, \(x) wass_cspp(x, niv = "Ouvriers")),
           m_ouvrier = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Ouvriers"])),
           sd_ouvrier = map_dbl(data, \(x) sd(x$logsal[x$CSPP1 == "Ouvriers"])),
           n_ouvrier = map_dbl(data, \(x) sum(x$CSPP1 == "Ouvriers"))) |> 
    select(-data)

res_OD <- res_OD |>
    pivot_longer(
        cols = -c(SEXE, AG10, AN11, diplome7),
        names_to  = "variable",
        values_to = "valeur"
    ) |> 
    mutate(
        mesure  = case_match(str_sub(variable, 1,1),
                             "w" ~ "wasserstein",
                             "m" ~ "moyenne",
                             "s" ~ "sd",
                             "n" ~ "n"),
        cspp1 = gsub("^w_|^m_|^sd_|^n_", "", variable)
    ) |> 
    select(-variable) |> 
    pivot_wider(names_from = mesure, values_from = valeur) |> 
    mutate(cspp1 = factor(cspp1))

res_OD <- res_OD |> 
    group_by(SEXE, AG10, AN11, diplome7) |> 
    mutate(delta_m = moyenne - moyenne[cspp1 == "ouvrier"])


# Résultats comparant origine ouvrier vs cadre
res_OD |> 
    filter(cspp1 == "cadres", diplome13 %in% c("Aucun diplôme", "Baccalauréat général", "Licence/Maîtrise", "Grande école")) |> 
    ggplot(aes(x = AG10, y = wasserstein, col = gen, group = gen)) + 
    geom_line() + geom_point(alpha = .3, size = 5)+
    facet_grid(diplome13~SEXE)

res_OD |> 
    filter(cspp1 == "cadres", diplome13 %in% c("Aucun diplôme", "Baccalauréat général", "Licence/Maîtrise", "Grande école")) |> 
    ggplot(aes(x = AG10, y = delta_m, col = gen, group = gen)) + 
    geom_line() + geom_point(alpha = .3, size = 5)+
    facet_grid(diplome13~SEXE)


# Utilisation du contrefactuel rendement du niveau de diplome égal

contr_OD <- EE_OD |> 
    unnest(cols = data) |> 
    nest(data = -c(SEXE, AG10, AN11)) |> 
    mutate(
       w_cadres = map_dbl(data, \(x) wass_cspp(x, niv = "Cadres et professions intellectuelles")),
       m_cadres = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Cadres et professions intellectuelles"])),
       w_pi = map_dbl(data, \(x) wass_cspp(x, niv = "Professions intermédiaires")),
       m_pi = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Professions intermédiaires"])),
       w_emp = map_dbl(data, \(x) wass_cspp(x, niv = "Employés")),
       m_emp = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Employés"])),
       w_ouvrier = map_dbl(data, \(x) wass_cspp(x, niv = "Ouvriers")),
       m_ouvrier = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Ouvriers"])),
       w_cadres_c = map_dbl(data, \(x) wass_cspp(x, niv = "Cadres et professions intellectuelles", var = "logsal_contrefactuel")),
       m_cadres_c = map_dbl(data, \(x) mean(x$logsal_contrefactuel[x$CSPP1 == "Cadres et professions intellectuelles"])),
       w_pi_c = map_dbl(data, \(x) wass_cspp(x, niv = "Professions intermédiaires", var = "logsal_contrefactuel")),
       m_pi_c = map_dbl(data, \(x) mean(x$logsal_contrefactuel[x$CSPP1 == "Professions intermédiaires"])),
       w_emp_c = map_dbl(data, \(x) wass_cspp(x, niv = "Employés", var = "logsal_contrefactuel")),
       m_emp_c = map_dbl(data, \(x) mean(x$logsal_contrefactuel[x$CSPP1 == "Employés"])),
       w_ouvrier_c = map_dbl(data, \(x) wass_cspp(x, niv = "Ouvriers", var = "logsal_contrefactuel")),
       m_ouvrier_c = map_dbl(data, \(x) mean(x$logsal_contrefactuel[x$CSPP1 == "Ouvriers"]))) 

# tests de normalité des logsal
EE_OD |> 
    mutate(skew = map_dbl(data, \(x) moments::skewness(x |> pull(logsal))),
           kurt = map_dbl(data, \(x) moments::kurtosis(x |> pull(logsal))))

# pas si normal que cela.

contr_OD |> 
    filter(SEXE == "Homme") |> 
    lm(w_cadres_c ~ AG10, data = _) |> summary()

contr_OD |> 
    filter(SEXE == "Femme") |> 
    lm(w_cadres_c ~ AG10, data = _) |> summary()

contr_OD |> 
    filter(SEXE == "Homme") |> 
    lm(w_cadres ~ AG10, data = _) |> summary()
contr_OD |> 
    filter(SEXE == "Homme") |> 
    lm(w_pi_c ~ AG10, data = _) |> summary()

contr_OD |> 
    filter(SEXE == "Homme") |> 
    lm((m_cadres - m_ouvrier) ~ AG10, data = _) |> summary()

contr_OD |> 
    filter(SEXE == "Homme", AG10 =="(40,50]") 
contr_OD |> 
    filter(SEXE == "Femme", AG10 =="(40,50]") 


# Et avec un contrefactuel sur les origines dans les même proportions pour chacune des générations.
contr_OD <- contr_OD |> 
    mutate(GEN = map_dbl(data, \(x) first(x$GEN)))

Les_poids_origines <- EE |> 
    filter(CSPP1 != "Inactifs (n'ayant jamais travaillé)") |> 
    count(GEN, CSPP1) |> 
    drop_na() |> 
    group_by(GEN) |> 
    transmute(CSPP1, perc = 100 * n / sum(n)) |> 
    group_by(CSPP1) |> 
    mutate(w_origin = perc[GEN=="1940"] / perc)


EE_origin <- EE |> left_join(Les_poids_origines, by = c("GEN", "CSPP1"))

EE_origin <- EE_origin |> 
    drop_na(diplome7, CSPP1) |> 
    filter(diplome7 != "Non réponse", CSPP1 %in% pcs_list) |> 
    nest(data = -c(SEXE, AN11, AG10, diplome7))


library(Hmisc)  # pour wtd.quantile et wtd.Ecdf

quantile_map_w <- function(source_sal, target_sal, 
                           source_w = NULL, target_w = NULL) {
    
    # Poids uniformes si non fournis
    if (is.null(source_w)) source_w <- rep(1, length(source_sal))
    if (is.null(target_w)) target_w <- rep(1, length(target_sal))
    
    # ECDF pondérée de la source : rang de chaque individu dans sa distribution
    percentiles <- wtd.Ecdf(source_sal, weights = source_w)
    # Interpoler pour obtenir le percentile de chaque valeur source
    perc_source <- approx(percentiles$x, percentiles$ecdf, 
                          xout = source_sal, rule = 2)$y
    perc_source <- pmin(pmax(perc_source, 0.001), 0.999)
    
    # Quantiles pondérés de la cible : à quel salaire correspond ce percentile ?
    wtd.quantile(target_sal, weights = target_w, 
                 probs = perc_source, type = "i/n") |> as.numeric()
}



EE_origin <- EE_origin |> 
    mutate(data = map(data, \(x) {
        sal_bac <- x |> filter(dip7 == "Niveau0") |> pull(logsal)
        if(length(sal_bac) == 0) return(x |> mutate(logsal_contrefactuel = logsal))
        
        mapping_table <- x |> 
            filter(dip7 != "Niveau0") |> 
            group_by(dip7) |> 
            mutate(logsal_contrefactuel = quantile_map(logsal, sal_bac)) |> 
            ungroup() |> 
            select(id, logsal_contrefactuel) 
        
        # 3. Fusionner le résultat avec le tibble d'origine
        x |> 
            left_join(mapping_table, by = "id") |> 
            mutate(logsal_contrefactuel = coalesce(logsal_contrefactuel, logsal))
    }))






res_origin <- EE_origin |> 
    mutate(w_cadres = map_dbl(data, \(x) wass_cspp(x, niv = "Cadres et professions intellectuelles")),
           m_cadres = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Cadres et professions intellectuelles"])),
           sd_cadres = map_dbl(data, \(x) sd(x$logsal[x$CSPP1 == "Cadres et professions intellectuelles"])),
           n_cadres = map_dbl(data, \(x) sum(x$CSPP1 == "Cadres et professions intellectuelles")),
           w_pi = map_dbl(data, \(x) wass_cspp(x, niv = "Professions intermédiaires")),
           m_pi = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Professions intermédiaires"])),
           sd_pi = map_dbl(data, \(x) sd(x$logsal[x$CSPP1 == "Professions intermédiaires"])),
           n_pi = map_dbl(data, \(x) sum(x$CSPP1 == "Professions intermédiaires")),
           w_emp = map_dbl(data, \(x) wass_cspp(x, niv = "Employés")),
           m_emp = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Employés"])),
           sd_emp = map_dbl(data, \(x) sd(x$logsal[x$CSPP1 == "Employés"])),
           n_emp = map_dbl(data, \(x) sum(x$CSPP1 == "Employés")),
           w_ouvrier = map_dbl(data, \(x) wass_cspp(x, niv = "Ouvriers")),
           m_ouvrier = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Ouvriers"])),
           sd_ouvrier = map_dbl(data, \(x) sd(x$logsal[x$CSPP1 == "Ouvriers"])),
           n_ouvrier = map_dbl(data, \(x) sum(x$CSPP1 == "Ouvriers"))) |> 
    mutate(w_cadres_o = map_dbl(data, \(x) wass_cspp(x, niv = "Cadres et professions intellectuelles", wa = TRUE, wb = TRUE)),
           m_cadres_o = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Cadres et professions intellectuelles"])),
           sd_cadres_o = map_dbl(data, \(x) sd(x$logsal[x$CSPP1 == "Cadres et professions intellectuelles"])),
           n_cadres_o = map_dbl(data, \(x) sum(x$CSPP1 == "Cadres et professions intellectuelles")),
           w_pi_o = map_dbl(data, \(x) wass_cspp(x, niv = "Professions intermédiaires", wa = TRUE, wb = TRUE)),
           m_pi_o = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Professions intermédiaires"])),
           sd_pi_o = map_dbl(data, \(x) sd(x$logsal[x$CSPP1 == "Professions intermédiaires"])),
           n_pi_o = map_dbl(data, \(x) sum(x$CSPP1 == "Professions intermédiaires")),
           w_emp_o = map_dbl(data, \(x) wass_cspp(x, niv = "Employés", wa = TRUE, wb = TRUE)),
           m_emp_o = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Employés"])),
           sd_emp_o = map_dbl(data, \(x) sd(x$logsal[x$CSPP1 == "Employés"])),
           n_emp_o = map_dbl(data, \(x) sum(x$CSPP1 == "Employés")),
           w_ouvrier_o = map_dbl(data, \(x) wass_cspp(x, niv = "Ouvriers", wa = TRUE, wb = TRUE)),
           m_ouvrier_o = map_dbl(data, \(x) mean(x$logsal[x$CSPP1 == "Ouvriers"])),
           sd_ouvrier_o = map_dbl(data, \(x) sd(x$logsal[x$CSPP1 == "Ouvriers"])),
           n_ouvrier_o = map_dbl(data, \(x) sum(x$CSPP1 == "Ouvriers")))
    


