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

# Les log sal moyen
EE_mean <- EE |> 
    group_by(SEXE, AN11, AG10, CSTOT1) |> 
    summarise(mean_logsal = mean(logsal, na.rm = TRUE),
              sd_logsal = sd(logsal, na.rm = TRUE), .groups = "drop")



EE <- EE |> mutate(PCS1 = fct_recode(CSTOT1, !!!csp_origin))

#
EE_dip <- EE |> 
    filter(str_detect(dip7,"Niveau")) |> 
    nest(data = -c(SEXE, AN11, AG10, dip7))

EE_dip |> 
    mutate(n_cadres = map_int(data, \(x) x |> filter(PCS1 == "Cadre") |> nrow()),
           n_ouvriers = map_int(data, \(x) x |> filter(PCS1 == "Ouvrier") |> nrow()))


EE_dippcs <- EE |> 
    filter(str_detect(dip7,"Niveau"), PCS1 %in% c("Cadre", "Pinter", "Employé", "Ouvrier")) |> 
    nest(data = -c(SEXE, AN11, AG10, dip7, PCS1))

EE_dippcs |> mutate(n = map_int(data, nrow)) |> view()


wass_boot <- function(data, indices, ori, var = "logsal") {
    d <- data[indices, ]
    x <- d |> filter(Origin == ori) |> pull(.data[[var]])
    y <- d |> filter(Origin == "Ouvrier") |> pull(.data[[var]])
    if (length(x) < 2 | length(y) < 2) return(NA)
    wasserstein1d(x, y, p = 2)
}

wass_safe <- function(a, b, p = 2) {
    if (length(a) < 2 | length(b) < 2) return(NA_real_)
    wasserstein1d(a, b, p = p)
}

wass_fct <- function(df_list, var = "logsal", R = 100) {
    
    les_estimations <- reduce(c("Cadre", "Pinter", "Employé"), \(df, ori) {
        df |> mutate(
            "w_{ori}"  := map_dbl(df_list, \(x) wass_safe(
                a = x |> filter(Origin == ori)       |> pull(.data[[var]]),
                b = x |> filter(Origin == "Ouvrier") |> pull(.data[[var]])
            )),
            "m_{ori}"  := map_dbl(df_list, \(x) mean(x[[var]][x$Origin == ori],       na.rm = TRUE)),
            "sd_{ori}" := map_dbl(df_list, \(x) sd(x[[var]][x$Origin == ori],         na.rm = TRUE))
        )
    }, .init = tibble(data = df_list)) |> 
        mutate(
            m_Ouvrier  = map_dbl(df_list, \(x) mean(x[[var]][x$Origin == "Ouvrier"], na.rm = TRUE)),
            sd_Ouvrier = map_dbl(df_list, \(x) sd(x[[var]][x$Origin == "Ouvrier"],  na.rm = TRUE))
        )
    
    les_boots <- reduce(c("Cadre", "Pinter", "Employé"), \(df, ori) {
        df |> mutate(
            "boot_{ori}" := map(df_list, \(x) {
                # Vérifier que les deux groupes ont assez d'observations
                n_ori <- sum(x$Origin == ori,       na.rm = TRUE)
                n_ouv <- sum(x$Origin == "Ouvrier", na.rm = TRUE)
                if (n_ori < 2 | n_ouv < 2) return(NULL)
                boot(data = x,
                     statistic = \(d, i) wass_boot(d, i, ori = ori, var = var),
                     R = R)
            })
        )
    }, .init = tibble(data = df_list))
    
    les_ic <- les_boots |> 
        transmute(across(
            .cols  = starts_with("boot_"),
            .fns   = \(x) map(x, \(y) {
                if (is.null(y)) return(c(NA_real_, NA_real_))  # cellule vide
                ci <- boot.ci(y, type = "perc")
                if (is.null(ci)) return(c(NA_real_, NA_real_))  # boot.ci peut aussi échouer
                ci$percent[4:5]
            }),
            .names = "{.col}_ci"
        )) |> 
        unnest_wider(col = ends_with("_ci"), names_sep = "_")
    
    bind_cols(les_estimations, les_ic) |> select(-data)
}

EE_dippcs <- EE_dippcs |> 
    bind_cols(wass_fct(EE_dippcs$data, R=100))

EE_dippcs |> 
    select(SEXE, AG10, AN11, dip7, PCS1, w_Cadre) |> 
    mutate(dip7 = ordered(dip7), PCS1 = ordered(PCS1)) |> 
    lm(w_Cadre ~ SEXE + AG10 + AN11 + dip7 + PCS1, data = _) |> 
    summary()


# # Analyse avec données manquantes non aléatoires
# library(visdat)
# 
# EE_dippcs |> 
#     mutate(dip7 = ordered(dip7), PCS1 = ordered(PCS1)) |> 
#     mutate(n_cadre = map_int(data, \(x) sum(x$Origin == "Cadre")),
#            n_ouv   = map_int(data, \(x) sum(x$Origin == "Ouvrier")),
#            n_min   = pmin(n_cadre, n_ouv)) |> 
#     drop_na(w_Cadre) |> 
#     lm(w_Cadre ~ SEXE + AG10 + AN11 + dip7 + PCS1, 
#        data    = _, 
#        weights = n_min) |> 
#     summary()
# 
# library(naniar)
# library(misty)
# 
# # Créer indicateur binaire de manquant
# df_reg <- EE_dippcs |> 
#     select(SEXE, AG10, AN11, dip7, PCS1, w_Cadre) |> 
#     mutate(manquant = as.integer(is.na(w_Cadre)))
# 
# # Est-ce que les manquants dépendent des covariables ?
# glm(manquant ~ SEXE + AG10 + AN11 + dip7 + PCS1, 
#     data   = df_reg, 
#     family = binomial) |> 
#     summary()
# # Si des coefficients sont significatifs -> MAR ou MNAR, pas MCAR
# library(mice)
# 
# df_reg <- EE_dippcs |> 
#     select(SEXE, AG10, AN11, dip7, PCS1, w_Cadre)
# 
# # Imputation (méthode pmm = predictive mean matching, adaptée aux variables continues)
# imp <- mice(df_reg, m = 20, method = "pmm", seed = 42, printFlag = FALSE)
# 
# # Régression sur chaque jeu imputé + pooling de Rubin
# fit <- with(imp, lm(w_Cadre ~ SEXE + AG10 + AN11 + dip7 + PCS1))
# pool(fit) |> summary()
# 
# df_reg2 <- EE_dippcs |> 
#     mutate(dip7 = ordered(dip7), PCS1 = ordered(PCS1)) |> 
#     select(SEXE, AG10, AN11, dip7, PCS1, w_Cadre)
# 
# imp2 <- mice(df_reg2, m = 20, method = "pmm", seed = 42, printFlag = FALSE)
# fit2 <- with(imp2, lm(w_Cadre ~ SEXE + AG10 + AN11 + dip7 + PCS1))
# pool(fit2) |> summary()


# calcul de l'échantillon min
# Vérifier que n_min prédit bien les manquants
EE_dippcs |> 
    mutate(
        n_min   = map_int(data, \(x) min(sum(x$Origin == "Cadre"),
                                         sum(x$Origin == "Ouvrier"))),
        observé = as.integer(!is.na(w_Cadre))
    ) |> 
    glm(observé ~ n_min, data = _, family = binomial) |> 
    summary()


df_reg <- EE_dippcs |> 
    mutate(dip7 = ordered(dip7), PCS1 = ordered(PCS1)) |> 
    mutate(
        n_cadre = map_int(data, \(x) sum(x$Origin == "Cadre")),
        n_ouv   = map_int(data, \(x) sum(x$Origin == "Ouvrier")),
        n_min   = pmin(n_cadre, n_ouv)
    ) |> 
    select(SEXE, AG10, AN11, dip7, PCS1, w_Cadre, n_min)


# Méthode pmm avec n_min comme prédicteur de l'imputation
# mais exclu de la régression finale
library(mice)
imp_informed <- mice(
    df_reg, 
    m           = 20, 
    method      = "pmm", 
    seed        = 42,
    predictorMatrix = {
        pm <- make.predictorMatrix(df_reg)
        # n_min prédit w_Cadre dans l'imputation...
        pm["w_Cadre", "n_min"] <- 1
        # ...mais w_Cadre ne prédit pas n_min
        pm["n_min", ] <- 0
        pm
    },
    printFlag = FALSE
)

# Régression finale sans n_min (variable auxiliaire uniquement)
fit_informed <- with(imp_informed, 
                     lm(w_Cadre ~ SEXE + AG10 + AN11 + dip7 + PCS1))
pool(fit_informed) |> summary() |> arrange(p.value)

# Régression pondérée sur les cellules observées
fit_pondere <- df_reg |> 
    filter(!is.na(w_Cadre)) |> 
    lm(w_Cadre ~ SEXE + AG10 + AN11 + dip7 + PCS1,
       data    = _,
       weights = n_min)   # les grandes cellules comptent plus

summary(fit_pondere)

library(lavaan)
model <- '
  dip7  ~ Origin + SEXE + GEN + AN11
  PCS1 ~ Origin + dip7 + SEXE + GEN + AN11
  logsal  ~ Origin + dip7 + PCS1 + SEXE + GEN + AN11
'
fit <- sem(model, data = EE, estimator = "ML")

EE_ordered <- EE |> 
    drop_na(dip7, logsal, PCS1, Origin, GEN) |> 
    filter(PCS1 != "ACCE", dip7 != "Non réponse") |> 
    mutate(dip7 = ordered(dip7) |> fct_drop() |> fct_rev(),
           GEN = ordered(GEN),
           PCS1 = fct_recode(PCS1, EmpOuv = "Employé", EmpOuv = "Ouvrier") |> ordered() |> fct_rev(),
           Origin_Cadre = as.integer(Origin == "Cadre"),
           Origin_Ouvrier = as.integer(Origin == "Ouvrier"), 
           Origin_Employé = as.integer(Origin == "Employé"),
           Origin_Pinter = as.integer(Origin == "Pinter"),
           Origin_Agri = as.integer(Origin == "Agriculteur"))

model2 <- '
  dip7  ~ Origin_Cadre + Origin_Ouvrier + Origin_Employé + Origin_Pinter + Origin_Agri + SEXE + GEN + AN11
  PCS1 ~ Origin_Cadre + Origin_Ouvrier + Origin_Employé + Origin_Pinter + Origin_Agri + dip7 + SEXE + GEN + AN11
  logsal  ~ Origin_Cadre + Origin_Ouvrier + Origin_Employé + Origin_Pinter + Origin_Agri + dip7 + PCS1 + SEXE + GEN + AN11
'
fit2 <- sem(model2, data = EE_ordered, estimator = "WLSMV")
summary(fit2, standardized = TRUE)


model3 <- '
  dip7  ~ a1*Origin_Cadre + a2*Origin_Ouvrier + SEXE + GEN + AN11
  PCS1  ~ b1*Origin_Cadre + b2*Origin_Ouvrier + c*dip7 + SEXE + GEN + AN11
  logsal ~ d1*Origin_Cadre + d2*Origin_Ouvrier + e*dip7 + f*PCS1 + SEXE + GEN + AN11

  # Effets indirects de Origin_Cadre sur salaire
  ind_cadre_via_dip     := a1*e
  ind_cadre_via_pcs     := b1*f
  ind_cadre_via_dip_pcs := a1*c*f
  total_indirect_cadre  := a1*e + b1*f + a1*c*f
  total_cadre           := d1 + a1*e + b1*f + a1*c*f

  # Idem Origin_Ouvrier
  ind_ouvrier_via_dip     := a2*e
  ind_ouvrier_via_pcs     := b2*f
  ind_ouvrier_via_dip_pcs := a2*c*f
  total_indirect_ouvrier  := a2*e + b2*f + a2*c*f
  total_ouvrier           := d2 + a2*e + b2*f + a2*c*f
'

fit3 <- sem(model3, data = EE_ordered, estimator = "WLSMV")
summary(fit3, standardized = TRUE)

decomp <- tibble(
    chemin = c("Direct", "Via diplôme", "Via PCS", "Via dip→PCS"),
    cadre   = c(-0.024, 0.008, 0.034, 0.111),
    ouvrier = c(0.053, -0.008, -0.047, -0.111)
)

decomp |>
    pivot_longer(-chemin, names_to = "origine", values_to = "effet") |>
    ggplot(aes(x = chemin, y = effet, fill = origine)) +
    geom_col(position = "dodge") +
    geom_hline(yintercept = 0) +
    labs(title = "Décomposition de l'effet de l'origine sociale sur le salaire",
         y = "Effet standardisé", x = "")

library(lavaanPlot)
lavaanPlot(model = fit2, 
           node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"),
           coefs = TRUE,        # affiche les coefficients
           stand = TRUE,        # coefficients standardisés
           sig = 0.05)          # seulement les liens significatifs

# Par période
#
EE_ordered <- EE_ordered |>
  mutate(
    Cadre_GEN = Origin_Cadre * as.numeric(GEN),
    Ouvrier_GEN = Origin_Ouvrier * as.numeric(GEN)
  )

model3_groupGEN <- '
  dip7  ~ a1*Origin_Cadre + a2*Origin_Ouvrier + SEXE + AN11
  PCS1  ~ b1*Origin_Cadre + b2*Origin_Ouvrier + c*dip7 + SEXE + AN11
  logsal ~ d1*Origin_Cadre + d2*Origin_Ouvrier + e*dip7 + f*PCS1 + SEXE + AN11

  ind_cadre_via_dip     := a1*e
  ind_cadre_via_pcs     := b1*f
  ind_cadre_via_dip_pcs := a1*c*f
  total_indirect_cadre  := a1*e + b1*f + a1*c*f
  total_cadre           := d1 + a1*e + b1*f + a1*c*f

  ind_ouvrier_via_dip     := a2*e
  ind_ouvrier_via_pcs     := b2*f
  ind_ouvrier_via_dip_pcs := a2*c*f
  total_indirect_ouvrier  := a2*e + b2*f + a2*c*f
  total_ouvrier           := d2 + a2*e + b2*f + a2*c*f
'

gens_ok <- EE_ordered |>
  group_by(GEN) |>
  filter(n_distinct(AN11) > 1) |>
  ungroup()

fit_gen <- sem(
  model3_groupGEN,
  data = gens_ok,
  group = "GEN",
  estimator = "WLSMV"
)

summary(fit_gen, standardized = TRUE)


fit_gen <- sem(
  model3,
  data = EE_ordered,
  group = "GEN",
  estimator = "WLSMV"
)


model_gen_free_simple <- '
  dip7  ~ Origin_Cadre + Origin_Ouvrier + SEXE + AN11
  PCS1  ~ Origin_Cadre + Origin_Ouvrier + dip7 + SEXE + AN11
  logsal ~ Origin_Cadre + Origin_Ouvrier + dip7 + PCS1 + SEXE + AN11
'

fit_gen_free <- sem(
  model_gen_free_simple,
  data = gens_ok,
  group = "GEN",
  estimator = "WLSMV"
)

summary(fit_gen_free, standardized = TRUE)



