source("nouvelles_variables.R")
library(transport)
library(WRS2)
library(boot)
library(viridis)

# Les log sal moyen
EE_mean <- EE |> 
    group_by(SEXE, ANby5, AG5, CSTOT1) |> 
    summarise(mean_logsal = mean(logsal, na.rm = TRUE),
              sd_logsal = sd(logsal, na.rm = TRUE), .groups = "drop")

# Equations structurelles
library(lavaan)

EE_ordered <- EE |> 
    drop_na(dip7, logsal, PCS1, Origin, gen5) |> 
    filter(PCS1 != "ACCE", dip7 != "Non réponse") |> 
    mutate(dip7 = ordered(dip7) |> fct_drop() |> fct_rev(),
           PCS1 = fct_recode(PCS1, EmpOuv = "Employé", EmpOuv = "Ouvrier") |> ordered() |> fct_rev(),
           Origin_Cadre = as.integer(Origin == "Cadre"),
           Origin_Ouvrier = as.integer(Origin == "Ouvrier"), 
           Origin_Employé = as.integer(Origin == "Employé"),
           Origin_Pinter = as.integer(Origin == "Pinter"),
           Origin_Agri = as.integer(Origin == "Agriculteur"),
           gen5 = factor(gen5, ordered = FALSE),
           ANby5 = factor(ANby5, ordered = FALSE))

X <- model.matrix(~ gen5 + ANby5, data = EE_ordered)[, -1]

colnames(X) <- make.names(colnames(X))

EE_ordered_mm <- bind_cols(
    EE_ordered,
    as_tibble(X)
)

dummy_vars <- colnames(X)
controls <- paste(c("SEXE", dummy_vars), collapse = " + ")

model_a <- paste0('
  dip7  ~ a1*Origin_Cadre + a2*Origin_Ouvrier + ', controls, '
  PCS1  ~ b1*Origin_Cadre + b2*Origin_Ouvrier + c*dip7 + ', controls, '
  logsal ~ d1*Origin_Cadre + d2*Origin_Ouvrier + e*dip7 + f*PCS1 + ', controls, '

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
')

fit_a <- sem(model_a, data = EE_ordered_mm, estimator = "WLSMV")

summary(fit_a, standardized = TRUE)

"On voit clairement que le diplome se massifie au cours des générations et, en même temps, 
que la rentabilité du diplome pour l'accès PCS diminue.
On a clairement Origin -> dip7 -> PCS1 -> logsal
Les femmes sont désavantagés dans la rentabilité du diplome et le salaire.
l'effet direct de l'origine sur le salaire montre une atténuation dans le cumul des effets."

# Même chose mais en faisant une analyse par SEXE
controls_bis <- paste(dummy_vars, collapse = " + ")
model_a_par_sexe <- paste0('
  dip7  ~ a1*Origin_Cadre + a2*Origin_Ouvrier + ', controls_bis, '
  PCS1  ~ b1*Origin_Cadre + b2*Origin_Ouvrier + c*dip7 + ', controls_bis, '
  logsal ~ d1*Origin_Cadre + d2*Origin_Ouvrier + e*dip7 + f*PCS1 + ', controls_bis, '

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
')

fit_a_H <- sem(model_a_par_sexe, data = EE_ordered_mm |> filter(SEXE == "Homme"), estimator = "WLSMV")
summary(fit_a_H, standardized = TRUE)

fit_a_F <- sem(model_a_par_sexe, data = EE_ordered_mm |> filter(SEXE == "Femme"), estimator = "WLSMV")
summary(fit_a_F, standardized = TRUE)

"La séparation par sexe montre que les mécanismes de reproduction sociale sont globalement similaires chez les hommes et chez les femmes, mais qu’ils ne passent pas exactement par les mêmes canaux. Chez les hommes, l’origine sociale conserve un effet direct plus marqué sur la PCS atteinte. Chez les femmes, les trajectoires apparaissent davantage médiées par le diplôme : le diplôme pèse plus fortement sur l’accès à la PCS et le désavantage d’origine ouvrière se transmet plus fortement par la chaîne diplôme-PCS-salaire. Les générations féminines récentes se distinguent par une forte élévation du niveau de diplôme, sans que cela se traduise mécaniquement par un accès équivalent aux positions sociales les plus favorables."

# Test comparant Homme Femme
model_sexe_free <- paste0('
  dip7  ~ c(a1_H, a1_F)*Origin_Cadre
        + c(a2_H, a2_F)*Origin_Ouvrier
        + ', controls_bis, '

  PCS1  ~ c(b1_H, b1_F)*Origin_Cadre
        + c(b2_H, b2_F)*Origin_Ouvrier
        + c(c_H, c_F)*dip7
        + ', controls_bis, '

  logsal ~ c(d1_H, d1_F)*Origin_Cadre
         + c(d2_H, d2_F)*Origin_Ouvrier
         + c(e_H, e_F)*dip7
         + c(f_H, f_F)*PCS1
         + ', controls_bis, '

  # Effets indirects hommes
  ind_cadre_via_dip_H     := a1_H*e_H
  ind_cadre_via_pcs_H     := b1_H*f_H
  ind_cadre_via_dip_pcs_H := a1_H*c_H*f_H
  total_indirect_cadre_H  := a1_H*e_H + b1_H*f_H + a1_H*c_H*f_H
  total_cadre_H           := d1_H + a1_H*e_H + b1_H*f_H + a1_H*c_H*f_H

  ind_ouvrier_via_dip_H     := a2_H*e_H
  ind_ouvrier_via_pcs_H     := b2_H*f_H
  ind_ouvrier_via_dip_pcs_H := a2_H*c_H*f_H
  total_indirect_ouvrier_H  := a2_H*e_H + b2_H*f_H + a2_H*c_H*f_H
  total_ouvrier_H           := d2_H + a2_H*e_H + b2_H*f_H + a2_H*c_H*f_H

  # Effets indirects femmes
  ind_cadre_via_dip_F     := a1_F*e_F
  ind_cadre_via_pcs_F     := b1_F*f_F
  ind_cadre_via_dip_pcs_F := a1_F*c_F*f_F
  total_indirect_cadre_F  := a1_F*e_F + b1_F*f_F + a1_F*c_F*f_F
  total_cadre_F           := d1_F + a1_F*e_F + b1_F*f_F + a1_F*c_F*f_F

  ind_ouvrier_via_dip_F     := a2_F*e_F
  ind_ouvrier_via_pcs_F     := b2_F*f_F
  ind_ouvrier_via_dip_pcs_F := a2_F*c_F*f_F
  total_indirect_ouvrier_F  := a2_F*e_F + b2_F*f_F + a2_F*c_F*f_F
  total_ouvrier_F           := d2_F + a2_F*e_F + b2_F*f_F + a2_F*c_F*f_F

  # Différences femmes - hommes
  diff_a1 := a1_F - a1_H
  diff_a2 := a2_F - a2_H
  diff_b1 := b1_F - b1_H
  diff_b2 := b2_F - b2_H
  diff_c  := c_F  - c_H
  diff_d1 := d1_F - d1_H
  diff_d2 := d2_F - d2_H
  diff_e  := e_F  - e_H
  diff_f  := f_F  - f_H

  diff_total_cadre  := total_cadre_F - total_cadre_H
  diff_total_ouvrier := total_ouvrier_F - total_ouvrier_H
')

fit_sexe_free <- sem(
  model_sexe_free,
  data = EE_ordered_mm,
  group = "SEXE",
  estimator = "WLSMV"
)

summary(fit_sexe_free, standardized = TRUE)

model_sexe_equal_paths <- paste0('
  dip7  ~ a1*Origin_Cadre
        + a2*Origin_Ouvrier
        + ', controls_bis, '

  PCS1  ~ b1*Origin_Cadre
        + b2*Origin_Ouvrier
        + c*dip7
        + ', controls_bis, '

  logsal ~ d1*Origin_Cadre
         + d2*Origin_Ouvrier
         + e*dip7
         + f*PCS1
         + ', controls_bis, '
')

fit_sexe_equal_paths <- sem(
  model_sexe_equal_paths,
  data = EE_ordered_mm,
  group = "SEXE",
  estimator = "WLSMV"
)

lavTestLRT(fit_sexe_equal_paths, fit_sexe_free)

lavTestWald(fit_sexe_free, constraints = "a1_H == a1_F")
lavTestWald(fit_sexe_free, constraints = "a2_H == a2_F")
lavTestWald(fit_sexe_free, constraints = "b1_H == b1_F")
lavTestWald(fit_sexe_free, constraints = "b2_H == b2_F")
lavTestWald(fit_sexe_free, constraints = "c_H == c_F")
lavTestWald(fit_sexe_free, constraints = "e_H == e_F")
lavTestWald(fit_sexe_free, constraints = "f_H == f_F")
lavTestWald(fit_sexe_free, constraints = "total_cadre_H == total_cadre_F")
lavTestWald(fit_sexe_free, constraints = "total_ouvrier_H == total_ouvrier_F")

"Commentaire Codex : La comparaison multi-groupes confirme que les mécanismes de reproduction sociale diffèrent significativement selon le sexe. Le modèle contraint, imposant l’égalité des principaux coefficients entre hommes et femmes, est très fortement rejeté. Les tests de Wald montrent que les effets de l’origine sociale sur le diplôme et la PCS, ainsi que les effets du diplôme et de la PCS sur le salaire, diffèrent tous significativement entre les deux groupes. Substantiellement, l’avantage associé à une origine cadre apparaît plus important chez les hommes, tandis que le désavantage associé à une origine ouvrière est plus marqué chez les femmes. Chez ces dernières, les trajectoires semblent davantage médiées par le diplôme, ce qui suggère que la certification scolaire joue un rôle particulièrement structurant dans l’accès aux positions sociales et salariales."
