## R/scenarios.R — paramètres, impacts, exécutions et tables


#SOURCE : réalisé au niveau du qmd SINON réactiver ce qui suit
#source(file.path("R","modele_funs.R"), local = TRUE)

# --- Paramètres Référence ---
params <- list(
  CU_RA = 0.20, MO_RA_AF = 0.03, IVV = 378, MO_RA_Y = 0.08,
  LA_VAL_AF = 2400, LA_VAL_J = 1350,
  cout_alim = 306, cout_elevage = 306,
  COST_VET_D = 0,
  SH_REDIRECT = 0.0, DPRICE_ABS = 0.0,
  SH_BLOCK = 0.0, C_DAY_BRD = 0.0, D_BLOCK = 0,
  SH_PCR = 0.0, C_PCR = 0.0
)

# --- Impacts MHE (additifs) ---
impacts_mhe <- list(
  dMO_RA_AF = 0.015,
  dMO_RA_Y  = 0.02,
  dIVV      = 21,
  dCU_RA    = 0.05
)

# --- Scénario MHE ---
params_mhe <- apply_impacts(params, impacts_mhe)
params_mhe$COST_VET_D <- 15   # exemple de surcoût à la vache
params_mhe$SH_PCR     <- 0
params_mhe$C_PCR      <- 25

# --- Exécution ---
res_ref <- run_scenario(params)
res_mhe <- run_scenario(params_mhe)

# --- Tables analyses pour graphiques ---
build_step_df <- function(res, scenario_label){
  data.frame(
    step  = c("REV_AF","REV_Y","COST_PROD","COST_VET_D","COST_RESTR","MB_op"),
    value = c(res$breakdown$REV_AF, res$breakdown$REV_Y,
              -res$breakdown$COST_PROD, -res$breakdown$COST_VET_D,
              -res$breakdown$COST_RESTR,  res$MB_op),
    scenario = scenario_label, stringsAsFactors = FALSE
  )
}
sd_ref <- build_step_df(res_ref, "Référence")
sd_mhe <- build_step_df(res_mhe, "MHE")
sd_all <- rbind(sd_ref, sd_mhe)
sd_all$type <- ifelse(sd_all$step %in% c("REV_AF","REV_Y"), "Revenus",
                      ifelse(sd_all$step %in% c("COST_PROD","COST_VET_D","COST_RESTR"), "Coûts", "Marge"))

# --- Table synthèse 'df' (réf vs mhe) ---
df <- rbind(
  to_row("TR_eff",     res_ref$breakdown$TR_eff,     res_mhe$breakdown$TR_eff),
  to_row("n_weaned",   res_ref$breakdown$n_weaned,   res_mhe$breakdown$n_weaned),
  to_row("n_brd_sold", res_ref$breakdown$n_brd_sold, res_mhe$breakdown$n_brd_sold),
  to_row("REV_AF",     res_ref$breakdown$REV_AF,     res_mhe$breakdown$REV_AF),
  to_row("REV_Y",      res_ref$breakdown$REV_Y,      res_mhe$breakdown$REV_Y),
  to_row("COST_PROD",  res_ref$breakdown$COST_PROD,  res_mhe$breakdown$COST_PROD),
  to_row("COST_VET_D", res_ref$breakdown$COST_VET_D, res_mhe$breakdown$COST_VET_D),
  to_row("COST_RESTR", res_ref$breakdown$COST_RESTR, res_mhe$breakdown$COST_RESTR),
  to_row("MB_op",      res_ref$MB_op,                res_mhe$MB_op)
); df$block <- "results"

# --- Rappel paramètres (fusion réf/mhe) ---
df_params_ref <- params_to_df(params, "params_ref")
df_params_mhe <- params_to_df_mhe(params_mhe, "params_mhe")
params_all <- merge(df_params_ref, df_params_mhe, by = c("item","block"), all = TRUE)
