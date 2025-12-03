## R/modele_funs.R  — moteur pur (pas d’IO)

# --- Fonctions cœur ---
n_brd_sold_fun <- function(n_weaned, TR_eff){
  if (TR_eff > n_weaned) warning("TR_eff > n_weaned : vérifier les entrées.")
  n_weaned - TR_eff
}

REV_AF    <- function(CU_RA, LA_VAL_AF) CU_RA * LA_VAL_AF
REV_Y     <- function(n_brd_sold, LA_VAL_J) n_brd_sold * LA_VAL_J
COST_PROD <- function(cout_alim, cout_elevage) cout_alim + cout_elevage

COST_PRIX <- function(n_brd_sold, sh_redirect, dprice_abs) n_brd_sold * sh_redirect * dprice_abs
COST_BLOC <- function(n_brd_sold, sh_block, c_day_brd, d_block) n_brd_sold * sh_block * c_day_brd * d_block
COST_PCR  <- function(n_brd_sold, sh_pcr, c_pcr) n_brd_sold * sh_pcr * c_pcr

run_scenario <- function(p){
  TR_eff     <- p$CU_RA + p$MO_RA_AF
  IVV_years  <- p$IVV / 365
  n_weaned   <- (1 / IVV_years) * (1 - p$MO_RA_Y)
  n_brd_sold <- n_brd_sold_fun(n_weaned, TR_eff)
  
  REV_AF_val     <- REV_AF(p$CU_RA, p$LA_VAL_AF)
  REV_Y_val      <- REV_Y(n_brd_sold, p$LA_VAL_J)
  COST_PROD_val  <- COST_PROD(p$cout_alim, p$cout_elevage)
  COST_RESTR_val <- COST_PRIX(n_brd_sold, p$SH_REDIRECT, p$DPRICE_ABS) +
    COST_BLOC(n_brd_sold, p$SH_BLOCK, p$C_DAY_BRD, p$D_BLOCK) +
    COST_PCR(n_brd_sold, p$SH_PCR, p$C_PCR)
  
  MB_op <- (REV_AF_val + REV_Y_val) - (COST_PROD_val + p$COST_VET_D + COST_RESTR_val)
  
  list(
    MB_op = MB_op,
    breakdown = list(
      CU_RA = p$CU_RA, MO_RA_AF = p$MO_RA_AF, MO_RA_Y = p$MO_RA_Y,
      TR_eff = TR_eff, IVV = p$IVV, n_weaned = n_weaned, n_brd_sold = n_brd_sold,
      REV_AF = REV_AF_val, REV_Y = REV_Y_val,
      COST_PROD = COST_PROD_val, COST_VET_D = p$COST_VET_D, COST_RESTR = COST_RESTR_val
    )
  )
}

# --- Conventions d’impacts (tous ADDITIFS ici) ---
# IVV (jours), MO_RA_Y (points), MO_RA_AF (points), CU_RA (points)
apply_impacts <- function(p, impacts, sequential = TRUE) {
  if (is.null(impacts)) return(p)
  p_eff <- p
  base_val <- function(current, ref) if (sequential) current else ref
  
  if (!is.null(impacts$dIVV) && !is.null(p$IVV))
    p_eff$IVV <- base_val(p_eff$IVV, p$IVV) + impacts$dIVV
  
  if (!is.null(impacts$dMO_RA_Y))
    p_eff$MO_RA_Y <- base_val(p_eff$MO_RA_Y, p$MO_RA_Y) + impacts$dMO_RA_Y
  
  if (!is.null(impacts$dCU_RA))
    p_eff$CU_RA <- base_val(p_eff$CU_RA, p$CU_RA) + impacts$dCU_RA
  
  if (!is.null(impacts$dMO_RA_AF))
    p_eff$MO_RA_AF <- base_val(p_eff$MO_RA_AF, p$MO_RA_AF) + impacts$dMO_RA_AF
  
  # garde-fous
  if (!is.null(p_eff$MO_RA_Y))  p_eff$MO_RA_Y  <- min(max(p_eff$MO_RA_Y, 0), 1)
  if (!is.null(p_eff$CU_RA))    p_eff$CU_RA    <- max(p_eff$CU_RA, 0)
  if (!is.null(p_eff$MO_RA_AF)) p_eff$MO_RA_AF <- max(p_eff$MO_RA_AF, 0)
  
  p_eff
}

# --- Utilitaires tables ---
to_row <- function(name, ref, mhe) data.frame(item = name, ref = ref, mhe = mhe, delta = mhe - ref)
params_to_df <- function(lst, label){
  data.frame(item = names(lst), ref = unlist(lst), mhe = NA, delta = NA, block = label, stringsAsFactors = FALSE)
}
params_to_df_mhe <- function(lst, label){
  data.frame(item = names(lst), ref = NA, mhe = unlist(lst), delta = NA, block = label, stringsAsFactors = FALSE)
}
