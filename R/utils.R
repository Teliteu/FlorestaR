# ============================================================
# utils.R - Helper Functions
# ============================================================

# ============================================================
# 1. SPATIAL GRID FUNCTION (For 3D Visualization)
# ============================================================
#' Transforms local plot coordinates into a global spatial grid
#' @export
transformar_para_grid <- function(df_resultado) {

  require(dplyr)

  u_parc <- unique(df_resultado$Parcela)
  n_parc <- length(u_parc)

  # If there is only one plot, no grid translation is needed
  if(n_parc <= 1) return(df_resultado)

  n_cols <- ceiling(sqrt(n_parc))

  # Use the first plot as a reference to calculate bounding box dimensions
  df_ref <- df_resultado %>% dplyr::filter(Parcela == u_parc[1])
  width  <- max(df_ref$X, na.rm=T) - min(df_ref$X, na.rm=T)
  height <- max(df_ref$Y, na.rm=T) - min(df_ref$Y, na.rm=T)

  # Fallback for empty or 1D plots
  if(width <= 0 || is.infinite(width)) width <- 100
  if(height <= 0 || is.infinite(height)) height <- 100

  step_x <- width
  step_y <- height

  res <- df_resultado %>%
    dplyr::group_by(Parcela) %>%
    dplyr::mutate(
      idx_seq = dplyr::cur_group_id() - 1,
      col_grid = idx_seq %% n_cols,
      row_grid = floor(idx_seq / n_cols),
      # Translate local coordinates to the new global grid slot
      X_Global = (X - min(X, na.rm=T)) + (col_grid * step_x),
      Y_Global = (Y - min(Y, na.rm=T)) + (row_grid * step_y)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(X = X_Global, Y = Y_Global) %>%
    dplyr::select(-idx_seq, -col_grid, -row_grid, -X_Global, -Y_Global)

  return(res)
}

# ============================================================
# 2. FINAL REPORT EXPORTER (Wide/Excel-like Format)
# ============================================================
gerar_tabela_final_larga <- function(df_hist) {
  require(dplyr)
  require(tidyr)

  message(">>> Generating Final Table (One row per tree)...")

  # Extract static tree attributes based on their first recorded state
  base <- df_hist %>%
    dplyr::group_by(i_arbre) %>%
    dplyr::summarise(
      Specie = dplyr::first(specie),
      Parcela = dplyr::first(Parcela),
      Grupo = dplyr::first(GrEc_xx),
      X_Original = dplyr::first(X),
      Y_Original = dplyr::first(Y),
      Status_Final = dplyr::last(status),
      # Identify the exact year a tree died or was harvested
      Ano_Evento = ifelse(all(status == "viva"), "Survivor", as.character(min(Ano[status != "viva"]))),
      .groups = "drop"
    )

  # Pivot DBH (DAP) history into a wide format (one column per year)
  daps_wide <- df_hist %>%
    dplyr::group_by(i_arbre) %>%
    dplyr::mutate(
      # Keep DBH for the exact year of death (to render stumps), but set to NA for subsequent years
      DAP_Visual = ifelse(cumany(status != "viva" & dplyr::lag(status, default="viva") != "viva"), NA, DAP)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(i_arbre, Ano, DAP_Visual) %>%
    tidyr::pivot_wider(names_from = Ano, values_from = DAP_Visual, names_prefix = "DAP_Ano_")

  # Merge static traits with the wide time-series data
  return(dplyr::left_join(base, daps_wide, by = "i_arbre"))
}
