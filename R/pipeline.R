#' Main Forest Project Execution Function
#'
#' @param arq_csv Character. Path to the forest inventory CSV.
#' @param arq_rds Character. Path to the trained model RDS.
#' @param pasta_saida Character. Output folder name.
#' @param area_total Numeric. Total area in hectares.
#' @param anos_simulacao Numeric. Number of years to simulate.
#' @param gerar_animacao Logical. Whether to generate the 3D GIF.
#' @export
executar_projeto_florestal <- function(arq_csv, arq_rds, pasta_saida = "Resultados_Projeto",
                                       area_total = 18, anos_simulacao = 18, gerar_animacao = FALSE) {

  require(dplyr)
  require(readr)

  # Collateral damage ratio based on the training plots (2.25 dead trees per cut tree)
  TX_DANO_POR_ARVORE <- 2.25

  # Ensure output directory exists
  if (!dir.exists(pasta_saida)) dir.create(pasta_saida)

  # 1. Data Loading & Initialization
  message(">>> Loading data...")

  # Load the trained neural network model
  mod <- readRDS(arq_rds)

  # Load and prep the forest inventory data
  df <- readr::read_csv(arq_csv, show_col_types = FALSE) %>%
    mutate(
      i_arbre = as.character(i_arbre),
      status = "viva",
      Ano = 0,
      DAP = as.numeric(DAP),
      DAP_inicial = as.numeric(DAP)
    )

  # SNAPSHOT: Save the pristine forest state before any logging happens.
  df_pre_corte <- df

  # 1.1 Calculate Ecological Group Proportions (For Recruitment)
  message(">>> Calculating ecological group probabilities...")
  tb_prob <- df %>%
    filter(!is.na(GrEc_xx)) %>%
    group_by(GrEc_xx) %>%
    summarise(freq = n(), .groups="drop") %>%
    mutate(prob = freq / sum(freq))

  # 2. Forest Management (UI Selection)
  message(">>> Management Interface (Waiting for selection)...")
  df_corte <- selecionar_arvores_gui(df, area_total)

  # Free up some memory after UI closes
  gc()

  # Flag the selected trees as cut
  ids_cortados <- df_corte$i_arbre
  df$status[df$i_arbre %in% ids_cortados] <- "cortada"

  # 2.1 Apply Collateral Logging Damage
  vivas_remanescentes <- df %>% dplyr::filter(status == "viva")

  if(length(ids_cortados) > 0) {
    n_dano <- round(length(ids_cortados) * TX_DANO_POR_ARVORE)

    if(n_dano > 0) {
      ids_dano <- sample(vivas_remanescentes$i_arbre, min(n_dano, nrow(vivas_remanescentes)))
      df$status[df$i_arbre %in% ids_dano] <- "morta_dano"

      message(sprintf(">>> Collateral Damage Applied: %d cut trees -> %d damaged/dead trees.",
                      length(ids_cortados), length(ids_dano)))
    }
  }

  # 3. Simulation Engine
  message(paste(">>> Simulating", anos_simulacao, "years..."))

  df_vivas <- df %>% dplyr::filter(status == "viva")
  df_mortas_exploracao <- df %>% dplyr::filter(status != "viva")

  # Run the core simulation
  res_sim <- rodar_simulacao(df_vivas, mod, tb_prob, anos = anos_simulacao)

  # Merge results
  res_longo <- dplyr::bind_rows(df_mortas_exploracao, res_sim)

  # 4. Data Export (FINAL REPORT ONLY)
  message(">>> Saving results...")

  if(exists("gerar_tabela_final_larga")) {
    # Generate the wide-format table (DBH values in columns)
    res_excel <- gerar_tabela_final_larga(res_longo)

    # Apply requested formatting and cleanup
    res_excel <- res_excel %>%
      # Force dplyr rename to avoid namespace masking
      dplyr::rename(X = X_Original, Y = Y_Original) %>%
      # Force dplyr select to remove event columns
      dplyr::select(-any_of(c("Ano_Evento", "ano_corte")))

    # Save the single file using empty strings instead of NA
    write.csv2(res_excel, file.path(pasta_saida, "FINAL.csv"), row.names = FALSE, na = "")

    message(paste(">>> SUCCESS! Report saved at:", file.path(pasta_saida, "FINAL.csv")))
  }

  # 5. 3D Animation Rendering
  if(gerar_animacao) {
    if(exists("gerar_animacao")) {
      message(">>> Generating 3D Animation...")
      gerar_animacao(res_longo, df_original = df_pre_corte, output_dir = file.path(pasta_saida, "Animacao"))
    } else {
      message(">>> WARNING: Function 'gerar_animacao' not found. Make sure to load visualizacao.R")
    }
  }

  return(res_longo)
}
