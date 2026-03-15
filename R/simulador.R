# --------------------------------------------------
# simulador.R
# --------------------------------------------------
rodar_simulacao <- function(df, modelo_rds, tb_prob, anos = 17, tx_mort = 1.92, tx_recr = 2.55, dap_recrutas = 20) {

  library(dplyr)
  library(caret)
  library(nnet)

  message("\n>>> [INIT] SIMULATOR (DIRECT DBH)...")

  # Load normalization scales and the trained neural network model
  sc <- modelo_rds$scales
  modelo_ia <- modelo_rds$model
  niveis_treino <- c("IND", "NP/DL", "NP/TS", "PI")

  # Helper function to process yearly mortality and growth for a given dataframe
  processar_dinamica <- function(df_temp, ano_atual) {

    # 1. Mortality (Random selection based on the mortality rate)
    vivos_idx <- which(df_temp$status == "viva")
    if(length(vivos_idx) > 0) {
      n_mortes <- round(length(vivos_idx) * tx_mort / 100)
      if(n_mortes > 0) {
        mortos_idx <- sample(vivos_idx, size = min(n_mortes, length(vivos_idx)))
        df_temp$status[mortos_idx] <- "morta_natural"
      }
    }

    # 2. Growth (Neural Network Prediction)
    vivos_pos_mort <- which(df_temp$status == "viva")
    if(length(vivos_pos_mort) > 0) {

      # Update normalized time for the AI input
      df_temp$Tempo_n[vivos_pos_mort] <- as.numeric(ano_atual / sc$max_tempo)

      subset_ia <- df_temp[vivos_pos_mort, c("DAP_ini_n", "Tempo_n", "GrEc_xx")]

      # Ensure categorical variables match the training levels
      subset_ia$GrEc_xx <- factor(subset_ia$GrEc_xx, levels = niveis_treino)
      subset_ia$GrEc_xx[is.na(subset_ia$GrEc_xx)] <- "NP/TS"

      # Direct prediction of the final normalized DBH (DAP)
      pred_dap_n <- predict(modelo_ia, newdata = subset_ia)

      # Convert normalized DBH back to real scale (cm)
      dap_real <- as.numeric(pred_dap_n) * sc$max_dap

      # Overwrite the old DBH with the new predicted value
      df_temp$DAP[vivos_pos_mort] <- dap_real

      # Update derived variables (Normalized DBH, Basal Area, and Volume)
      df_temp$DAP_ini_n[vivos_pos_mort] <- df_temp$DAP[vivos_pos_mort] / sc$max_dap
      df_temp$Area_Basal[vivos_pos_mort] <- (pi * df_temp$DAP[vivos_pos_mort]^2) / 40000
      df_temp$Volume[vivos_pos_mort] <- df_temp$Area_Basal[vivos_pos_mort] * 10 * 0.7
    }

    df_temp$Ano <- ano_atual
    return(df_temp)
  }

  # --- SETUP ---
  # Initialize the main dataframe for Year 0
  df_main <- df %>%
    mutate(
      Ano = 0, status = "viva", Tempo_n = 0,
      DAP_ini_n = DAP / sc$max_dap,
      Area_Basal = (pi * DAP^2) / 40000,
      Volume = ((pi * DAP^2) / 40000) * 10 * 0.7
    )

  historico_list <- list(df_main)
  lista_recrutas <- list()

  # --- SIMULATION LOOP ---
  for(ano in seq_len(anos)) {
    cat(sprintf("\r>>> Processing Cycle: %d/%d", ano, anos))

    # Process the original population
    df_main <- processar_dinamica(df_main, ano)

    # Process all existing recruitment batches
    if(length(lista_recrutas) > 0) {
      for(i in seq_along(lista_recrutas)) {
        lista_recrutas[[i]] <- processar_dinamica(lista_recrutas[[i]], ano)
      }
    }

    # 3. Recruitment (New trees entering the simulation)
    # Count all living trees (original + previous recruits) to calculate the recruitment pool
    n_vivos <- sum(df_main$status == "viva") +
      if(length(lista_recrutas) > 0) sum(sapply(lista_recrutas, function(x) sum(x$status == "viva"))) else 0

    n_rec <- round(n_vivos * tx_recr / 100)

    if(n_rec > 0) {
      # Hack to quickly copy dataframe structure without keeping data
      novos <- df_main[1:n_rec, ]
      novos[,] <- NA

      # Sample ecological groups based on their original probabilities
      grupos_sort <- sample(tb_prob$GrEc_xx, size=n_rec, replace=TRUE, prob=tb_prob$prob)

      novos$i_arbre <- paste0("R", ano, "_", 1:n_rec)
      novos$Parcela <- sample(unique(df_main$Parcela), n_rec, replace=TRUE)

      # Generic species assignment for all new trees
      novos$specie  <- "Recruit"
      novos$GrEc_xx <- grupos_sort

      # Randomly distribute new trees across the area
      novos$X       <- sample(df_main$X, n_rec, replace=TRUE)
      novos$Y       <- sample(df_main$Y, n_rec, replace=TRUE)

      novos$status  <- "viva"
      novos$Ano     <- ano

      # Set initial size parameters for recruits
      novos$DAP         <- dap_recrutas
      novos$DAP_inicial <- dap_recrutas
      novos$Area_Basal  <- (pi * dap_recrutas^2) / 40000
      novos$Volume      <- novos$Area_Basal * 10 * 0.7

      # Set AI variables for the next cycle
      novos$DAP_ini_n <- dap_recrutas / sc$max_dap
      novos$Tempo_n   <- ano / sc$max_tempo

      # Store the new batch of recruits
      lista_recrutas[[length(lista_recrutas) + 1]] <- novos
    }

    # Append the current year's state to the history log
    historico_list[[ano + 1]] <- bind_rows(c(list(df_main), lista_recrutas))

    # Garbage collection to free up RAM every 2 simulated years
    if(ano %% 2 == 0) gc(verbose = FALSE)
  }

  return(bind_rows(historico_list))
}
