#' GUI for Tree Selection (CONAMA 406/2009 Logging Rules)
#' @export
#' @import shiny miniUI DT dplyr
selecionar_arvores_gui <- function(df, area_total = 18.0) {

  require(shiny)
  require(miniUI)
  require(DT)
  require(dplyr)

  # Maximum allowable volume to be extracted (30 m3/ha)
  limite_volume_upa <- 30 * area_total

  # 1. Group species to calculate legal retention thresholds
  censo_conama <- df %>%
    dplyr::filter(DAP_inicial >= 50) %>%
    dplyr::group_by(specie) %>%
    dplyr::summarise(
      N_DMC = n(),
      Eh_Rara = N_DMC <= 3, # Species with 3 or fewer individuals are legally protected
      Min_Manter = as.integer(max(ceiling(N_DMC * 0.10), 3)), # Must retain at least 10% or 3 trees
      .groups = "drop"
    )

  # 2. Filter harvestable trees (Removes protected/rare species and undersized trees)
  df_interface <- df %>%
    dplyr::filter(DAP_inicial >= 50) %>%
    dplyr::inner_join(censo_conama %>% dplyr::filter(!Eh_Rara), by = "specie")

  # 3. Build UI Layout
  ui <- miniPage(
    gadgetTitleBar("Forest Management"),
    miniContentPanel(
      fillCol(flex = c(1.2, 3.8),
              wellPanel(
                fluidRow(
                  column(5,
                         h5("Total Extracted Volume"),
                         h3(uiOutput("ui_vol_total")),
                         p(paste("Maximum Limit:", round(limite_volume_upa, 1), "m³"))
                  ),
                  column(7,
                         h5("Remaining Balance"),
                         DT::dataTableOutput("tabela_fiscal", height = "120px")
                  )
                ),
                style = "padding: 10px; background: #fdfefe; border: 1px solid #ddd;"
              ),
              div(
                h5("Available Trees (Only species with > 3 individuals and DBH >= 50 cm):"),
                DT::dataTableOutput("tabela_arvores", height = "100%")
              )
      )
    )
  )

  # 4. Server Logic
  server <- function(input, output, session) {

    # Render main selection table (Translating column names for display)
    output$tabela_arvores <- DT::renderDataTable({
      DT::datatable(
        df_interface %>% dplyr::select(
          Tree_ID = i_arbre,
          Species = specie,
          Initial_DBH = DAP_inicial,
          Volume,
          Plot = Parcela
        ),
        selection = 'multiple',
        options = list(pageLength = 20, scrollY = "300px", dom = 'ftp'),
        rownames = FALSE
      )
    })

    # Core audit logic: recalculates thresholds whenever a tree is selected
    auditoria <- reactive({
      ids <- input$tabela_arvores_rows_selected
      cortadas <- if(length(ids) > 0) df_interface[ids, ] else data.frame()
      vol_total <- if(nrow(cortadas) > 0) sum(cortadas$Volume, na.rm = TRUE) else 0

      resumo_corte <- if(nrow(cortadas) > 0) {
        cortadas %>% dplyr::group_by(specie) %>% dplyr::summarise(Qtd_Corte = n(), .groups = "drop")
      } else {
        data.frame(specie = character(), Qtd_Corte = integer())
      }

      # Join selections with the legal thresholds to check for violations
      fiscal <- censo_conama %>%
        dplyr::filter(!Eh_Rara) %>%
        dplyr::left_join(resumo_corte, by = "specie") %>%
        dplyr::mutate(
          Qtd_Corte = ifelse(is.na(Qtd_Corte), 0, Qtd_Corte),
          Saldo = N_DMC - Qtd_Corte,
          Alerta = Saldo < Min_Manter,
          Status = ifelse(Alerta, "BELOW MINIMUM (3)", "OK")
        ) %>%
        dplyr::filter(Qtd_Corte > 0 | Alerta)

      list(vol_total = vol_total, fiscal_tab = fiscal)
    })

    # UI updates: Volume counter
    output$ui_vol_total <- renderUI({
      v <- auditoria()$vol_total
      cor <- if(v > limite_volume_upa) "red" else "#27ae60"
      HTML(paste0("<span style='color:", cor, "; font-size: 24px; font-weight: bold;'>", round(v, 2), " m³</span>"))
    })

    # UI updates: Audit table (Translating column names for display)
    output$tabela_fiscal <- DT::renderDataTable({
      datatable(
        auditoria()$fiscal_tab %>% dplyr::select(
          Species = specie,
          Cut_Qty = Qtd_Corte,
          Balance = Saldo,
          Legal_Min = Min_Manter,
          Status
        ),
        options = list(dom = 't', paging = FALSE, scrollY = "100px"),
        rownames = FALSE
      ) %>%
        formatStyle('Status', color = styleEqual(c("BELOW MINIMUM (3)"), c("red")))
    })

    # Close gadget and return selected trees
    observeEvent(input$done, {
      ids <- input$tabela_arvores_rows_selected
      stopApp(if(length(ids) > 0) df_interface[ids, ] else df_interface[0, ])
    })
  }

  message(">>> Waiting for user selection in the interface...")
  flush.console()

  # Launch the Shiny gadget in the default web browser
  runGadget(ui, server, viewer = browserViewer())
}
