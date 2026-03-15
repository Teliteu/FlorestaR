#' @param df_resultado Simulation results dataframe
#' @param df_original Original pre-cut dataframe (Optional)
#' @param output_dir Output directory path
#' @export
#' @import rayshader rgl dplyr magick raster tree3d
gerar_animacao <- function(df_resultado, df_original = NULL, output_dir, width=1000, height=1000, fps=4) {

  requireNamespace("rayshader", quietly=TRUE)
  requireNamespace("rgl", quietly=TRUE)
  requireNamespace("tree3d", quietly=TRUE)
  requireNamespace("dplyr", quietly=TRUE)

  # Create the output folder if it doesn't exist
  if(!dir.exists(output_dir)) dir.create(output_dir)

  # 1. Prepare data and assign stable visuals (so trees don't change color/shape every frame)
  df_vis_sim <- atribuir_estetica_estavel(df_resultado)

  if(!is.null(df_original)) {
    df_vis_orig <- atribuir_estetica_estavel(df_original)
    df_vis_orig$Ano <- 0
    # Try applying the spatial grid transformation
    try({ df_vis_orig <- transformar_para_grid(df_vis_orig) }, silent=TRUE)
    df_vis_orig$Ano <- -1
  }

  # Apply grid transformation to the simulation data
  try({ df_vis_sim <- transformar_para_grid(df_vis_sim) }, silent=TRUE)

  # Establish a baseline reference for tree heights to keep proportions stable
  n_ref <- if(!is.null(df_original)) nrow(df_original) else nrow(df_vis_sim %>% dplyr::filter(Ano==0))
  constante_visual <- n_ref * 1.8

  tudo <- if(!is.null(df_original)) dplyr::bind_rows(df_vis_sim, df_vis_orig) else df_vis_sim

  # Calculate the spatial bounding box for the 3D scene
  limites <- tudo %>%
    dplyr::summarise(xmin=min(X,na.rm=T), xmax=max(X,na.rm=T), ymin=min(Y,na.rm=T), ymax=max(Y,na.rm=T))

  extent_obj <- raster::extent(limites$xmin, limites$xmax, limites$ymin, limites$ymax)

  # Setup the RGL 3D window
  rgl::clear3d()
  rgl::open3d(windowRect = c(0, 0, width, height))
  rgl::bg3d("white")

  # Flat terrain base
  dem <- matrix(0, 400, 400)

  # Helper function to render a single year/frame
  render_frame <- function(df_frame, nome_arq) {
    n_atual <- nrow(df_frame)
    zscale_frame <- constante_visual / max(n_atual, 1)

    rgl::clear3d()

    # Render the ground base
    dem %>% rayshader::sphere_shade(texture="imhof1") %>%
      rayshader::plot_3d(dem, solid=T, shadow=T, zscale=zscale_frame, solidcolor="#e0f7fa", background="white", plot_new=F)

    # Render living trees, grouped by crown type for performance
    vivas <- df_frame %>% dplyr::filter(status=="viva")
    if(nrow(vivas)>0) {
      grps <- split(vivas, vivas$crown_type)
      for(tp in names(grps)) {
        g <- grps[[tp]]
        try({
          rayshader::render_tree(
            heightmap=dem, lat=g$Y, long=g$X,
            trunk_radius=g$DAP_inicial*0.1,
            tree_height=g$DAP_inicial*2/3,
            crown_color=g$crown_color,
            zscale=zscale_frame,
            clear_previous=F,
            trunk_height_ratio=2/3, crown_width_ratio=1,
            custom_obj_crown=tree3d::get_crown_file(tp, solid=F),
            custom_obj_trunk=tree3d::get_trunk_file(),
            extent = extent_obj
          )
        }, silent=F)
      }
    }

    # Render dead/cut trees as simple black stumps
    mortas <- df_frame %>% dplyr::filter(status!="viva")
    if(nrow(mortas)>0) {
      try({
        rayshader::render_tree(
          heightmap=dem, lat=mortas$Y, long=mortas$X,
          trunk_radius=mortas$DAP_inicial*0.1/3, type="basic",
          tree_height=mortas$DAP_inicial/1000, crown_width_ratio=0, trunk_height_ratio = 0.65,
          trunk_color="black", zscale=zscale_frame, clear_previous=F,
          extent = extent_obj
        )
      }, silent=F)
    }

    # Set camera angle and take a screenshot
    rayshader::render_camera(theta=45, phi=30, zoom=0.9)
    rgl::snapshot3d(file.path(output_dir, nome_arq), width=width, height=height, webshot=F)
  }

  # Render pre-cut scenario if available
  if(!is.null(df_original)) {
    message(">>> Rendering Original...")
    render_frame(df_vis_orig, "frame_original.png")
  }

  # Render simulation years
  anos <- sort(unique(df_vis_sim$Ano))
  message(">>> Rendering Simulation...")
  for(a in anos) {
    n_log <- nrow(df_vis_sim %>% dplyr::filter(Ano == a))
    render_frame(df_vis_sim %>% dplyr::filter(Ano == a), sprintf("frame_ano_%03d.png", a))
    message(sprintf("\r>>> Saved: Year %d (N=%d, ZScale=%.2f)", a, n_log, constante_visual/n_log))
  }

  rgl::close3d()

  # Combine all PNGs into an animated GIF
  message("\n>>> Assembling GIF...")
  imgs_sim <- list.files(output_dir, pattern = "frame_ano_.*\\.png", full.names = TRUE)
  img_orig <- file.path(output_dir, "frame_original.png")

  # Repeat the original frame 3 times at the start so the user has time to see it before it plays
  lista <- if(file.exists(img_orig)) c(rep(img_orig, 3), imgs_sim) else imgs_sim

  if(length(lista)>0) {
    gif <- magick::image_animate(magick::image_join(lapply(lista, magick::image_read)), fps=fps)
    magick::image_write(gif, file.path(output_dir, "animacao_floresta.gif"))
    return(file.path(output_dir, "animacao_floresta.gif"))
  }
}

# --- Helper Function ---
atribuir_estetica_estavel <- function(df, seed = 123) {
  stopifnot("i_arbre" %in% names(df))
  ids <- unique(df$i_arbre)

  # Set seed to ensure the same tree ID always gets the same color/shape
  set.seed(seed)
  ref <- data.frame(
    i_arbre = ids,
    crown_color = sample(c("#A2C683", "#75C165", "#066038"), length(ids), replace = TRUE),
    crown_type = sample(c("columnar", "oval", "pyramidal1", "pyramidal2", "rounded"), length(ids), replace = TRUE)
  )

  # Remove existing aesthetic columns if they somehow already exist, then join the stable ones
  df_limpo <- df %>% dplyr::select(-dplyr::any_of(c("crown_color", "crown_type")))
  dplyr::left_join(df_limpo, ref, by = "i_arbre")
}
