# ── Librerías
library(sf)
library(readxl)
library(dplyr)
library(tidyr)
library(stringi)
library(ggplot2)
library(viridis)
library(ggpubr)
library(patchwork)

# ── Rutas ──
# va a buscar la ruta donde estan los mapitas y el archivo Excel de indicadores finales con el indice
path_shp  <- "C:/Users/Javie/OneDrive/Escritorio/PROGRAMACIÓN II/quarto presentación_files/gadm41_GTM_shp"
path_xlsx <- file.path(path_shp, "indicadores_acceso_tecnologico.xlsx")

# ── Utilidades ──
norm <- function(x) stringi::stri_trans_general(tolower(x), "Latin-ASCII")
sanitize <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)

# ── 1) Shapefile GADM nivel 1 (Departamentos) ──
stopifnot(dir.exists(path_shp), file.exists(path_xlsx))
gadm1 <- sf::st_read(dsn = path_shp, layer = "gadm41_GTM_1", quiet = TRUE) |>
  mutate(NAME_1_norm = norm(NAME_1))

gadm1 <- gadm1 |>
  mutate(
    NAME_1 = ifelse(NAME_1 == "Quezaltenango", "Quetzaltenango", NAME_1),
    NAME_1_norm = norm(NAME_1)  
  )

# ── 2) Excel: tomar primeras 11 columnas y detectar la de código INE (1..22) ──
df_raw <- readxl::read_xlsx(path_xlsx, .name_repair = "unique")
df10   <- df_raw[, 1:11]

# Detecta automáticamente la columna de códigos 1..22; si no, pon el nombre manual
posibles_cod <- names(df10)[sapply(df10, function(x) all(na.omit(as.integer(x)) %in% 1:22))]
cod_col <- if (length(posibles_cod) >= 1) posibles_cod[1] else "codigo_depto"

# Asegura entero y convierte textos numéricos (%, comas) a número
df10 <- df10 |>
  mutate(ine_code = as.integer(.data[[cod_col]])) |>
  mutate(across(setdiff(names(df10), c(cod_col, "ine_code")), ~{
    if (is.character(.x)) readr::parse_number(.x) else .x
  }))

# ── 3) Cruce INE → nombre oficial de depto (para empatar con GADM) ──
cruce_ine <- tibble::tibble(
  ine_code = 1:22,
  depto_ine = c(
    "Guatemala","El Progreso","Sacatepéquez","Chimaltenango","Escuintla",
    "Santa Rosa","Sololá","Totonicapán","Quetzaltenango","Suchitepéquez",
    "Retalhuleu","San Marcos","Huehuetenango","Quiché","Baja Verapaz",
    "Alta Verapaz","Petén","Izabal","Zacapa","Chiquimula","Jalapa","Jutiapa"
  )
) |>
  mutate(depto_norm = norm(depto_ine))

df10_join <- df10 |> left_join(cruce_ine, by = "ine_code")

# ── 4) Columnas/indicadores a mapear ──
indic_cols <- setdiff(names(df10), c(cod_col, "ine_code"))
stopifnot(length(indic_cols) > 0)

# ── 5) Carpeta de salida y loop: UN MAPA POR INDICADOR ──
out_dir <- "mapas_icl2024"
dir.create(out_dir, showWarnings = FALSE)

for (col in indic_cols) {
  
  # datos del indicador actual
  dat_i <- df10_join |>
    select(ine_code, depto_ine, depto_norm, !!col)
  
  # unir con geometría
  mapa_i <- gadm1 |>
    left_join(dat_i, by = c("NAME_1_norm" = "depto_norm"))
  
  # título/etiqueta y archivo
  titulo <- paste0("Indicador: ", col)
  fname  <- file.path(out_dir, paste0(sanitize(col), ".png"))
  
  # mapa
  p <- ggplot(mapa_i) +
    geom_sf(aes(fill = .data[[col]]), color = "white", linewidth = 0.2) +
    scale_fill_viridis_c(option = "C", na.value = "grey90",
                         name = col, guide = guide_colorbar(title.position = "top")) +
    coord_sf(datum = NA) +
    labs(
      title    = titulo,
      subtitle = "",
      caption  = "Fuente: Encuesta Nacional de Condiciones de Vida 2018"
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.title    = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.title    = element_text(face = "bold")
    )
  
  # ----- TOP-10- tabla -----
  fmt <- scales::label_number(accuracy = 0.1, big.mark = ",")
  
  top10 <- df10_join |>
    transmute(Departamento = depto_ine, Valor = .data[[col]]) |>
    arrange(desc(Valor)) |>
    slice_head(n = 10)
  
  # si no hay datos numéricos, evita explotar y salta a solo mapa
  if (nrow(top10) == 0 || all(is.na(top10$Valor))) {
    p_final <- p
  } else {
    top10 <- top10 |>
      mutate(
        Rank  = row_number(),
        Ranking = case_when(
          Rank == 1 ~ "🥇",
          Rank == 2 ~ "🥈",
          Rank == 3 ~ "🥉",
          TRUE ~ paste0("", Rank)
        ),
        Valor = fmt(Valor)
      ) |>
      select(Ranking, Departamento, Valor)
    
    n_rows <- nrow(top10)
    n_cols <- ncol(top10)
    
    tbl <- ggpubr::ggtexttable(
      top10, rows = NULL,
      theme = ggpubr::ttheme("classic", base_size = 12)
    ) +
      labs(title = "Top 10 (mayor a menor)") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
    
    # --- colorear columna de "Valor" por magnitud ---
    col_val <- which(colnames(top10) == "Valor")
    vals_num <- suppressWarnings(readr::parse_number(top10$Valor))
    rng <- range(vals_num, na.rm = TRUE)
    pal <- scales::col_numeric(viridis::viridis(100), domain = rng)
    
    for (i in seq_len(n_rows)) {
      r <- i + 1  # +1 porque la fila 1 es el encabezado
      v <- vals_num[i]
      
      # Verificación extra para evitar errores
      if (!is.na(v) &&
          r <= (nrow(top10) + 1) &&                # fila válida
          length(col_val) == 1 &&
          col_val <= ncol(top10)) {                # columna válida
        
        tbl <- ggpubr::table_cell_bg(tbl, row = r, column = col_val, fill = pal(v))
        tbl <- ggpubr::table_cell_font(tbl, row = r, column = col_val, color = "white", face = "bold")
      }
    }
    
    # zebra striping
    for (i in seq_len(n_rows)) {
      r <- i + 1
      if (i %% 2 == 0) tbl <- ggpubr::table_cell_bg(tbl, row = r, column = 1:n_cols, fill = "#F4F6F7")
    }
    
    # resaltar top 3 si existen
    if (n_rows >= 1) tbl <- ggpubr::table_cell_bg(tbl, row = 2, column = 1:n_cols, fill = "#FFF3CD")
    if (n_rows >= 2) tbl <- ggpubr::table_cell_bg(tbl, row = 3, column = 1:n_cols, fill = "#FFF9E3")
    if (n_rows >= 3) tbl <- ggpubr::table_cell_bg(tbl, row = 4, column = 1:n_cols, fill = "#FEFCF3")
    
    # colorear por magnitud la columna "Valor" (ubicación por nombre, a prueba de cambios)
    col_val <- which(colnames(top10) == "Valor")
    vals_num <- suppressWarnings(readr::parse_number(top10$Valor))
    rng <- range(vals_num, na.rm = TRUE)
    pal <- scales::col_numeric(viridis::viridis(100), domain = rng)
    
    for (i in seq_len(n_rows)) {
      r <- i + 1
      v <- vals_num[i]
      if (!is.na(v) && length(col_val) == 1) {
        tbl <- ggpubr::table_cell_bg(tbl,  row = r, column = col_val, fill = pal(v))
        tbl <- ggpubr::table_cell_font(tbl, row = r, column = col_val, color = "white", face = "bold")
      }
    }
    
    # combinar con el mapa 
    p_final <- (p + theme(legend.position = "bottom")) |
      patchwork::wrap_elements(full = tbl)
    p_final <- p_final + patchwork::plot_layout(widths = c(2.2, 1))
  }
  
  # ── guardar archivo (8x6 pulgadas, 300 dpi) ──
  ggsave(filename = fname, plot = p_final, width = 8, height = 6, dpi = 300)
  message("✔ Guardado: ", fname)
}

message("Listo. Revisa la carpeta: ", normalizePath(out_dir))

