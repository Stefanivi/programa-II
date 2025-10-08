<<<<<<< HEAD:Proyecto/Presentación/quarto presentación_files/app.R
# ---- Paquetes (auto-instala) ----
pkgs <- c("shiny","leaflet","sf","dplyr","viridisLite","DT","readxl","stringi","htmltools")
miss <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(miss)) install.packages(miss, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Utilidades y rutas (AJUSTA) ----
norm <- function(x) stringi::stri_trans_general(tolower(x), "Latin-ASCII")

# va a buscar la ruta donde estan los mapitas y el archivo Excel de indicadores finales con el indice

path_shp  <- paste(getwd(), "/Presentación/quarto presentación_files/gadm41_GTM_shp", sep="")
path_xlsx <- file.path(path_shp, "indicadores_acceso_tecnologico.xlsx")

# ---- Datos base ----
gadm1 <- sf::st_read(dsn = path_shp, layer = "gadm41_GTM_1", quiet = TRUE) |>
  dplyr::mutate(NAME_1_norm = norm(NAME_1))

df_raw <- readxl::read_xlsx(path_xlsx, .name_repair = "unique")
df10   <- df_raw[, 1:11]

posibles_cod <- names(df10)[sapply(df10, function(x) all(na.omit(as.integer(x)) %in% 1:22))]
cod_col <- if (length(posibles_cod) >= 1) posibles_cod[1] else "codigo_depto"

df10 <- df10 |>
  dplyr::mutate(ine_code = as.integer(.data[[cod_col]])) |>
  dplyr::mutate(across(setdiff(names(df10), c(cod_col, "ine_code")), ~{
    if (is.character(.x)) readr::parse_number(.x) else .x
  }))

cruce_ine <- tibble::tibble(
  ine_code = 1:22,
  depto_ine = c(
    "Guatemala","El Progreso","Sacatepéquez","Chimaltenango","Escuintla",
    "Santa Rosa","Sololá","Totonicapán","Quezaltenango","Suchitepéquez",
    "Retalhuleu","San Marcos","Huehuetenango","Quiché","Baja Verapaz",
    "Alta Verapaz","Petén","Izabal","Zacapa","Chiquimula","Jalapa","Jutiapa"
  )
) |> dplyr::mutate(depto_norm = norm(depto_ine))

df10_join <- df10 |> dplyr::left_join(cruce_ine, by = "ine_code")

# Solo indicadores numéricos
indic_cols <- setdiff(names(df10), c(cod_col, "ine_code"))
indic_cols <- indic_cols[sapply(df10[indic_cols], is.numeric)]
stopifnot(length(indic_cols) > 0)

# Unir todo para pintar rápido
mapa_all <- gadm1 |> dplyr::left_join(df10_join, by = c("NAME_1_norm" = "depto_norm"))

# ---- UI ----
ui <- shiny::fluidPage(
  shiny::titlePanel("Mapa interactivo de indicadores"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput("indic", "Indicador:", choices = indic_cols),
      shiny::checkboxInput("rev", "Escala inversa", value = FALSE),
      shiny::sliderInput("op", "Opacidad del relleno", min = 0.2, max = 1, value = 0.85, step = 0.05)
    ),
    shiny::mainPanel(
      leaflet::leafletOutput("mapa", height = 600),
      shiny::tags$hr(),
      DT::DTOutput("top10")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  # mapa base
  output$mapa <- leaflet::renderLeaflet({
    leaflet::leaflet(mapa_all) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addPolylines(weight = 1, color = "#ffffff", opacity = 1)
  })
  
  # capa por indicador
  shiny::observe({
    req(input$indic)
    col  <- input$indic
    vals <- mapa_all[[col]]
    
    pal <- leaflet::colorNumeric(
      palette = viridisLite::viridis(256, option = "C", direction = ifelse(input$rev, -1, 1)),
      domain = vals, na.color = "#e9ecef"
    )
    
    lab <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      mapa_all$NAME_1, col, ifelse(is.na(vals), "NA", format(round(vals, 3), big.mark = ","))
    ) |> lapply(htmltools::HTML)
    
    leaflet::leafletProxy("mapa", data = mapa_all) |>
      leaflet::clearGroup("polys") |>
      leaflet::clearControls() |>
      leaflet::addPolygons(
        fillColor = ~pal(vals),
        color = "#ffffff", weight = 1, opacity = 1,
        fillOpacity = input$op,
        highlightOptions = leaflet::highlightOptions(weight = 2, color = "#2c3e50", bringToFront = TRUE),
        label = lab, group = "polys"
      ) |>
      leaflet::addLegend("bottomright", pal = pal, values = vals, title = col, opacity = 0.9)
  })
  
  # Top 10 dinámico
  output$top10 <- DT::renderDT({
    col <- input$indic
    df10_join |>
      dplyr::select(Departamento = depto_ine, Valor = dplyr::all_of(col)) |>
      dplyr::arrange(dplyr::desc(Valor)) |>
      dplyr::slice_head(n = min(10, sum(!is.na(Valor)))) |>
      DT::datatable(options = list(pageLength = 10, dom = "t"), rownames = FALSE)
  })
}

shiny::shinyApp(ui, server)
=======
# ---- Paquetes (auto-instala) ----
pkgs <- c("shiny","leaflet","sf","dplyr","viridisLite","DT","readxl","stringi","htmltools")
miss <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
if (length(miss)) install.packages(miss, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Utilidades y rutas (AJUSTA) ----
norm <- function(x) stringi::stri_trans_general(tolower(x), "Latin-ASCII")

# va a buscar la ruta donde estan los mapitas y el archivo Excel de indicadores finales con el indice

path_shp  <- paste(getwd(), "/Presentación/quarto presentación_files/gadm41_GTM_shp", sep="")
path_xlsx <- file.path(path_shp, "indicadores_acceso_tecnologico.xlsx")

# ---- Datos base ----
gadm1 <- sf::st_read(dsn = path_shp, layer = "gadm41_GTM_1", quiet = TRUE) |>
  dplyr::mutate(NAME_1_norm = norm(NAME_1))

df_raw <- readxl::read_xlsx(path_xlsx, .name_repair = "unique")
df10   <- df_raw[, 1:11]

posibles_cod <- names(df10)[sapply(df10, function(x) all(na.omit(as.integer(x)) %in% 1:22))]
cod_col <- if (length(posibles_cod) >= 1) posibles_cod[1] else "codigo_depto"

df10 <- df10 |>
  dplyr::mutate(ine_code = as.integer(.data[[cod_col]])) |>
  dplyr::mutate(across(setdiff(names(df10), c(cod_col, "ine_code")), ~{
    if (is.character(.x)) readr::parse_number(.x) else .x
  }))

cruce_ine <- tibble::tibble(
  ine_code = 1:22,
  depto_ine = c(
    "Guatemala","El Progreso","Sacatepéquez","Chimaltenango","Escuintla",
    "Santa Rosa","Sololá","Totonicapán","Quezaltenango","Suchitepéquez",
    "Retalhuleu","San Marcos","Huehuetenango","Quiché","Baja Verapaz",
    "Alta Verapaz","Petén","Izabal","Zacapa","Chiquimula","Jalapa","Jutiapa"
  )
) |> dplyr::mutate(depto_norm = norm(depto_ine))

df10_join <- df10 |> dplyr::left_join(cruce_ine, by = "ine_code")

# Solo indicadores numéricos
indic_cols <- setdiff(names(df10), c(cod_col, "ine_code"))
indic_cols <- indic_cols[sapply(df10[indic_cols], is.numeric)]
stopifnot(length(indic_cols) > 0)

# Unir todo para pintar rápido
mapa_all <- gadm1 |> dplyr::left_join(df10_join, by = c("NAME_1_norm" = "depto_norm"))

# ---- UI ----
ui <- shiny::fluidPage(
  shiny::titlePanel("Mapa interactivo de indicadores"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput("indic", "Indicador:", choices = indic_cols),
      shiny::checkboxInput("rev", "Escala inversa", value = FALSE),
      shiny::sliderInput("op", "Opacidad del relleno", min = 0.2, max = 1, value = 0.85, step = 0.05)
    ),
    shiny::mainPanel(
      leaflet::leafletOutput("mapa", height = 600),
      shiny::tags$hr(),
      DT::DTOutput("top10")
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  # mapa base
  output$mapa <- leaflet::renderLeaflet({
    leaflet::leaflet(mapa_all) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addPolylines(weight = 1, color = "#ffffff", opacity = 1)
  })
  
  # capa por indicador
  shiny::observe({
    req(input$indic)
    col  <- input$indic
    vals <- mapa_all[[col]]
    
    pal <- leaflet::colorNumeric(
      palette = viridisLite::viridis(256, option = "C", direction = ifelse(input$rev, -1, 1)),
      domain = vals, na.color = "#e9ecef"
    )
    
    lab <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      mapa_all$NAME_1, col, ifelse(is.na(vals), "NA", format(round(vals, 3), big.mark = ","))
    ) |> lapply(htmltools::HTML)
    
    leaflet::leafletProxy("mapa", data = mapa_all) |>
      leaflet::clearGroup("polys") |>
      leaflet::clearControls() |>
      leaflet::addPolygons(
        fillColor = ~pal(vals),
        color = "#ffffff", weight = 1, opacity = 1,
        fillOpacity = input$op,
        highlightOptions = leaflet::highlightOptions(weight = 2, color = "#2c3e50", bringToFront = TRUE),
        label = lab, group = "polys"
      ) |>
      leaflet::addLegend("bottomright", pal = pal, values = vals, title = col, opacity = 0.9)
  })
  
  # Top 10 dinámico
  output$top10 <- DT::renderDT({
    col <- input$indic
    df10_join |>
      dplyr::select(Departamento = depto_ine, Valor = dplyr::all_of(col)) |>
      dplyr::arrange(dplyr::desc(Valor)) |>
      dplyr::slice_head(n = min(10, sum(!is.na(Valor)))) |>
      DT::datatable(options = list(pageLength = 10, dom = "t"), rownames = FALSE)
  })
}

shiny::shinyApp(ui, server)
>>>>>>> c57946e2d26fdd4737ce9fcdfc1e6a159721af92:Proyecto/Presentacion/quarto presentación_files/app.R
