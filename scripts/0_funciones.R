# Un módulo sin enlace ========================================================

descargar_modulo_n <- function(year, trimestre, modulo = "sdem", formato = "csv")
{
  url <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/"
  
  # obtener enlaces según el periodo
  if (year >= 2023) {
    url <- paste(url, "enoe_", year, "_trim", trimestre, "_", formato, ".zip", sep = "")
  } else if ((year == 2020 & trimestre >= 3 ) | (year >= 2021 & year <= 2022)) {
    url <- paste(url, "enoe_n_", year, "_trim", trimestre, "_", formato, ".zip", sep = "")
  } else {
    url <- paste(url, year, "trim", trimestre, "_", formato, ".zip", sep = "")
  }
  
  # enlaces con casos especiales
  # doble barra
  if (year == 2024 & trimestre == 2) {
    url <- sub(
      x = url,
      pattern = "/enoe_2024",
      replacement = "//enoe_2024"
    )
  } else if (year == 2020 & trimestre == 2) {
    # no existe enlace
    stop("No existe enlace para dicho periodo. Se sugiere utilizar las bases de datos de la ETOE, la cual proporciona información para los meses de abril, mayo y junio: <https://www.inegi.org.mx/investigacion/etoe/default.html#Microdatos>. Las cifras que ofrece ETOE no son estrictamente comparables con ENOE pero son una aproximación a los indicadores de la ENOE. La comparación es útil como medida de referencia.")
  }
  
  # proceso de descarga
  archivo_temp <- tempfile(fileext = ".zip")
  
  # se aumenta tiempo máximo de descarga para evitar error
  options(timeout = max(900, getOption("timeout")))
  
  download.file(
    url = url,
    destfile = archivo_temp
  )
  
  # lista de los microdatos comprimidos
  microdatos <- unzip(archivo_temp, list = TRUE)$Name
  
  # extracción
  unzip(
    zipfile = archivo_temp, 
    # modulo especifico
    files = grepv(modulo, microdatos, ignore.case = TRUE),
    exdir = tempdir()
  )
  
  # ruta para cargar archivos
  archivo_temp <- list.files(
    path = tempdir(),
    pattern = "\\.csv$|\\.dbf$|\\.dta$|\\.sav$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # nombre para la lista
  nombres <- basename(archivo_temp)
  
  nombres <- sub(
    x = nombres,
    pattern = "ENOE_",
    replacement = ""
  )
  
  nombres <- sub(
    x = nombres,
    pattern = "\\.csv|\\.dbf|\\.dta|\\.sav",
    replacement = "",
    ignore.case = TRUE
  )
  
  # importación según tipo de archivo
  if (formato == "csv") {
    enoe <- readr::read_csv(
      file = archivo_temp,
      col_types = cols(.default = col_character())
    )
  } else if (formato == "dbf") {
    enoe <- foreign::read.dbf(archivo_temp)
  } else if (formato == "dta") {
    enoe <- haven::read_dta(archivo_temp)
  } else if (formato == "sav") {
    enoe <- haven::read_sav(archivo_temp)
  }
  
  # eliminación de archivos descargados y extraídos
  archivo_temp <- list.files(
    path = tempdir(),
    pattern = "\\.zip$|\\.csv$|\\.dbf$|\\.dta$|\\.sav$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  unlink(archivo_temp)
  
  return(enoe)
}

# Distintos módulos sin enlace ================================================

obtener_n_enlaces <- function(year_actual, trimestre_actual, n_year)
{
  # datos de entrada -------
  # resto de variables
  year_final <- year_actual - n_year
  year <- year_actual
  
  trimestre_final <- trimestre_actual
  trimestre <- trimestre_actual
  
  i <- 1
  tamanio <- trimestre_actual + ((n_year - 1) * 4) + (4 - trimestre_final + 1)
  enlace <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/"
  
  url <- vector(mode = "character", length = tamanio)
  
  # bucle -----
  while (year_final <= year) {
    
    while (trimestre >= 1) {
      
      # https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/enoe_2026_trim1_csv.zip
      
      # obtener enlaces según el periodo
      if (year >= 2023) {
        url[[i]] <- paste(enlace, "enoe_", year, "_trim", trimestre, "_csv.zip", sep = "")
      } else if ((year == 2020 & trimestre >= 3 ) | (year >= 2021 & year <= 2022)) {
        url[[i]] <- paste(enlace, "enoe_n_", year, "_trim", trimestre, "_csv.zip", sep = "")
      } else {
        url[[i]] <- paste(enlace, year, "trim", trimestre, "_csv.zip", sep = "")
      }
      
      # enlaces con casos especiales
      # doble barra
      if (year == 2024 & trimestre == 2) {
        url[[i]] <- paste(enlace, "/enoe_", year, "_trim", trimestre, "_csv.zip", sep = "")
      } else if (year == 2020 & trimestre == 2) {
        # no existe enlace
        url[[i]] <- paste("No existe enlace para el periodo", year, "trimestre", trimestre)
      }
      
      if (year == year_final & trimestre == trimestre_actual) {
        break
      } else {
        trimestre <- trimestre - 1 
        i <- i + 1
      }
    } 
    
    trimestre <- 4
    year <- year - 1
  }
  
  return(url)
}

descargar_n_modulos <- function(year_actual, trimestre_actual, n_year = 4, modulo = "sdem")
{
  # obtener enlaces ----
  url <- obtener_n_enlaces(year_actual, trimestre_actual, n_year)
  
  # crear archivos temporales -----
  
  tamanio <- length(url)
  i <- vector(mode = "integer", length = tamanio)
  archivos_temp <- vector(mode = "character", length = tamanio)
  
  for (i in 1:tamanio) {
    archivos_temp[[i]] <- tempfile(fileext = ".zip")
  }
  
  # descargar microdatos -----
  
  options(timeout = max(900, getOption("timeout")))
  
  i <- vector(mode = "integer", length = tamanio)
  # si ningún elemento contiene dicha expresión devuelve un vector vacío
  periodo_sn_enlace <- grep(
    x = url,
    pattern = "No existe enlace"
  )
  
  # si el vector está vacío le asigna valor 0
  if (is_empty(periodo_sn_enlace)) {periodo_sn_enlace <- 0}
  
  for (i in 1:tamanio) {
    if ( i != periodo_sn_enlace ) {
      download.file(
        url = url[[i]],
        destfile = archivos_temp[[i]]
      )
    }
  }
  
  # extraer módulo especificado -----
  
  tamanio <- length(archivos_temp)
  i <- vector(mode = "integer", length = tamanio)
  
  for (i in 1:tamanio) {
    
    if ( i != periodo_sn_enlace ) {
      microdatos <- unzip(
        zipfile = archivos_temp[[i]],
        list = TRUE,
      )$Name
      
      unzip(
        zipfile = archivos_temp[[i]],
        list = FALSE,
        files = grepv(modulo, microdatos, ignore.case = TRUE),
        exdir = tempdir(),
        overwrite = TRUE
      )
    }
  }
  
  # carga -----
  
  # ruta
  archivos_extraidos <- list.files(
    path = tempdir(),
    pattern = "\\.csv",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # nombre
  nombre_archivo <- basename(archivos_extraidos)
  
  nombre_archivo <- sub(
    x = nombre_archivo,
    pattern = "\\.csv",
    replacement = "",
    ignore.case = TRUE
  )
  
  nombre_archivo <- sub(
    x = nombre_archivo,
    pattern = "ENOE_|ENOEN_",
    replacement = "",
    ignore.case = TRUE
  )
  
  # nombre_archivo <- tolower(nombre_archivo)
  
  # definir como tibbles
  
  # crea n objetos, según el número de archivos, tipo tibble
  # tamanio <- length(archivos_extraidos)
  # i <- vector(mode = "integer", length = tamanio)
  # 
  # for (i in 1:tamanio) {
  #   assign(
  #     x = nombre_archivo[[i]],
  #     value = read_csv(
  #       file = archivos_extraidos[[i]],
  #       col_types = cols(.default = col_character())
  #     )
  #   )
  # }
  
  # crea un objeto tipo lista
  # fuente R for data science (2a ed.) sección 26.3.4
  n_modulos <- archivos_extraidos |> 
    set_names(nm = nombre_archivo) |> 
    map( 
      \(archivos_extraidos) # para añadir argumentos 
      readr::read_csv(
        archivos_extraidos, 
        col_types = cols(.default = col_character())
      )
    ) 
  
  # eliminar archivos comprimidos -----
  
  unlink(
    list.files(
      path = tempdir(),
      pattern = "\\.zip$|\\.csv$",
      full.names = TRUE
    )
  )
  
  return(n_modulos)
}

# Criterio general ============================================================

# para las tasas se requiere: 
# 1. definir `eda` como entero 
# 2. definir como encuesta compleja
# 3. aplicar criterio general (INEGI, 2023:28):
# datos |> 
#   filter(r_def == 0, c_res == 1 | c_res == 3, between(eda, 15, 98))

aplicar_criterio <- function(datos, ponderador){
  datos |>
    mutate(
      eda = as.integer(eda),
      {{ponderador}} := as.double(  {{ponderador}}  )
    ) |>
    # criterio general (INEGI, 2023:28)
    filter(
      r_def == 0,
      c_res == 1 | c_res == 3,
      between(eda, 15, 98)
    )
}

# Indicadores =================================================================

# tasas complementarias a nivel general
calcular_tasas_tot <- function(datos, nombre, filtro_num, filtro_den){
  df <- get(datos)
  
  df |>
    summarise(
      datos = datos,
      nombre = nombre,
      numerador = survey_total(  !!parse_expr(filtro_num)  , vartype = NULL, na.rm = TRUE),
      denominador = survey_total(  !!parse_expr(filtro_den)  , vartype = NULL, na.rm = TRUE),
      tasa = numerador / denominador
    )
}

# tasas complementarias desagregadas
calcular_tasas_sub <- function(datos, nombre, grupo, filtro_num, filtro_den){
  datos <- get(datos)
  grupo_sym <- sym(grupo)
  
  datos |>
    group_by(  !!grupo_sym  ) |> 
    summarise(
      nombre = nombre,
      numerador = survey_total(  !!parse_expr(filtro_num)  , vartype = NULL, na.rm = TRUE),
      denominador = survey_total(  !!parse_expr(filtro_den)  , vartype = NULL, na.rm = TRUE),
      tasa = numerador / denominador
    ) |> 
    rename("valor" :=  !!grupo_sym) |> 
    mutate(grupo = grupo) |> 
    relocate(grupo, .before = valor)
}

# total agregado
calcular_pob_tot <- function(datos, poblacion, filtro){
  df <- get(datos)
  
  df |> 
    summarise(
      datos = datos,
      poblacion = poblacion,
      # n = n(),
      tot = survey_total(  !!parse_expr(filtro)  , vartype = c("se", "cv", "ci"))
    ) 
}

# total desagregado por grupos
calcular_pob_sub <- function(datos, poblacion, grupo, filtro){
  datos <- get(datos)
  grupo_sym <- sym(grupo)
  
  datos |> 
    group_by(  !!grupo_sym  ) |>
    summarise(
      poblacion = poblacion, 
      # n = n(),
      tot = survey_total(  !!parse_expr(filtro)  , vartype = c("se", "cv", "ci"))
    ) |> 
    rename("valor" :=  !!grupo_sym) |> 
    mutate(grupo = grupo) |> 
    relocate(grupo, .before = valor) |> 
    relocate(poblacion, .before = grupo)
}

# se define como encuesta compleja (INEGI, 2023: 42)
definir_encuesta <- function(datos, ponderador){
  datos |>
    as_survey_design(
      ids = upm,
      strata = est,
      weights =  {{ponderador}}  ,
      nest = TRUE
    )
}

# indicadores de precisión estadística (INEGI, 2023: 57)
validar_precision <- function(datos, estimador_cv){
  # datos <- get(datos)
  cv_sym <- sym(estimador_cv)
  
  datos |>
    mutate(
      nivel_precision = case_when(
        !!cv_sym    <= 0.15 ~ "Alta",
        !!cv_sym    > 0.15 &    !!cv_sym     <= 0.30 ~ "Moderada",
        !!cv_sym    > 0.30 ~ "Baja",
        TRUE ~ NA
      )
    )
}



# Cuadros estadísticos ========================================================

## tasas por sexo -------------------------------

generar_cuadro_tasas_tot <- 
  function(
    datos_proc, 
    trimestre = c("Primer", "Segundo", "Tercer", "Cuarto"), 
    periodo_actual
  )
  {  
    # validar con argumentos especificados
    trimestre <- match.arg(trimestre)
    
    # etiquetas
    # año previo
    periodo_anterior <- periodo_actual - 1
    # subtítulo
    subtitulo <- paste(
      trimestre, "trimestre de", periodo_anterior, 
      "y", periodo_actual, "<br>(porcentaje)"
    )
    # encabezado de columna
    encabezado <- paste(trimestre, "trimestre de", periodo_actual)
    # fuente
    fuente <- paste(
      "Fuente: INEGI. Encuesta Nacional de Ocupación y Empleo (ENOE), ",
      periodo_actual, ".",
      sep = ""
    )
    
    # tabla final
    datos_proc |> 
      gt() |> 
      tab_header(
        title = md("**Tasas complementarias, según sexo**"),
        subtitle = md(subtitulo)
      ) |>
      tab_spanner(
        label = encabezado,
        columns = c(tasa, contains("sex"))
      ) |> 
      cols_hide(columns = c(datos, numerador:denominador)) |> 
      cols_label(
        nombre = "Tasa",
        tasa = "Total",
        sex_1 = "Hombres",
        sex_2 = "Mujeres"
      ) |> 
      cols_move_to_end(sex_1) |> 
      cols_align(columns = nombre, align = "left") |> 
      fmt_number(columns = -nombre, decimals = 1, scale_by = 100) |> 
      # alineación vertical al centro
      tab_style(
        locations = cells_column_labels(),
        style = cell_text(align = "center", v_align = "middle")
      ) |> 
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 1),
        footnote = "Población económicamente activa (PEA) como porcentaje de la población de 15 años y más."
      ) |> 
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 2:4),
        footnote = "Valor relativo respecto a la PEA."
      ) |> 
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 5:11),
        footnote = "Valor relativo respecto a la población ocupada."
      ) |>
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 2),
        footnote = "Considera a la población que se encuentra sin trabajar, pero que busca trabajo."
      ) |>
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 3),
        footnote = "Considera a la población desocupada y a la ocupada que trabajó menos de 15 horas a la semana."
      ) |>
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 4),
        footnote = "Incluye, además de a las y los desocupados, a las y los ocupados que buscan empleo."
      ) |>
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 5),
        footnote = "Representa a la población que, por las actividades realizadas, percibe un sueldo, salario o jornal, de la unidad económica para la que trabaja."
      ) |>
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 6),
        footnote = "Porcentaje de la población ocupada que tiene la necesidad y disponibilidad de ofertar más tiempo de trabajo de lo que su ocupación actual le permite."
      ) |>
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 7),
        footnote = md("Incluye a las personas que trabajan menos de 35 horas a la semana por razones ajenas a sus decisiones, las que trabajan más de 35 horas semanales con ingresos mensuales inferiores al salario mínimo y las que laboran más de 48 horas semanales y ganan hasta 2 salarios mínimos. Por construcción, los indicadores de la ENOE que involucran a la población ocupada, u otra variable clasificada en rangos de salarios mínimos, son sensibles a los cambios en dichos salarios. Para la comparación en el tiempo de la TCCO se presentan resultados a partir de salarios mínimos equivalentes base enero 2026. El INEGI pone a disposición de las y los usuarios cifras comparables en la siguiente liga: <https://www.inegi.org.mx/programas/enoe/15ymas/#Tabulados>.")
      ) |>
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 8),
        footnote = "Se refiere a la suma, sin duplicar, de las y los ocupados que son laboralmente vulnerables por la naturaleza de la unidad económica para la que trabajan, más aquellas personas cuyo vínculo o dependencia laboral no se reconoce por su fuente de trabajo. Así, en esta tasa se incluyen -además del componente que labora en micronegocios no registrados o sector informal- otras modalidades análogas, como las y los ocupados por cuenta propia en la agricultura de subsistencia, así como a quienes laboran sin la protección de la seguridad social y cuyos servicios los utilizan las unidades económicas registradas."
      ) |>
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 9),
        footnote = "Representa a la población ocupada que trabaja para una unidad económica que opera a partir de los recursos del hogar, pero sin constituirse como empresa, de modo que la actividad no tiene una situación identificable e independiente de ese hogar."
      ) |>
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 10),
        footnote = "Proporción de la población ocupada no agropecuaria que comprende la suma, sin duplicar, de las y los ocupados que son laboralmente vulnerables por la naturaleza de la unidad económica para la que trabajan, más otras y otros ocupados no agropecuarios cuyo vínculo o dependencia laboral no se reconoce por su fuente de trabajo."
      ) |>
      tab_footnote(
        locations = cells_body(columns = nombre, rows = 11),
        footnote = "Proporción de la población ocupada que trabaja para una unidad económica no agropecuaria y que opera a partir de los recursos del hogar, pero sin constituirse como empresa. Así, los ingresos, los materiales y equipos que se utilizan para el negocio no son independientes o distinguibles de los del propio hogar. Esta tasa se calcula teniendo como referente (denominador) a la población ocupada no agropecuaria."
      ) |> 
      tab_source_note(
        source_note = fuente
      ) |>
      tab_options(
        heading.title.font.weight = "bold",
        column_labels.font.weight = "bold",
        footnotes.order = "preserve_order"
      )
  }

## tasas desagregadas ---------------------------

generar_cuadros_tasas_sub <- 
  function(
    datos_proc, 
    grupo = c("entidades", "ciudades"), 
    trimestre = c("Primer", "Segundo", "Tercer", "Cuarto"), 
    periodo_actual
  )
  {
    
    grupo <- match.arg(grupo)
    trimestre <- match.arg(trimestre)
    
    if (grupo == "entidades") { 
      titulo <- "entidad federativa";
      encabezado <- "Entidad federativa"
    } 
    if (grupo == "ciudades") {
      titulo <- "área metropolitana";
      encabezado <- "Área metropolitana de la ciudad de:"
    }
    
    titulo <- paste(
      "**Población y tasas complementarias de ocupación y desocupación, <br> según ",
      titulo, "**",
      sep = ""
    )
    
    subtitulo <- paste(trimestre, "trimestre de", periodo_actual, "<br>(personas y porcentaje)")
    
    fuente <- paste(
      "Fuente: INEGI. Encuesta Nacional de Ocupación y Empleo (ENOE), ",
      periodo_actual, ".",
      sep = ""
    )
    
    datos_proc |> 
      gt() |> 
      # título
      tab_header(
        title = md(titulo),
        subtitle = md(subtitulo)
      ) |>
      # encabezado
      tab_spanner(
        columns = ocupada:desocupada,
        label = "Poblacion",
        level = 2
      ) |> 
      tab_spanner(
        columns = ocupada:desocupada,
        label = "(personas)",
        level = 1
      ) |> 
      tab_spanner(
        columns = tp:tosi2,
        label = "Tasa de",
        level = 2
      ) |> 
      tab_spanner(
        columns = tp:tosi2,
        label = "(porcentaje)",
        level = 1
      ) |> 
      cols_hide(columns = til2:tosi2) |> 
      # etiqueta de encabezados de columnas 
      cols_label(
        valor = md(encabezado),
        ocupada = "Ocupada",
        desocupada = "Desocupada",
        tp = "Participación",
        td = "Desocupación ",
        topd = "Ocupación parcial y desocupación ",
        tpg = "Presión general ",
        tta = "Trabajo asalariado ",
        tsub = "Subocupación ",
        tcco = "Condiciones críticas de ocupación ",
        til1 = "Informalidad laboral 1 ",
        tosi1 = "Ocupación en el sector informal 1 "
        # til2 = "Informalidad laboral 2 ",
        # tosi2 = "Ocupación en el sector informal 2 ",
      ) |> 
      # formato de valores de celdas
      fmt_number(
        columns = ocupada:desocupada,
        decimals = 0
      ) |> 
      fmt_number(
        columns = -(valor:desocupada),
        decimals = 1,
        scale_by = 100
      ) |> 
      # formato de estilo
      cols_align(columns = valor, align = "left") |>  # primera columna
      cols_align(columns = ocupada:desocupada, align = "right") |> # población
      cols_align(columns = starts_with("t"), align = "center") |> # tasas
      # alineación vertical al centro
      tab_style(
        locations = cells_column_labels(),
        style = cell_text(v_align = "middle")
      ) |> 
      # la primera fila en negrita 
      tab_style(
        locations = cells_body(rows = 1),
        style = cell_text(weight = "bold")
      ) |> 
      # notas al pie de tabla
      tab_footnote(
        locations = cells_column_labels(columns = tp),
        footnote = "Población económicamente activa (PEA) como porcentaje de la población de 15 años y más."
      ) |> 
      tab_footnote(
        locations = cells_column_labels(columns = td:tpg),
        footnote = "Valor relativo respecto a la PEA."
      ) |> 
      tab_footnote(
        locations = cells_column_labels(columns = tta:tosi1),
        footnote = "Valor relativo respecto a la población ocupada."
      ) |> 
      tab_footnote(
        locations = cells_column_labels(columns = tcco),
        footnote = gt::md("Por construcción, los indicadores de la ENOE, que involucran a la población ocupada u otra variable clasificada en rangos de salarios mínimos, son sensibles a los cambios en dichos salarios. Las cifras de la tasa de condiciones críticas de ocupación (TCCO) se construyen a partir de salarios mínimos nominales. Para efecto de comparar en el tiempo indicadores que toman como referencia salarios mínimos, es necesario considerar la evolución del Índice Nacional de Precios al Consumidor (INPC) y construir salarios mínimos equivalentes, a partir de un año base como referencia. El INEGI pone a disposición de las y los usuarios cifras comparables en el tiempo de la TCCO en la siguiente liga: <https://www.inegi.org.mx/programas/enoe/15ymas/#Tabulados>.")
      ) |> 
      tab_source_note(
        source_note =  fuente
      ) |>
      # estilo de fuente
      tab_options(
        heading.title.font.weight = "bold",
        column_labels.font.weight = "bold"
      )
    
  }


