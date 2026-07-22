descargar_enoe <- function(url){
  
  # Datos de entrada ----
  
  # Almacena la ruta del directorio temporal
  dir_temp <- tempdir(check = TRUE)
  
  # Crea un archivo temporal que contendrá el archivo descargado
  archivo_temp <- tempfile(tmpdir = dir_temp, fileext = ".zip")
  
  # Proceso de descarga ----
  
  # tiempo máximo de descarga 15 min
  options(timeout = max(900, getOption("timeout")))
  
  download.file(url, destfile = archivo_temp) # descarga
  
  # ruta del archivo comprimido
  archivo_temp <- list.files(dir_temp, pattern = "\\.zip$", full.names = TRUE)
  
  # proceso de extracción ----
  
  # lo almacena en el directorio temporal
  unzip(zipfile = archivo_temp, exdir = dir_temp)
  
  # proceso de carga ----
  
  # ruta de los archivos extraídos
  enoe <- list.files(dir_temp, pattern = "\\.csv", full.names = TRUE)
  
  # cuestionario de ocupación y empleo 1
  coe1 <- read_csv(
    enoe[grepl("COE1", enoe)],
    col_types = cols(.default = col_character())
  )
  
  # cuestionario de ocupación y empleo 2
  coe2 <- read_csv(
    enoe[grepl("COE2", enoe)],
    col_types = cols(.default = col_character())
  )
  
  # hogar
  hog <- read_csv(
    enoe[grepl("HOG", enoe)], 
    col_types = cols(.default = col_character())
  )
  
  # sociodemográfico
  sdem <- read_csv(
    enoe[grepl("SDEM", enoe)],
    col_types = cols(.default = col_character())
  )
  
  # vivienda
  viv <- read_csv(
    enoe[grepl("VIV", enoe)], 
    col_types = cols(.default = col_character())
  )
  
  # proceso de eliminación ----
  
  # ruta de archivos descargados
  archivo_temp <- list.files(
    dir_temp, 
    pattern = "\\.zip$|\\.csv$", 
    full.names = TRUE
  )
  
  # eliminación de archivos descargados
  unlink(archivo_temp) 
  
  # eliminación de objetos creados
  # remove(list = c("archivo_temp", "dir_temp", "enoe", "url")) 
  
  return(list(coe1 = coe1, coe2 = coe2, hog = hog, sdem = sdem, viv = viv))
  
}

descargar_microdatos_enoe <- function(url, cuestionario){
  # Datos de entrada ----
  
  # Almacena la ruta del directorio temporal
  dir_temp <- tempdir(check = TRUE)
  
  # Crea un archivo temporal que contendrá el archivo descargado
  archivo_temp <- tempfile(tmpdir = dir_temp, fileext = ".zip")
  
  # Proceso de descarga ----
  # tiempo máximo de descarga 15 min
  options(timeout = max(900, getOption("timeout")))
  
  download.file(url, destfile = archivo_temp) # descarga
  
  # ruta del archivo comprimido
  archivo_temp <- list.files(dir_temp, pattern = "\\.zip$", full.names = TRUE)
  
  # lista de los microdatos comprimidos
  microdatos <- unzip(archivo_temp, list = TRUE)$Name
  
  unzip(
    archivo_temp, 
    # cuestionario especifico
    files = microdatos[grepl(cuestionario, microdatos)],
    exdir = dir_temp
  )
  
  # Proceso de carga ----
  # ruta del archivo extraído
  microdatos <- list.files(dir_temp, pattern = "\\.csv", full.names = TRUE)
  
  microdatos <- read_csv(microdatos, col_types = cols(.default = col_character()))
  
  # Proceso de eliminación ----
  # archivos descargados
  archivo_temp <- list.files(dir_temp, pattern = "\\.zip$|\\.csv$", full.names = TRUE)
  unlink(archivo_temp)
  
  # objetos creados
  # remove(list = c("archivo_temp", "cuestionario", "dir_temp", "url"))
  
  return(microdatos)
}

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
