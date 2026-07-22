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

# cd_mayor_mil <- sdem |> 
#   mutate(cd_a = as.integer(cd_a)) |> 
#   filter(cd_a < 81) |> 
#   group_by(cd_a) |> 
#   summarise(
#     tot = sum(fac_tri)
#   ) |> 
#   filter(tot > 100000) |> 
#   mutate(
#     cd_a_mayor_mil = 1,
#     cd_a = as.character(cd_a)
#   ) |> 
#   select(-tot)
# 
# sdem <- sdem |> 
#   left_join(cd_mayor_mil, by = "cd_a")