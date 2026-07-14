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
# 2. Aplicar criterio general (INEGI, 2023:28):
# sdem |> 
#   filter(r_def == 0, c_res == 1 | c_res == 3, between(eda, 15, 98))


# tasas complementarias a nivel general
calcular_tasas_gen <- function(datos, nombre, ponderador, filtro_num, filtro_den){
  datos <- get(datos)
  ponderador_sym <- sym(ponderador)
  
  datos |>
    mutate(ponderador = as.double(  !!ponderador_sym  )) |>
    summarise(
      nombre = nombre,
      numerador = sum(ponderador[  !!parse_expr(filtro_num)  ], na.rm = TRUE),
      denominador = sum(ponderador[  !!parse_expr(filtro_den)  ], na.rm = TRUE),
      tasa = numerador / denominador
    )
}

# tasas complementarias desagregadas
calcular_tasas_sub <- function(datos, nombre, ponderador, grupo, filtro_num, filtro_den){
  datos <- get(datos)
  ponderador_sym <- sym(ponderador)
  grupo_sym <- sym(grupo)
  
  datos |>
    mutate(ponderador = as.double(  !!ponderador_sym  )) |>
    group_by(  !!grupo_sym  ) |> 
    summarise(
      nombre = nombre,
      numerador = sum(ponderador[  !!parse_expr(filtro_num)  ], na.rm = TRUE),
      denominador = sum(ponderador[  !!parse_expr(filtro_den)  ], na.rm = TRUE),
      tasa = numerador / denominador
    )
}

# # Tamaño de la muestra
# calcular_total <- function(datos){
#   datos |> 
#     summarise(
#       n = n(),
#       tot = survey_total(vartype = NULL)
#     )
# }
# 
# # Variables dicotómicas o categóricas
# calcular_prop <- function(datos, variable){
#   datos |>
#     group_by({{  variable  }}) |>
#     summarise(
#       n = n(),
#       tot = survey_total(vartype = c("se", "cv", "ci")),
#       prop = survey_prop(vartype = NULL)
#     )
# }
# 
# # se define como encuesta compleja
# definir_encuesta <- function(datos){
#   datos_procesados <- datos |> 
#     as_survey_design(
#       ids = upm,
#       strata = est,
#       weights = fac_tri,
#       nest = TRUE
#     )
#   return(datos_procesados)
# }
# 
# validar_precision_map <- function(datos, estimador_cv){
#   datos <- get(datos)
#   cv_sym <- sym(estimador_cv)
#   
#   datos |> 
#     mutate(
#       nivel_precision = case_when(
#         !!cv_sym <= 0.15 ~ "Alta",
#         !!cv_sym > 0.15 & !!cv_sym <= 0.30 ~ "Moderada",
#         !!cv_sym > 0.30 ~ "Baja",
#         TRUE ~ NA
#       )
#     )
# }