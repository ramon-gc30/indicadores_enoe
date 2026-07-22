# Insumos =====================================================================
## librerías requeridas ----
if (require(pacman) == FALSE) { 
  install.packages("pacman") 
  library(pacman)
} 

pacman::p_load(tidyverse, srvyr, here, rlang)
# pacman::p_loaded()

source(here::here("scripts", "0_funciones_v2-1.R"))

## importación -----

url <- "https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/microdatos/enoe_2026_trim1_csv.zip"
cuestionario <- "SDEM"

sdem <- descargar_microdatos_enoe(url, cuestionario)

# Procedimiento ===============================================================

## limpieza ---- 
# para calcular tasas
sdem <- aplicar_criterio(sdem, fac_tri)

# encuesta compleja
sdem <- definir_encuesta(sdem, fac_tri)

# solamente las 39 ciudades
sdem_cd <- sdem |> 
  filter(
    cd_a != "81" & cd_a != "82" & cd_a != "83" &
      cd_a != "84" & cd_a != "85" & cd_a != "86"
  )

## valores de entrada ----
entrada_tasas_tot <- tribble(
  ~datos, ~nombre, ~filtro_num, ~filtro_den, 
  # ---
  "sdem", "tp", "clase1 == 1", "eda >= 15 & eda <= 98", 
  "sdem", "td", "clase2 == 2", "clase1 == 1", 
  "sdem", "topd", "clase2 == 2 | dur9c == 2", "clase1 == 1", 
  "sdem", "tpg", "clase2 == 2 | tpg_p8a == 1", "clase1 == 1", 
  "sdem", "tta", "remune2c == 1", "clase2 == 1", 
  "sdem", "tsub", "sub_o == 1", "clase2 == 1", 
  "sdem", "tcco", "tcco == 1 | tcco == 2 | tcco == 3", "clase2 == 1", 
  "sdem", "til1", "emp_ppal == 1", "clase2 == 1", 
  "sdem", "tosi1", "tue2 == 5", "clase2 == 1", 
  "sdem", "til2", "emp_ppal == 1 & ambito1 != 1", "clase2 == 1 & ambito1 != 1", 
  "sdem", "tosi2", "tue2 == 5", "clase2 == 1 & ambito1 != 1", 
  "sdem_cd", "tp", "clase1 == 1", "eda >= 15 & eda <= 98", 
  "sdem_cd", "td", "clase2 == 2", "clase1 == 1", 
  "sdem_cd", "topd", "clase2 == 2 | dur9c == 2", "clase1 == 1", 
  "sdem_cd", "tpg", "clase2 == 2 | tpg_p8a == 1", "clase1 == 1", 
  "sdem_cd", "tta", "remune2c == 1", "clase2 == 1", 
  "sdem_cd", "tsub", "sub_o == 1", "clase2 == 1", 
  "sdem_cd", "tcco", "tcco == 1 | tcco == 2 | tcco == 3", "clase2 == 1", 
  "sdem_cd", "til1", "emp_ppal == 1", "clase2 == 1", 
  "sdem_cd", "tosi1", "tue2 == 5", "clase2 == 1", 
  "sdem_cd", "til2", "emp_ppal == 1 & ambito1 != 1", "clase2 == 1 & ambito1 != 1", 
  "sdem_cd", "tosi2", "tue2 == 5", "clase2 == 1 & ambito1 != 1"
)

entrada_tasas_sub <- tribble(
  ~datos, ~nombre, ~grupo, ~filtro_num, ~filtro_den, 
  # ---
  "sdem", "tp", "sex", "clase1 == 1", "eda >= 15 & eda <= 98", 
  "sdem", "td", "sex", "clase2 == 2", "clase1 == 1", 
  "sdem", "topd", "sex", "clase2 == 2 | dur9c == 2", "clase1 == 1", 
  "sdem", "tpg", "sex", "clase2 == 2 | tpg_p8a == 1", "clase1 == 1", 
  "sdem", "tta", "sex", "remune2c == 1", "clase2 == 1", 
  "sdem", "tsub", "sex", "sub_o == 1", "clase2 == 1", 
  "sdem", "tcco", "sex", "tcco == 1 | tcco == 2 | tcco == 3", "clase2 == 1", 
  "sdem", "til1", "sex", "emp_ppal == 1", "clase2 == 1", 
  "sdem", "tosi1", "sex", "tue2 == 5", "clase2 == 1", 
  "sdem", "til2", "sex", "emp_ppal == 1 & ambito1 != 1", "clase2 == 1 & ambito1 != 1", 
  "sdem", "tosi2", "sex", "tue2 == 5", "clase2 == 1 & ambito1 != 1", 
  "sdem", "tp", "cve_ent", "clase1 == 1", "eda >= 15 & eda <= 98", 
  "sdem", "td", "cve_ent", "clase2 == 2", "clase1 == 1", 
  "sdem", "topd", "cve_ent", "clase2 == 2 | dur9c == 2", "clase1 == 1", 
  "sdem", "tpg", "cve_ent", "clase2 == 2 | tpg_p8a == 1", "clase1 == 1", 
  "sdem", "tta", "cve_ent", "remune2c == 1", "clase2 == 1", 
  "sdem", "tsub", "cve_ent", "sub_o == 1", "clase2 == 1", 
  "sdem", "tcco", "cve_ent", "tcco == 1 | tcco == 2 | tcco == 3", "clase2 == 1", 
  "sdem", "til1", "cve_ent", "emp_ppal == 1", "clase2 == 1", 
  "sdem", "tosi1", "cve_ent", "tue2 == 5", "clase2 == 1", 
  "sdem", "til2", "cve_ent", "emp_ppal == 1 & ambito1 != 1", "clase2 == 1 & ambito1 != 1", 
  "sdem", "tosi2", "cve_ent", "tue2 == 5", "clase2 == 1 & ambito1 != 1", 
  "sdem_cd", "tp", "cd_a", "clase1 == 1", "eda >= 15 & eda <= 98", 
  "sdem_cd", "td", "cd_a", "clase2 == 2", "clase1 == 1", 
  "sdem_cd", "topd", "cd_a", "clase2 == 2 | dur9c == 2", "clase1 == 1", 
  "sdem_cd", "tpg", "cd_a", "clase2 == 2 | tpg_p8a == 1", "clase1 == 1", 
  "sdem_cd", "tta", "cd_a", "remune2c == 1", "clase2 == 1", 
  "sdem_cd", "tsub", "cd_a", "sub_o == 1", "clase2 == 1", 
  "sdem_cd", "tcco", "cd_a", "tcco == 1 | tcco == 2 | tcco == 3", "clase2 == 1", 
  "sdem_cd", "til1", "cd_a", "emp_ppal == 1", "clase2 == 1", 
  "sdem_cd", "tosi1", "cd_a", "tue2 == 5", "clase2 == 1", 
  "sdem_cd", "til2", "cd_a", "emp_ppal == 1 & ambito1 != 1", "clase2 == 1 & ambito1 != 1", 
  "sdem_cd", "tosi2", "cd_a", "tue2 == 5", "clase2 == 1 & ambito1 != 1"
)

entrada_pob_tot <- tribble(
  ~datos, ~poblacion, ~filtro, 
  # ---
  "sdem", "pea", "clase1 == 1", 
  "sdem", "pnea", "clase1 == 2", 
  "sdem", "ocupada", "clase2 == 1", 
  "sdem", "desocupada", "clase2 == 2", 
  "sdem", "disponible", "clase2 == 3", 
  "sdem", "no disponible", "clase2 == 4", 
  "sdem_cd", "pea", "clase1 == 1", 
  "sdem_cd", "pnea", "clase1 == 2", 
  "sdem_cd", "ocupada", "clase2 == 1", 
  "sdem_cd", "desocupada", "clase2 == 2", 
  "sdem_cd", "disponible", "clase2 == 3", 
  "sdem_cd", "no disponible", "clase2 == 4"
)

entrada_pob_sub <- tribble(
  ~datos, ~poblacion, ~grupo, ~filtro, 
  # ---
  "sdem", "pea", "sex", "clase1 == 1", 
  "sdem", "pnea", "sex", "clase1 == 2", 
  "sdem", "ocupada", "sex", "clase2 == 1", 
  "sdem", "desocupada", "sex", "clase2 == 2", 
  "sdem", "disponible", "sex", "clase2 == 3", 
  "sdem", "no disponible", "sex", "clase2 == 4", 
  "sdem", "pea", "cve_ent", "clase1 == 1", 
  "sdem", "pnea", "cve_ent", "clase1 == 2", 
  "sdem", "ocupada", "cve_ent", "clase2 == 1", 
  "sdem", "desocupada", "cve_ent", "clase2 == 2", 
  "sdem", "disponible", "cve_ent", "clase2 == 3", 
  "sdem", "no disponible", "cve_ent", "clase2 == 4", 
  "sdem_cd", "pea", "cd_a", "clase1 == 1", 
  "sdem_cd", "pnea", "cd_a", "clase1 == 2", 
  "sdem_cd", "ocupada", "cd_a", "clase2 == 1", 
  "sdem_cd", "desocupada", "cd_a", "clase2 == 2", 
  "sdem_cd", "disponible", "cd_a", "clase2 == 3", 
  "sdem_cd", "no disponible", "cd_a", "clase2 == 4"
)

## cálculo ----

# cálculo de tasas
tasas_tot <- pmap(entrada_tasas_tot, calcular_tasas_tot) |> 
  list_rbind()

tasas_sub <- pmap(entrada_tasas_sub, calcular_tasas_sub) |> 
  list_rbind()

# estimación de totales
pob_tot <- pmap(entrada_pob_tot, calcular_pob_tot) |> 
  list_rbind()

pob_sub <- pmap(entrada_pob_sub, calcular_pob_sub) |> 
  list_rbind()

# niveles de precisión
pob_sub <- validar_precision(pob_sub, "tot_cv")

# Exportación =================================================================
write_rds(tasas_tot, here::here("datos", "tasas_tot.rds"))
write_rds(tasas_sub, here::here("datos", "tasas_sub.rds"))
write_rds(pob_tot, here::here("datos", "pob_tot.rds"))
write_rds(pob_sub, here::here("datos", "pob_sub.rds"))