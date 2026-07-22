# Insumos =====================================================================

## Librerías -------------------------------------
if (require(pacman) == FALSE) {
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(tidyverse, here, gt, writexl)

source(here::here("scripts", "0_funciones_v3-1.R"))

## Datos procesados -----------------------------
datos_proc <- read_rds(here::here("datos", "datos_proc.rds"))

pob_tot <- datos_proc[[1]]
pob_sub <- datos_proc[[2]]
tasas_tot <- datos_proc[[3]]
tasas_sub <- datos_proc[[4]]

## Valores --------------------------------------
tasas_label <- c(
  "Tasa de participación" = "tp",
  "Tasa de desocupación (TD)" = "td",
  "Tasa de ocupación parcial y desocupación (TOPD1)" = "topd",
  "Tasa de presión general (TPRG)" = "tpg",
  "Tasa de trabajo asalariado" = "tta",
  "Tasa de subocupación" = "tsub",
  "Tasa de condiciones críticas de ocupación (TCCO)" = "tcco",
  "Tasa de informalidad laboral 1 (TIL1)" = "til1",
  "Tasa de ocupación en el sector informal 1 (TOSI1)" = "tosi1",
  "Tasa de informalidad laboral 2 (TIL2)" = "til2",
  "Tasa de ocupación en el sector informal 2 (TOSI2)" = "tosi2"
)

estados <- c(
  "Estados Unidos Mexicanos" = "sdem",
  "Aguascalientes" = "01",
  "Baja California" = "02",
  "Baja California Sur" = "03",
  "Campeche" = "04",
  "Coahuila de Zaragoza" = "05",
  "Colima" = "06",
  "Chiapas" = "07",
  "Chihuahua" = "08",
  "Ciudad de México" = "09",
  "Durango" = "10",
  "Guanajuato" = "11",
  "Guerrero" = "12",
  "Hidalgo" = "13",
  "Jalisco" = "14",
  "México" = "15",
  "Michoacán de Ocampo" = "16",
  "Morelos" = "17",
  "Nayarit" = "18",
  "Nuevo León" = "19",
  "Oaxaca" = "20",
  "Puebla" = "21",
  "Querétaro" = "22",
  "Quintana Roo" = "23",
  "San Luis Potosí" = "24",
  "Sinaloa" = "25",
  "Sonora" = "26",
  "Tabasco" = "27",
  "Tamaulipas" = "28",
  "Tlaxcala" = "29",
  "Veracruz de Ignacio de la Llave" = "30",
  "Yucatán" = "31",
  "Zacatecas" = "32"
)

ciudades <- c(
  # "Agregado 39 ciudades" = "sdem_cd",
  "Ciudad de México (CDMX y Méx.)" = "1",
  "Guadalajara (Jal.)" = "2",
  "Monterrey (N. L.)" = "3",
  "Puebla (Pue.)" = "4",
  "León (Gto.)" = "5",
  "Torreón - La Laguna (Coah. y Dgo.)" = "6",
  "San Luis Potosí (S. L. P.)" = "7",
  "Mérida (Yuc.)" = "8",
  "Chihuahua (Chih.)" = "9",
  "Tampico (Tamps. y Ver.)" = "10",
  "Veracruz (Ver.)" = "12",
  "Acapulco de Juárez (Gro.)" = "13",
  "Aguascalientes (Ags.)" = "14",
  "Morelia (Mich.)" = "15",
  "Toluca (Méx.)" = "16",
  "Saltillo (Coah.)" = "17",
  "Villahermosa (Tab.)" = "18",
  "Tuxtla Gutiérrez (Chis.)" = "19",
  "Ciudad Juárez (Chih.)" = "20",
  "Tijuana (B. C.)" = "21",
  "Culiacán (Sin.)" = "24",
  "Hermosillo (Son.)" = "25",
  "Durango (Dgo.)" = "26",
  "Tepic (Nay.)" = "27",
  "Campeche (Camp.)" = "28",
  "Cuernavaca (Mor.)" = "29",
  "Coatzacoalcos (Ver.)" = "30",
  "Oaxaca (Oax.)" = "31",
  "Zacatecas (Zac.)" = "32",
  "Colima (Col.)" = "33",
  "Querétaro (Qro.)" = "36",
  "Tlaxcala (Tlax.)" = "39",
  "La Paz (B. C. S.)" = "40",
  "Cancún (Q. Roo)" = "41",
  "Ciudad del Carmen (Camp.)" = "42",
  "Pachuca (Hgo.)" = "43",
  "Mexicali (B. C.)" = "44",
  "Reynosa (Tamps.)" = "46",
  "Tapachula (Chis.)" = "52"
)

cd_cve <- tribble(
  ~valor, ~cve_ent,
  # ---
  "Aguascalientes (Ags.)", 1,
  "Mexicali (B. C.)", 2,
  "Tijuana (B. C.)", 2,
  "La Paz (B. C. S.)", 3,
  "Campeche (Camp.)", 4,
  "Ciudad del Carmen (Camp.)", 4,
  "Torreón - La Laguna (Coah. y Dgo.)", 5,
  "Saltillo (Coah.)", 5,
  "Colima (Col.)", 6,
  "Tapachula (Chis.)", 7,
  "Tuxtla Gutiérrez (Chis.)", 7,
  "Chihuahua (Chih.)", 8,
  "Ciudad Juárez (Chih.)", 8,
  "Ciudad de México (CDMX y Méx.)", 9,
  "Durango (Dgo.)", 10,
  "León (Gto.)", 11,
  "Acapulco de Juárez (Gro.)", 12,
  "Pachuca (Hgo.)", 13,
  "Guadalajara (Jal.)", 14,
  "Toluca (Méx.)", 15,
  "Morelia (Mich.)", 16,
  "Cuernavaca (Mor.)", 17,
  "Tepic (Nay.)", 18,
  "Monterrey (N. L.)", 19,
  "Oaxaca (Oax.)", 20,
  "Puebla (Pue.)", 21,
  "Querétaro (Qro.)", 22,
  "Cancún (Q. Roo)", 23,
  "San Luis Potosí (S. L. P.)", 24,
  "Culiacán (Sin.)", 25,
  "Hermosillo (Son.)", 26,
  "Villahermosa (Tab.)", 27,
  "Tampico (Tamps. y Ver.)", 28,
  "Reynosa (Tamps.)", 28,
  "Tlaxcala (Tlax.)", 29,
  "Coatzacoalcos (Ver.)", 30,
  "Veracruz (Ver.)", 30,
  "Mérida (Yuc.)", 31,
  "Zacatecas (Zac.)", 32
)

# Procedimiento ===============================================================

## Tasas por sexo -------------------------------

# Género
tasas_sex <- tasas_sub |> 
  filter(grupo == "sex") |> 
  pivot_wider(
    id_cols = nombre,
    names_from = valor,
    names_prefix = "sex_",
    # names_sort = TRUE,
    values_from = tasa
  )

# Unión
tasas_tbl <- tasas_tot |> 
  filter(datos == "sdem") |> 
  left_join(tasas_sex, by = "nombre") |> 
  mutate(nombre = fct_recode(nombre, !!!tasas_label))

# tabla final
tasas_tbl_gt <- 
  generar_cuadro_tasas_tot(
    tasas_tbl,
    trimestre = "Primer",
    periodo_actual = 2026
  )

## Entidades federativas -------------------------

# desagregado
ent_sub_pob <- pob_sub |> 
  filter(
    grupo == "cve_ent" & 
      (poblacion == "ocupada" | poblacion == "desocupada")
  ) |> 
  pivot_wider(
    id_cols = valor,
    names_from = poblacion,
    values_from = tot
  )

ent_sub_tasas <- tasas_sub |> 
  filter(grupo == "cve_ent") |> 
  pivot_wider(
    id_cols = valor,
    names_from = nombre,
    values_from = tasa
  )

# agregado
ent_tot_pob <- pob_tot |> 
  filter(
    datos == "sdem" & 
      (poblacion == "ocupada" | poblacion == "desocupada")
  ) |> 
  pivot_wider(
    id_cols = datos,
    names_from = poblacion,
    values_from = tot
  )

ent_tot_tasas <- tasas_tot |> 
  filter(datos == "sdem") |> 
  pivot_wider(
    id_cols = datos,
    names_from = nombre,
    values_from = tasa
  )

ent_tot <- full_join(ent_tot_pob, ent_tot_tasas, by = "datos") |>
  rename("valor" = "datos")

# unión
ent_tbl <- full_join(ent_sub_pob, ent_sub_tasas, by = "valor") |> 
  add_row(ent_tot, .before = 1) |> 
  mutate(valor = fct_recode(valor, !!!estados))

# tabla final
ent_tbl_gt <- 
  generar_cuadros_tasas_sub(
    ent_tbl,
    grupo = "entidades",
    trimestre = "Primer",
    periodo_actual = 2026
  )

## Áreas metropolitanas -------------------------

# desagregado
cd_sub_pob <- pob_sub |> 
  filter(
    grupo == "cd_a" & 
      (poblacion == "ocupada" | poblacion == "desocupada")
  ) |> 
  pivot_wider(
    id_cols = valor,
    names_from = poblacion,
    values_from = tot
  )

cd_sub_tasas <- tasas_sub |>
  filter(grupo == "cd_a") |>
  pivot_wider(
    id_cols = valor,
    names_from = nombre,
    values_from = tasa
  )

# agregado
cd_tot_pob <- pob_tot |> 
  filter(
    datos == "sdem_cd" &
      (poblacion == "ocupada" | poblacion == "desocupada")
  ) |>
  pivot_wider(
    id_cols = datos,
    names_from = poblacion,
    values_from = tot
  )

cd_tot_tasas <- tasas_tot |>
  filter(datos == "sdem_cd") |>
  pivot_wider(
    id_cols = datos,
    names_from = nombre,
    values_from = tasa
  )

cd_tot <- full_join(cd_tot_pob, cd_tot_tasas, by = "datos") |> 
  rename("valor" = "datos")

# unión
cd_tbl <- full_join(cd_sub_pob, cd_sub_tasas, by = "valor") |> 
  mutate(valor = fct_recode(valor, !!!ciudades)) |> 
  left_join(cd_cve, by = c("valor")) |>
  arrange(cve_ent, valor) |> 
  add_row(cd_tot, .before = 1) |> 
  mutate(valor = if_else(valor == "sdem_cd", "Agregado 39 ciudades", valor)) |> 
  select(-cve_ent)

# tabla final
cd_tbl_gt <- 
  generar_cuadros_tasas_sub(
    cd_tbl,
    grupo = "ciudades",
    trimestre = "Primer",
    periodo_actual = 2026
  )

# Guardar =====================================================================

# formato Word
# por sexo
gt::gtsave(
  data = tasas_tbl_gt,
  filename = "tasas_sex.docx",
  path = here::here("cuadros")
)

# entidades federativas
gt::gtsave(
  data = ent_tbl_gt,
  filename = "pob_y_tasas_ent.docx",
  path = here::here("cuadros")
)

# áreas metropolitanas
gt::gtsave(
  data = cd_tbl_gt,
  filename = "pob_y_tasas_cd.docx",
  path = here::here("cuadros")
)

# write_rds(tbls_sn_fmt, here::here("cuadros", "tbls_sn_fmt.rds"))
# tablas gt
write_rds(tasas_tbl_gt, here::here("cuadros", "tasas_tbl.rds"))
write_rds(ent_tbl_gt, here::here("cuadros", "ent_tbl.rds"))
write_rds(cd_tbl_gt, here::here("cuadros", "cd_tbl.rds"))

# tablas sin formato
tbls_sn_fmt <- list(tasas_tbl, ent_tbl, cd_tbl)

write_xlsx(
  list(
    "tasas_tbl" = tbls_sn_fmt[[1]],
    "ent_tbl" = tbls_sn_fmt[[2]],
    "cd_tbl" = tbls_sn_fmt[[3]]
  ),
  path = here::here("datos", "tbls_proc.xlsx")
)
