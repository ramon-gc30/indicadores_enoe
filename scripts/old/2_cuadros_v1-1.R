# Insumos =====================================================================

# librerías
library(tidyverse)
library(here)
library(gt)

# datos procesados
tasas_tot <- read_rds(here::here("datos", "tasas_tot.rds"))
tasas_sub <- read_rds(here::here("datos", "tasas_sub.rds"))
pob_tot <- read_rds(here::here("datos", "pob_tot.rds"))
pob_sub <- read_rds(here::here("datos", "pob_sub.rds"))

# valores
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
tasas_tbl_gt <- tasas_tbl |> 
  gt() |> 
  tab_header(
    title = "Tasas complementarias, según sexo",
    subtitle = html("primer trimestre de 2025 y 2026 <br> (porcentaje)")
  ) |>
  tab_spanner(
    label = "Primer trimestre de 2026",
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
    source_note = "Fuente: INEGI. Encuesta Nacional de Ocupación y Empleo (ENOE), 2026."
  ) |>
  tab_options(
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold",
    footnotes.order = "preserve_order"
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
ent_tbl_gt <- ent_tbl |> 
  gt() |> 
  tab_header(
    title = html("Población y tasas complementarias de ocupación y desocupación, <br> según entidad federativa"),
    subtitle = html("primer trimestre de 2026 <br> (personas y porcentaje)")
  ) |>
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
  cols_label(
    valor = "Entidad federativa",
    ocupada = "Ocupada",
    desocupada = "Desocupada",
    tp = "Participación ",
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
  fmt_number(
    columns = ocupada:desocupada,
    decimals = 0
  ) |> 
  fmt_number(
    columns = -(valor:desocupada),
    decimals = 1,
    scale_by = 100
  ) |> 
  cols_align(columns = valor, align = "left") |> 
  cols_align(columns = ocupada:desocupada, align = "right") |> 
  cols_align(columns = starts_with("t"), align = "center") |> 
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
    source_note = "Fuente: INEGI. Encuesta Nacional de Ocupación y Empleo (ENOE), 2026."
  ) |>
  tab_options(
    heading.title.font.weight = "bold",
    column_labels.font.weight = "bold",
  )

# Áreas metropolitanas --------------------------

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
cd_tbl_gt <- cd_tbl |> 
  gt() |> 
  tab_header(
    title = html("Población y tasas complementarias de ocupación y desocupación, <br> según área metropolitana"),
    subtitle = html("primer trimestre de 2026 <br> (personas y porcentaje)")
  ) |>
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
  cols_label(
    valor = "Área metropolitana de la ciudad de:",
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
  fmt_number(
    columns = ocupada:desocupada,
    decimals = 0
  ) |> 
  fmt_number(
    columns = -(valor:desocupada),
    decimals = 1,
    scale_by = 100
  ) |> 
  cols_align(columns = valor, align = "left") |> 
  cols_align(columns = ocupada:desocupada, align = "right") |> 
  cols_align(columns = starts_with("t"), align = "center") |> 
  # text_transform(
  #   fn = function(x) {paste0("<strong>", x, "</strong>")},
  #   locations = cells_body(rows = 1)
  # ) |>
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
    source_note = "Fuente: INEGI. Encuesta Nacional de Ocupación y Empleo (ENOE), 2026."
  ) |>
  tab_options(
    heading.subtitle.font.weight = "bold",
    column_labels.font.weight = "bold",
    stub.font.weight = "bold"
  )

# Guardar =====================================================================

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

# tablas gt
write_rds(tasas_tbl_gt, here::here("cuadros", "tasas_tbl.rds"))
write_rds(ent_tbl_gt, here::here("cuadros", "ent_tbl.rds"))
write_rds(cd_tbl_gt, here::here("cuadros", "cd_tbl.rds"))
