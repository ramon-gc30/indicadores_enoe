---
editor_options: 
  markdown: 
    wrap: 72
---

# Automatización de indicadores de la ENOE

Este proyecto tiene como objetivo crear los scripts que permitan automatizar:

- El cálculo de las tasas complementarias de ocupación y desocupación que reporta el INEGI
- Los cuadros estadísticos de los informes referentes a la ENOE

Dichas tasas son:

- Tasa de participación (TP)
- Tasa de desocupación (TD)
- Tasa de ocupación parcial y desocupación (TOPD)
- Tasa de presión general (TPRG)
- Tasa de trabajo asalariado (TTA)
- Tasa de subocupación (TSUB)
- Tasa de condiciones críticas de ocupación (TCCO)
- Tasa de informalidad laboral 1 (TIL1)
- Tasa de ocupación en el sector informal 1 (TOSI1)
- Tasa de informalidad laboral 2 (TIL2)
- Tasa de ocupación en el sector informal 2 (TOSI2)

Por su parte, los cuadros estadísticos son:

- Cuadro 4. Tasas complementarias, según sexo
- Cuadro 5. Población y tasas complementarias de ocupación y desocupación, según entidad federativa
- Cuadro 6. Población y tasas complementarias de ocupación y desocupación, según área metropolitana

Para lo cual, se utilizarán los microdatos de la ENOE correspondientes al primer trimestre de 2026 y adicionalmente los siguientes recursos:

- INEGI (2026). Contiene el informe de la ENOE correspondiente al primer trimestre de 2026
- INEGI (2025; 2024). Contiene los metadatos de los microdatos de la ENOE[^1]
- INEGI (2023b). Describe las fórmulas para calcular las tasas complementarias de ocupación y desocupación
- INEGI (2023a). Contiene los umbrales del coeficiente de variación que determinan los niveles de precisión de la estimación de los totales.

Se observa que este proyecto considera el diseño muestral de la ENOE que se caracteriza por ser probabilístico y, a su vez, bietápico, estratificado y por conglomerados, donde la unidad última de observación es la persona que al momento de la entrevista tenga 15 años cumplidos o más edad (INEGI, 2023a, p. 42).

Cabe subrayar que este proyecto todavía se encuentra en proceso de desarrollo. En su versión actual, está estructurado de la siguiente manera:

- La carpeta `bitacora` se encuentran los documentos que sirven como notas de laboratorio,
- En `datos` están los datos resultantes de la ejecución de los scripts,
- En `scripts` están, por un lado, las versiones previas de los códigos fuente (ubicados en `old`) y la versión actual

El archivo `0_funciones_v2-1.R` contiene un conjunto de funciones que permiten:

- Descargar los microdatos de la ENOE, ya sea un módulo (cuestionario) o todos, directamente desde el sitio web del INEGI,
- Calcular las tasas complementarias a nivel general o desagregada por grupos poblacionales,
- Estimar la población total o subtotal,
- Definir un conjunto de datos como encuesta compleja,
- Determinar los niveles de precisión de las estimaciones de los totales

Por otro lado, el archivo `1_preparacion_datos_v1-1.R` lleva a cabo el proceso de limpieza y manipulación de datos que permite obtener los siguientes datos procesados: 
* Tasas complementarias a nivel general y desagregada por género, entidad federativa y áreas metropolitanas, 
* Población total y subtotal por grupos poblacionales descritos líneas arriba, donde población se hace referencia a: 
  * Población Económicamente Activa (PEA) 
  * Población Económicamente No Activa (PNEA) 
  * Población ocupada 
  * Población desocupada 
  * Población disponible 
  * Población no disponible
  
De esta manera, se espera que este proyecto agilice la obtención de datos referentes al mercado laboral de México, ya sea a nivel nacional o desagregada por grupos poblacionales.

[^1]: se utiliza el diccionario de datos de la ENOE 1T-2025 dado que, al momento de la realización de este proyecto, el INEGI aún no publica los metadatos correspondientes al primer trimestre de 2026.

# Referencias

INEGI, [Instituto Nacional de Estadística y Geografía]. (2023a). Cómo se hace la ENOE. Métodos y procedimientos (3ra ed.). <https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=889463909743>
INEGI, [Instituto Nacional de Estadística y Geografía]. (2023b). Encuesta Nacional de Ocupación y Empleo. ENOE. Conociendo la base de datos. <https://www.inegi.org.mx/rnm/index.php/catalog/1121/related-materials>
INEGI, [Instituto Nacional de Estadística y Geografía]. (2024). Encuesta Nacional de Ocupación y Empleo (ENOE). Estructura de la base de datos. <https://www.inegi.org.mx/contenidos/programas/enoe/15ymas/doc/enoe_123_fd_c_bas_amp.pdf>
INEGI, [Instituto Nacional de Estadística y Geografía]. (2025). Red Nacional de Metadatos. Encuesta Nacional de Ocupación y Empleo 2025, Cuestionario ampliado, datos correspondientes al primer trimestre. <https://www.inegi.org.mx/rnm/index.php/catalog/1104/data-dictionary>
INEGI, [Instituto Nacional de Estadística y Geografía]. (2026). Sumó 61.1 millones de personas la población económicamente activa en el primer trimestre de 2026: 622 mil más respecto al mismo trimestre de un año antes (Boletín de Indicador No. 301/26; Encuesta Nacional de Ocupación y Empleo (ENOE), población de 15 años y más de edad. Primer trimestre de 2026). <https://www.inegi.org.mx/contenidos/saladeprensa/boletines/2026/enoe/enoe2026_05.pdf>

