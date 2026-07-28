# Exportar a SPSS y Excel

## La funcion export_integra()

[`export_integra()`](https://gesop-org.github.io/gesoptools/es/reference/export_integra.md)
conecta a la BD Integra, descarga los datos de un estudio y los exporta
a SPSS o Excel con todas las etiquetas correctamente aplicadas.

Ademas del fichero de datos, siempre genera un fichero
`variables_<study_id>.xlsx` con dos pestanas:

- **preguntas**: lista de variables con su etiqueta de pregunta
- **niveles**: lista de codigos con su etiqueta de respuesta

Este fichero puede editarse a mano y luego aplicarse de nuevo al SPSS
con
[`apply_variables_dict()`](https://gesop-org.github.io/gesoptools/es/reference/apply_variables_dict.md).

## Exportacion basica

``` r
library(gesoptools)
db_config <- db_config_from_env()

# Solo SPSS
export_integra(
  study_id  = "1824_POL_ARAGON",
  db_config = db_config,
  format    = "spss"
)

# Solo Excel
export_integra(
  study_id  = "1824_POL_ARAGON",
  db_config = db_config,
  format    = "excel"
)

# Los dos a la vez
export_integra(
  study_id  = "1824_POL_ARAGON",
  db_config = db_config
)
```

## Directorio de salida

``` r
export_integra(
  study_id   = "1824_POL_ARAGON",
  db_config  = db_config,
  format     = "spss",
  output_dir = "C:/Estudios/exportados"
)
```

## Limpiar etiquetas

Por defecto se eliminan etiquetas HTML y textos de instruccion de
entrevistador como “(no leer)”. Se puede desactivar:

``` r
export_integra(
  study_id           = "1824_POL_ARAGON",
  db_config          = db_config,
  format             = "spss",
  clean_html         = FALSE,
  clean_instructions = FALSE
)
```

Para eliminar textos adicionales especificos:

``` r
export_integra(
  study_id             = "1824_POL_ARAGON",
  db_config            = db_config,
  format               = "spss",
  clean_specific_texts = c("(espontanea)", "(multirrespuesta)")
)
```

## Variables de respuesta multiple

Las variables con respuestas multiples (valores separados por “;”) se
pueden tratar de tres formas:

``` r
# Mantener tal cual (por defecto)
export_integra(..., multi_response = "keep")

# Crear una variable dummy Si/No por cada nivel
export_integra(..., multi_response = "dummy")

# Dividir en VAR_1, VAR_2... por orden de respuesta
export_integra(..., multi_response = "split")

# Dividir y eliminar la variable original
export_integra(..., multi_response = "split", multi_drop_original = TRUE)
```

## Filtrar variables

Para exportar solo un subconjunto de variables:

``` r
export_integra(
  study_id   = "1824_POL_ARAGON",
  db_config  = db_config,
  format     = "spss",
  var_filter = c("SEXE", "EDAT", "P1", "P2", "P3")
)
```

## Incluir o excluir variables administrativas

``` r
# Sin variables admin (REGISTRO, POND, FECHAFIN...)
export_integra(..., include_admin = FALSE)
```

## Unificar NS/NC

``` r
# Unifica "No sabe", "No sap", "No recuerda" -> "NS"
# y "No contesta" -> "NC"
export_integra(..., ns_nc_recode = TRUE)
```
