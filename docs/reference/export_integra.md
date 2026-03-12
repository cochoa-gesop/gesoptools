# Exporta datos de un estudio Integra a SPSS o Excel

Exporta datos de un estudio Integra a SPSS o Excel

## Usage

``` r
export_integra(
  study_id,
  db_config,
  format = NULL,
  output_dir = getwd(),
  valid_states = c(1, 5, 6, 9),
  clean_html = TRUE,
  strip_parens = FALSE,
  clean_instructions = TRUE,
  clean_specific_texts = NULL,
  include_admin = TRUE,
  multi_response = c("keep", "dummy", "split"),
  multi_drop_original = FALSE,
  var_filter = NULL,
  idioma = 1L,
  overwrite = TRUE
)
```

## Arguments

- study_id:

  Identificador del estudio, p.ej. "1824_POL_ARAGON"

- db_config:

  Lista con host, port, user, password, dbname

- format:

  "spss" o "excel"

- output_dir:

  Directorio de salida (por defecto: directorio de trabajo)

- valid_states:

  Codigos ESTADO validos (por defecto c(1,5,6,9))

- clean_html:

  Si TRUE, elimina etiquetas HTML y entidades de las etiquetas

- strip_parens:

  Si TRUE, elimina texto entre parentesis de las etiquetas

- clean_instructions:

  Si TRUE, elimina textos de instruccion de entrevistador definidos
  internamente (p.ej. "(no leer)"). Se aplica a preguntas y niveles
  antes de exportar.

- clean_specific_texts:

  Vector de textos adicionales a eliminar. Puede usarse con o sin
  clean_instructions.

- include_admin:

  Si TRUE, incluye variables administrativas (REGISTRO, POND, etc.)

- multi_response:

  Como tratar variables de respuesta multiple (valores separados por
  ";"). Opciones: "keep" - no hace nada, se exporta el texto tal cual
  (default) "dummy" - crea una variable dicotomica Si/No por cada nivel
  (nombre: VAR\_, ej. VOT_1, VOT_2...) "split" - divide en VAR_1,
  VAR_2... por orden de respuesta

- multi_drop_original:

  Si TRUE, elimina la variable original al expandir multiples. Por
  defecto FALSE (se conserva junto a las derivadas).

- var_filter:

  Vector de nomvar a incluir; NULL = todas las variables del codebook

- idioma:

  Codigo de idioma en variablescodigos (1 = principal, por defecto)

- overwrite:

  Si TRUE, sobreescribe ficheros existentes

## Value

Lista invisible con rutas de los ficheros generados
