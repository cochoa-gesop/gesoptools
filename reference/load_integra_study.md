# Carga un estudio de Integra y devuelve el data frame unificado listo para consumir directamente por el Shiny, sin transformaciones adicionales.

El data frame de salida sigue el contrato unificado:

- Variables categoricas -\> factor con etiquetas de texto como levels

- Variables numericas -\> numeric

- Variables de texto -\> character (incluyendo multiples con ";")

- Columnas admin: REGISTRO (character), FECHAFIN (Date), POND (numeric,
  1 por defecto), ESTUDI, MES, DATA

## Usage

``` r
load_integra_study(
  study_id,
  db_config,
  valid_states = c(1, 5, 6, 9),
  idioma = 1L,
  clean_html = TRUE,
  clean_instructions = TRUE,
  ns_nc_recode = TRUE
)
```

## Arguments

- study_id:

  Identificador del estudio (p.ej. "1824_POL_ARAGON")

- db_config:

  Lista con host, port, user, password, dbname

- valid_states:

  Codigos ESTADO validos (por defecto c(1,5,6,9))

- idioma:

  Codigo de idioma en variablescodigos (1 = principal)

- clean_html:

  Si TRUE, elimina entidades HTML de las etiquetas

- clean_instructions:

  Si TRUE, elimina textos de instruccion de entrevistador

- ns_nc_recode:

  Si TRUE, unifica variantes de NS/NC a "NS" y "NC"

## Value

Lista con: \$data data frame con el contrato unificado \$metadata lista
con \$variables (nomvar, label) y \$codebook (nomvar, value, label,
label_short)
