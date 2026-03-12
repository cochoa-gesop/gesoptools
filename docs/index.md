# gesoptools

Package R de uso interno en GESOP para trabajar con la base de datos
Integra.

## Funciones principales

| Funcion | Descripcion |
|----|----|
| [`db_config_from_env()`](https://cochoa-gesop.github.io/gesoptools/es/reference/db_config_from_env.md) | Lee las credenciales de Integra desde variables de entorno |
| [`export_integra()`](https://cochoa-gesop.github.io/gesoptools/es/reference/export_integra.md) | Exporta un estudio a SPSS (.sav) o Excel (.xlsx) |
| [`load_integra_study()`](https://cochoa-gesop.github.io/gesoptools/es/reference/load_integra_study.md) | Carga un estudio en R con el contrato unificado |
| [`apply_variables_dict()`](https://cochoa-gesop.github.io/gesoptools/es/reference/apply_variables_dict.md) | Aplica un diccionario revisado a un fichero SPSS |

## Instalacion

``` r
remotes::install_github("cochoa-gesop/gesoptools")
```

## Uso rapido

``` r
library(gesoptools)

# Configurar credenciales (una sola vez, en .Renviron)
# INTEGRA_HOST=...
# INTEGRA_USER=...
# INTEGRA_PASSWORD=...

db_config <- db_config_from_env()

# Exportar a SPSS
export_integra(
  study_id  = "1824_POL_ARAGON",
  db_config = db_config,
  format    = "spss"
)

# Cargar en R
estudio <- load_integra_study(
  study_id  = "1824_POL_ARAGON",
  db_config = db_config
)
datos <- estudio$data
```

## Documentacion

- [Guia de inicio
  rapido](https://cochoa-gesop.github.io/gesoptools/es/articles/quickstart.md)
- [Exportar a SPSS y
  Excel](https://cochoa-gesop.github.io/gesoptools/es/articles/export.md)
- [Cargar datos en
  Shiny](https://cochoa-gesop.github.io/gesoptools/es/articles/shiny.md)
- [Configurar
  credenciales](https://cochoa-gesop.github.io/gesoptools/es/articles/credentials.md)
