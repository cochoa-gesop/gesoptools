# gesoptools

Package R de uso interno en GESOP para trabajar con la base de datos Integra.

## Funciones principales

| Funcion | Descripcion |
|---|---|
| `db_config_from_env()` | Lee las credenciales de Integra desde variables de entorno |
| `export_integra()` | Exporta un estudio a SPSS (.sav) o Excel (.xlsx) |
| `load_integra_study()` | Carga un estudio en R con el contrato unificado |
| `apply_variables_dict()` | Aplica un diccionario revisado a un fichero SPSS |

## Instalacion

```r
remotes::install_github("GESOP-org/gesoptools")
```

## Uso rapido

```r
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

- [Guia de inicio rapido](articles/quickstart.html)
- [Exportar a SPSS y Excel](articles/export.html)
- [Cargar datos en Shiny](articles/shiny.html)
- [Configurar credenciales](articles/credentials.html)
