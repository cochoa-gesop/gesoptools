# Package index

## Conexion y configuracion

Funciones para configurar y verificar la conexion a la BD Integra.

- [`db_config_from_env()`](https://cochoa-gesop.github.io/gesoptools/es/reference/db_config_from_env.md)
  : Construye db_config leyendo variables de entorno INTEGRA\_\*

## Exportacion de datos

Exportar un estudio a fichero SPSS (.sav) o Excel (.xlsx).

- [`export_integra()`](https://cochoa-gesop.github.io/gesoptools/es/reference/export_integra.md)
  : Exporta datos de un estudio Integra a SPSS o Excel

## Carga de datos

Cargar datos de un estudio directamente en R con el contrato unificado.

- [`load_integra_study()`](https://cochoa-gesop.github.io/gesoptools/es/reference/load_integra_study.md)
  : Carga un estudio de Integra y devuelve el data frame unificado listo
  para consumir directamente por el Shiny, sin transformaciones
  adicionales.

## Utilidades SPSS

Aplicar un diccionario de variables revisado a un fichero SPSS
existente.

- [`apply_variables_dict()`](https://cochoa-gesop.github.io/gesoptools/es/reference/apply_variables_dict.md)
  : Lee un fichero SPSS y le aplica un diccionario de variables revisado
