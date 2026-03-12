# Construye db_config leyendo variables de entorno INTEGRA\_\*

Si se proporciona un fichero .Renviron (o se omite y existe uno en el
directorio de trabajo o en HOME), lo carga antes de leer las variables.

## Usage

``` r
db_config_from_env(renviron_path = NULL)
```

## Arguments

- renviron_path:

  Ruta al fichero .Renviron. NULL = buscar automaticamente.

## Value

Lista con host, port, user, password, dbname lista para pasar a
export_integra()
