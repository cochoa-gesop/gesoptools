# Lee un fichero SPSS y le aplica un diccionario de variables revisado

El diccionario revisado es el Excel generado por export_integra() que el
usuario ha podido editar a mano. La funcion:

1.  Preserva solo las variables presentes en la pestana "preguntas"

2.  Aplica las etiquetas de variable (variable label) de esa pestana

3.  Corrige las etiquetas de niveles (value labels) segun la pestana
    "niveles"

4.  Guarda el resultado como un nuevo fichero SPSS

## Usage

``` r
apply_variables_dict(
  spss_path,
  variables_path,
  output_path = NULL,
  sheet_preguntas = "preguntas",
  sheet_niveles = "niveles",
  na_codes = c(97, 98, 99)
)
```

## Arguments

- spss_path:

  Ruta al fichero .sav original

- variables_path:

  Ruta al Excel de variables revisado (pestanas "preguntas" y "niveles")

- output_path:

  Ruta del .sav de salida. Si NULL, sobreescribe el original.

- sheet_preguntas:

  Nombre de la pestana con variables y etiquetas (default "preguntas")

- sheet_niveles:

  Nombre de la pestana con niveles (default "niveles")

- na_codes:

  Vector de codigos numericos a tratar como NA del sistema (por defecto
  c(97, 98, 99))

## Value

Ruta al fichero .sav generado (invisible)
