# =============================================================================
# integra_export.R
# Exportaci\u00F3n de datos desde la BD Integra
#
# Funci\u00F3n principal:
#   export_integra(study_id, db_config, format, ...)
#
# Formatos soportados:
#   "spss"  -> fichero .sav perfectamente etiquetado
#   "excel" -> fichero .xlsx con pesta\u00F1a de c\u00F3digos y pesta\u00F1a de etiquetas
#
# Siempre genera adem\u00E1s un fichero "variables_<study_id>.xlsx" con:
#   - Pesta\u00F1a "preguntas": nomvar + etiqueta de la pregunta
#   - Pesta\u00F1a "niveles":   nomvar + c\u00F3digo + etiqueta del nivel
#
# Uso m\u00EDnimo:
#   source("integra_export.R")
#   export_integra("1824_POL_ARAGON", db_config, format = "spss")
#   export_integra("1824_POL_ARAGON", db_config, format = "excel")
# =============================================================================


# -----------------------------------------------------------------------------
# Note: uses native R pipe |> (requires R >= 4.1)
# -----------------------------------------------------------------------------

# Suppress R CMD check notes for dplyr column names used in NSE contexts
utils::globalVariables(c(
  ".",
  "ESTADO", "nomvar", "txtext", "nomcod", "defcod",
  "REGISTRO", "FECHAFIN", "POND", "ESTUDI",
  "label", "value", "tipo"
))



# -----------------------------------------------------------------------------
# Helper: construir db_config desde variables de entorno
# -----------------------------------------------------------------------------

#' Construye db_config leyendo variables de entorno INTEGRA_*
#'
#' Si se proporciona un fichero .Renviron (o se omite y existe uno en el
#' directorio de trabajo o en HOME), lo carga antes de leer las variables.
#'
#' @param renviron_path Ruta al fichero .Renviron. NULL = buscar automaticamente.
#' @return Lista con host, port, user, password, dbname lista para pasar a export_integra()
#' @export
db_config_from_env <- function(renviron_path = NULL) {

  # Cargar .Renviron si se indica o si existe en ubicaciones est\u00E1ndar
  if (!is.null(renviron_path)) {
    if (!file.exists(renviron_path))
      stop("Fichero .Renviron no encontrado: ", renviron_path)
    readRenviron(renviron_path)
  } else {
    for (candidate in unique(c(
      file.path(dirname(normalizePath("integra_export.R", mustWork = FALSE)), ".Renviron"),
      file.path(getwd(), ".Renviron"),
      file.path(Sys.getenv("HOME"), ".Renviron")
    ))) {
      if (file.exists(candidate)) { readRenviron(candidate); break }
    }
  }

  host     <- Sys.getenv("INTEGRA_HOST")
  user     <- Sys.getenv("INTEGRA_USER")
  password <- Sys.getenv("INTEGRA_PASSWORD")
  dbname   <- Sys.getenv("INTEGRA_DBNAME", unset = "integra4")
  port     <- suppressWarnings(as.integer(Sys.getenv("INTEGRA_PORT", unset = "3306")))

  missing <- c(
    if (!nzchar(host))     "INTEGRA_HOST",
    if (!nzchar(user))     "INTEGRA_USER",
    if (!nzchar(password)) "INTEGRA_PASSWORD"
  )
  if (length(missing) > 0)
    stop("Variables de entorno no definidas: ", paste(missing, collapse = ", "),
         "\nDef\u00EDnelas en tu fichero .Renviron o en el entorno del sistema.")

  list(host = host, port = port, user = user, password = password, dbname = dbname)
}


# -----------------------------------------------------------------------------
# Funci\u00F3n principal
# -----------------------------------------------------------------------------

#' Exporta datos de un estudio Integra a SPSS o Excel
#'
#' @param study_id        Identificador del estudio, p.ej. "1824_POL_ARAGON"
#' @param db_config       Lista con host, port, user, password, dbname
#' @param format          "spss" o "excel"
#' @param output_dir      Directorio de salida (por defecto: directorio de trabajo)
#' @param valid_states    Codigos ESTADO validos (por defecto c(1,5,6,9))
#' @param clean_html           Si TRUE, elimina etiquetas HTML y entidades de las etiquetas
#' @param strip_parens         Si TRUE, elimina texto entre parentesis de las etiquetas
#' @param clean_instructions   Si TRUE, elimina textos de instruccion de entrevistador
#'                             definidos internamente (p.ej. "(no leer)"). Se aplica
#'                             a preguntas y niveles antes de exportar.
#' @param clean_specific_texts Vector de textos adicionales a eliminar. Puede usarse
#'                             con o sin clean_instructions.
#' @param include_admin        Si TRUE, incluye variables administrativas (REGISTRO, POND, etc.)
#' @param multi_response       Como tratar variables de respuesta multiple (valores separados
#'                             por ";"). Opciones:
#'                             "keep"   - no hace nada, se exporta el texto tal cual (default)
#'                             "dummy"  - crea una variable dicotomica Si/No por cada nivel
#'                                        (nombre: VAR_<codigo>, ej. VOT_1, VOT_2...)
#'                             "split"  - divide en VAR_1, VAR_2... por orden de respuesta
#' @param multi_drop_original  Si TRUE, elimina la variable original al expandir multiples.
#'                             Por defecto FALSE (se conserva junto a las derivadas).
#' @param var_filter           Vector de nomvar a incluir; NULL = todas las variables del codebook
#' @param idioma               Codigo de idioma en variablescodigos (1 = principal, por defecto)
#' @param overwrite            Si TRUE, sobreescribe ficheros existentes
#'
#' @return Lista invisible con rutas de los ficheros generados
#' @export
export_integra <- function(study_id,
                           db_config,
                           format               = NULL,
                           output_dir           = getwd(),
                           valid_states         = c(1, 5, 6, 9),
                           clean_html           = TRUE,
                           strip_parens         = FALSE,
                           clean_instructions   = TRUE,
                           clean_specific_texts = NULL,
                           include_admin        = TRUE,
                           multi_response       = c('keep', 'dummy', 'split'),
                           multi_drop_original  = FALSE,
                           var_filter           = NULL,
                           idioma               = 1L,
                           overwrite            = TRUE) {

  if (!is.null(format)) format <- match.arg(format, c("spss", "excel"))
  multi_response <- match.arg(multi_response)
  if (!is.null(format)) dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  # -- 1. Conexi\u00F3n y carga bruta ----------------------------------------------
  message("[integra_export] Conectando a BD para estudio: ", study_id)
  raw <- .integra_fetch(study_id, db_config, valid_states, idioma)
  message("[integra_export] ", nrow(raw$data), " registros, ",
          nrow(raw$codebook), " entradas en codebook")

  # -- 2. Limpieza de etiquetas -----------------------------------------------
  raw$variables <- .clean_labels(raw$variables, clean_html, strip_parens,
                                 clean_instructions, clean_specific_texts)
  raw$codebook  <- .clean_labels(raw$codebook,  clean_html, strip_parens,
                                 clean_instructions, clean_specific_texts,
                                 cols = "label")

  # -- 3. Filtro de variables -------------------------------------------------
  if (!is.null(var_filter)) {
    raw$variables <- raw$variables[raw$variables$nomvar %in% var_filter, ]
    raw$codebook  <- raw$codebook[raw$codebook$nomvar  %in% var_filter, ]
  }

  # -- 4. Construir data frames de salida -------------------------------------

  # Detectar variables m\u00FAltiples en raw$data (antes de any as.numeric)
  # Una variable es m\u00FAltiple si alg\u00FAn valor contiene ";" en los datos originales
  study_coded <- setdiff(raw$variables$nomvar,
                         raw$variables$nomvar[stringr::str_ends(raw$variables$nomvar, "_O")])
  # Detectar siempre, independientemente del modo -- necesario para tratar
  # las m\u00FAltiples como texto (etiquetas separadas por ';') incluso en 'keep'
  multi_vars_detected <- character(0)
  for (.v in intersect(study_coded, names(raw$data))) {
    vals <- as.character(raw$data[[.v]])
    if (any(grepl(";", vals, fixed = TRUE), na.rm = TRUE))
      multi_vars_detected <- c(multi_vars_detected, .v)
  }

  df_codes  <- .build_coded_df(raw, include_admin,
                               multi_vars = multi_vars_detected)

  # -- 4b. Expandir variables de respuesta m\u00FAltiple -------------------------
  # (se hace antes de encode para que expand lea los c\u00F3digos num\u00E9ricos originales)
  if (multi_response != 'keep') {
    expanded <- .expand_multi_response(df_codes, raw$variables, raw$codebook,
                                       mode = multi_response,
                                       drop_original = multi_drop_original)
    df_codes       <- expanded$df
    raw$variables  <- expanded$variables
    raw$codebook   <- expanded$codebook
  }

  # -- 4c. Codificar variables m\u00FAltiples originales como texto ---------------
  # En 'keep': es el resultado final.
  # En 'dummy'/'split': convierte la variable original conservada (si drop_original=FALSE).
  # Las derivadas (factores) NO se tocan.
  vars_to_encode <- if (multi_response == 'keep') {
    multi_vars_detected
  } else if (!multi_drop_original) {
    intersect(multi_vars_detected, names(df_codes))  # solo las que siguen en el df
  } else {
    character(0)
  }
  if (length(vars_to_encode) > 0) {
    df_codes <- .encode_multi_as_text(df_codes, vars_to_encode,
                                      raw$variables, raw$codebook)
  }

  df_labels <- .build_labelled_df(raw, df_codes)      # etiquetas como character

  # -- 5. Exportar datos ------------------------------------------------------
  out_files <- list()

  if (!is.null(format)) {
    if (format == "spss") {
      path_spss <- file.path(output_dir, paste0(study_id, ".sav"))
      .export_spss(df_codes, raw$variables, raw$codebook, path_spss)
      out_files$spss <- path_spss
      message("[integra_export] SPSS exportado: ", path_spss)

    } else if (format == "excel") {
      path_xlsx <- file.path(output_dir, paste0(study_id, "_datos.xlsx"))
      .export_excel_data(df_codes, df_labels, raw$variables, path_xlsx)
      out_files$excel_data <- path_xlsx
      message("[integra_export] Excel de datos exportado: ", path_xlsx)
    }
  }

  # -- 6. Exportar diccionario de variables -----------------------------------
  # Construir variables_dict directamente desde names(df_codes) para garantizar
  # que el Excel refleja exactamente las columnas que van al SPSS (mismo orden,
  # mismas variables, incluyendo derivadas de m\u00FAltiples y admin).
  variables_dict <- tibble::tibble(
    nomvar   = names(df_codes),
    label    = sapply(names(df_codes), function(v) {
      lbl <- raw$variables$label[raw$variables$nomvar == v]
      if (length(lbl) == 1 && !is.na(lbl) && nzchar(lbl)) lbl else ""
    }, USE.NAMES = FALSE),
    tipo     = sapply(names(df_codes), function(v) {
      col <- df_codes[[v]]
      # Tipo por l\u00F3gica de negocio: la clase en df_codes no refleja el tipo
      # final del SPSS (la conversi\u00F3n a factor ocurre despu\u00E9s en .export_spss)
      if (inherits(col, c("Date", "POSIXct", "POSIXt"))) {
        "FECHA"
      } else if (v %in% multi_vars_detected) {
        "M\u00DALTIPLE"
      } else if (is.factor(col)) {
        "SIMPLE"
      } else if (is.character(col)) {
        "TEXTO"
      } else if (v %in% raw$codebook$nomvar) {
        "SIMPLE"
      } else {
        "N\u00DAMERO"
      }
    }, USE.NAMES = FALSE)
  )

  # Verificaci\u00F3n: n\u00FAmero de variables en SPSS vs diccionario
  message("[integra_export] Columnas SPSS: ", ncol(df_codes),
          " | Variables en diccionario: ", nrow(variables_dict))

  if (!is.null(format)) {
    path_vars <- file.path(output_dir, paste0("variables_", study_id, ".xlsx"))
    .export_variables_dict(variables_dict, raw$codebook, path_vars,
                           multi_vars = multi_vars_detected)
    out_files$variables_dict <- path_vars
    message("[integra_export] Diccionario exportado: ", path_vars)
  }

  # -- 7. Construir objetos de retorno -----------------------------------------
  # $data: data frame con el mismo contenido que el SPSS (df_codes con labels)
  # $variables: equivalente a la pesta\u00F1a "preguntas" del Excel
  # $niveles: equivalente a la pesta\u00F1a "niveles" del Excel
  out_files$data <- .build_spss_df(df_codes, raw$variables, raw$codebook)

  out_files$variables <- variables_dict |>
    dplyr::select(variable = nomvar, etiqueta = label, tipo) |>
    dplyr::mutate(tipo_nuevo = NA_character_)

  out_files$niveles <- raw$codebook |>
    dplyr::filter(nomvar %in% names(df_codes)) |>
    dplyr::select(variable = nomvar, codigo = value, etiqueta = label)

  invisible(out_files)
}


# =============================================================================
# Funci\u00F3n de carga para el Shiny (contrato unificado)
# =============================================================================

#' Carga un estudio de Integra y devuelve el data frame unificado listo para
#' consumir directamente por el Shiny, sin transformaciones adicionales.
#'
#' El data frame de salida sigue el contrato unificado:
#'   - Variables categoricas -> factor con etiquetas de texto como levels
#'   - Variables numericas   -> numeric
#'   - Variables de texto    -> character (incluyendo multiples con ";")
#'   - Columnas admin:       REGISTRO (character), FECHAFIN (Date),
#'                           POND (numeric, 1 por defecto),
#'                           ESTUDI, MES, DATA
#'
#' @param study_id           Identificador del estudio (p.ej. "1824_POL_ARAGON")
#' @param db_config          Lista con host, port, user, password, dbname
#' @param valid_states       Codigos ESTADO validos (por defecto c(1,5,6,9))
#' @param idioma             Codigo de idioma en variablescodigos (1 = principal)
#' @param clean_html         Si TRUE, elimina entidades HTML de las etiquetas
#' @param clean_instructions Si TRUE, elimina textos de instruccion de entrevistador
#' @param ns_nc_recode       Si TRUE, unifica variantes de NS/NC a "NS" y "NC"
#'
#' @return Lista con:
#'   $data     data frame con el contrato unificado
#'   $metadata lista con $variables (nomvar, label) y
#'             $codebook  (nomvar, value, label, label_short)
#' @export
load_integra_study <- function(study_id,
                               db_config,
                               valid_states       = c(1, 5, 6, 9),
                               idioma             = 1L,
                               clean_html         = TRUE,
                               clean_instructions = TRUE,
                               ns_nc_recode       = TRUE) {

  # -- 1. Obtener datos brutos y codebook desde la BD ---------------------------
  message("[load_integra_study] Conectando a BD para estudio: ", study_id)
  raw <- .integra_fetch(study_id, db_config, valid_states, idioma)
  message("[load_integra_study] ", nrow(raw$data), " registros, ",
          nrow(raw$codebook), " entradas en codebook")

  # -- 2. Limpiar etiquetas -----------------------------------------------------
  raw$variables <- .clean_labels(raw$variables, clean_html,
                                 strip_parens       = FALSE,
                                 clean_instructions = clean_instructions)
  raw$codebook  <- .clean_labels(raw$codebook,  clean_html,
                                 strip_parens       = FALSE,
                                 clean_instructions = clean_instructions,
                                 cols               = "label")

  # -- 3. Unificar NS/NC --------------------------------------------------------
  if (ns_nc_recode) {
    raw$codebook <- .recode_ns_nc(raw$codebook)
  }

  # -- 4. Construir el data frame unificado -------------------------------------
  df <- .build_unified_df(raw$data, raw$variables, raw$codebook, study_id)

  # -- 5. Construir codebook con label_short para el Shiny ----------------------
  # label_short = versi\u00F3n corta para ejes de gr\u00E1ficos; en Integra coincide con label.
  codebook_shiny <- raw$codebook |>
    dplyr::mutate(label_short = label)

  # A\u00F1adir entradas de MES (derivada, no viene de la BD)
  month_labels <- c("Gener","Febrer","Mar\u00E7","Abril","Maig","Juny",
                    "Juliol","Agost","Setembre","Octubre","Novembre","Desembre")
  codebook_shiny <- dplyr::bind_rows(
    codebook_shiny,
    tibble::tibble(
      nomvar      = "MES",
      value       = as.character(1:12),
      label       = month_labels,
      label_short = substr(month_labels, 1, 3)
    )
  )

  list(
    data     = df,
    metadata = list(
      variables = raw$variables,
      codebook  = codebook_shiny
    )
  )
}


# -----------------------------------------------------------------------------
# Helpers internos de load_integra_study
# -----------------------------------------------------------------------------

# Unifica variantes de NS/NC en el codebook
.recode_ns_nc <- function(codebook) {
  ns_variants <- c("No sabe", "No sap", "No sabe / No recuerda")
  nc_variants <- c("No contesta")
  codebook |>
    dplyr::mutate(
      label = dplyr::case_when(
        label %in% ns_variants ~ "NS",
        label %in% nc_variants ~ "NC",
        TRUE                   ~ label
      )
    )
}


# Construye el data frame con el contrato unificado a partir de datos brutos
.build_unified_df <- function(raw_data, variables, codebook, study_id) {

  study_vars <- variables$nomvar

  # -- Detectar tipos de cada variable -----------------------------------------
  # Texto: variables _O expl\u00EDcitas + cualquier variable sin codebook cuyos
  # valores no sean mayoritariamente num\u00E9ricos (misma l\u00F3gica que .build_coded_df)
  open_vars <- study_vars[stringr::str_ends(study_vars, "_O")]

  vars_with_codebook <- unique(codebook$nomvar)

  # Variables sin codebook y sin sufijo _O: detectar si son texto
  candidate_text <- setdiff(study_vars, c(open_vars, vars_with_codebook))
  extra_text_vars <- candidate_text[sapply(candidate_text, function(v) {
    if (!v %in% names(raw_data)) return(FALSE)
    vals <- raw_data[[v]][!is.na(raw_data[[v]])]
    if (length(vals) == 0) return(FALSE)
    pct_num <- mean(!is.na(suppressWarnings(as.numeric(as.character(vals)))))
    pct_num < 0.5
  })]

  text_vars <- union(open_vars, extra_text_vars)

  # M\u00FAltiples: tienen ";" en los valores
  coded_vars <- setdiff(study_vars, text_vars)
  multi_vars <- coded_vars[sapply(coded_vars, function(v) {
    if (!v %in% names(raw_data)) return(FALSE)
    any(grepl(";", as.character(raw_data[[v]]), fixed = TRUE), na.rm = TRUE)
  })]
  text_vars <- union(text_vars, multi_vars)  # m\u00FAltiples -> character

  # Resto: num\u00E9ricas con codebook -> factor; sin codebook -> numeric
  factor_vars  <- setdiff(coded_vars, multi_vars)
  numeric_vars <- setdiff(factor_vars, vars_with_codebook)
  factor_vars  <- intersect(factor_vars, vars_with_codebook)

  # -- Columnas admin a preservar -----------------------------------------------
  admin_cols <- c("REGISTRO", "FECHAFIN", "POND",
                  "SEXE", "EDAT_COD1", "CPROV", "HABI",
                  "AUT_QUALI", "WMUNI", "ESTADO")

  keep <- unique(c(study_vars, intersect(admin_cols, names(raw_data))))
  keep <- intersect(keep, names(raw_data))

  df <- raw_data |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::all_of(keep))

  cols_numeric <- intersect(numeric_vars, names(df))
  cols_text    <- intersect(text_vars,    names(df))
  cols_factor  <- intersect(factor_vars,  names(df))

  df <- df |>
    # Tipos base
    dplyr::mutate(
      dplyr::across(dplyr::all_of(cols_numeric), ~ suppressWarnings(as.numeric(.))),
      dplyr::across(dplyr::all_of(cols_text),    as.character),
      dplyr::across(dplyr::all_of(cols_factor),  ~ suppressWarnings(as.numeric(.)))
    ) |>
    # Limpiar "-" como NA en columnas character
    dplyr::mutate(dplyr::across(dplyr::where(is.character),
                                ~ dplyr::na_if(., "-")))

  # -- Convertir a factor las variables con codebook ----------------------------
  for (v in intersect(factor_vars, names(df))) {
    cb <- codebook[codebook$nomvar == v, ]
    if (nrow(cb) == 0) next
    df[[v]] <- factor(df[[v]], levels = cb$value, labels = cb$label)
  }

  # -- Atributo label en cada columna -------------------------------------------
  for (v in intersect(study_vars, names(df))) {
    lbl <- variables$label[variables$nomvar == v]
    if (length(lbl) == 1 && !is.na(lbl) && nzchar(lbl))
      attr(df[[v]], "label") <- lbl
  }

  # -- Columnas derivadas: REGISTRO, POND, ESTUDI, MES, DATA -------------------
  has_pond <- "POND" %in% names(df)
  df <- df |>
    dplyr::mutate(
      REGISTRO = as.character(REGISTRO),
      POND     = if (has_pond) as.numeric(POND) else 1,
      ESTUDI   = as.integer(substr(study_id, 1, 4)),
      MES      = as.integer(lubridate::month(FECHAFIN))
    ) |>
    dplyr::group_by(ESTUDI) |>
    dplyr::mutate(DATA = max(FECHAFIN, na.rm = TRUE)) |>
    dplyr::ungroup()

  df
}


# =============================================================================
# Funciones internas (prefijo punto)
# =============================================================================

# -----------------------------------------------------------------------------
# Conexi\u00F3n y lectura
# -----------------------------------------------------------------------------

.integra_fetch <- function(study_id, db_config, valid_states, idioma) {

  con <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    dbname   = db_config$dbname,
    host     = db_config$host,
    port     = db_config$port,
    user     = db_config$user,
    password = db_config$password
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # Datos brutos
  table_data <- paste0("datos_", study_id, "_0")
  if (!DBI::dbExistsTable(con, table_data))
    stop("[integra_export] Tabla no encontrada: ", table_data)

  raw_data <- DBI::dbReadTable(con, table_data) |>
    tibble::as_tibble() |>
    dplyr::filter(ESTADO %in% valid_states)

  # Codebook
  table_codes <- paste0("variablescodigos_", study_id)
  if (!DBI::dbExistsTable(con, table_codes))
    stop("[integra_export] Tabla no encontrada: ", table_codes)

  variable_codes <- DBI::dbReadTable(con, table_codes) |>
    tibble::as_tibble() |>
    dplyr::filter(idioma == !!idioma)

  # Diccionario de variables (una fila por variable)
  variables <- variable_codes |>
    dplyr::distinct(nomvar, txtext) |>
    dplyr::rename(label = txtext) |>
    dplyr::mutate(label = stringr::str_squish(label))

  # Codebook (una fila por c\u00F3digo por variable)
  # nomcod = etiqueta del nivel, defcod = c\u00F3digo num\u00E9rico
  codebook <- variable_codes |>
    dplyr::distinct(nomvar, nomcod, defcod) |>
    dplyr::filter(nzchar(trimws(nomcod)), nzchar(trimws(defcod))) |>
    dplyr::mutate(
      label = stringr::str_squish(as.character(nomcod)),
      value = trimws(as.character(defcod))   # c\u00F3digo num\u00E9rico como string limpio
    ) |>
    dplyr::select(nomvar, value, label)

  list(data = raw_data, variables = variables, codebook = codebook)
}


# -----------------------------------------------------------------------------
# Limpieza de etiquetas
# -----------------------------------------------------------------------------

.clean_labels <- function(df, clean_html, strip_parens,
                          clean_instructions   = FALSE,
                          clean_specific_texts = NULL,
                          cols = "label") {

  # Textos de instrucci\u00F3n internos -- ampliar aqu\u00ED en el futuro
  INSTRUCTION_TEXTS <- c(
    "(no leer)",
    "(anotar)",
    "(No llegir)"
  )

  for (col in intersect(cols, names(df))) {
    x <- df[[col]]

    if (clean_html) {
      x <- stringr::str_replace_all(x, c(
        "&nbsp;"   = " ",
        "&amp;"    = "&",
        "&lt;"     = "<",
        "&gt;"     = ">",
        "&quot;"   = '"',
        "&#039;"   = "'",
        "&apos;"   = "'"
      ))
      x <- stringr::str_remove_all(x, "<[^>]+>")
    }

    if (strip_parens) {
      x <- stringr::str_remove_all(x, "\\s*\\([^)]*\\)")
    }

    if (clean_instructions) {
      for (txt in INSTRUCTION_TEXTS) {
        x <- stringr::str_remove_all(x, stringr::fixed(txt, ignore_case = TRUE))
      }
    }

    if (!is.null(clean_specific_texts) && length(clean_specific_texts) > 0) {
      for (txt in clean_specific_texts) {
        x <- stringr::str_remove_all(x, stringr::fixed(txt, ignore_case = TRUE))
      }
    }

    x <- stringr::str_squish(x)
    df[[col]] <- x
  }
  df
}


# -----------------------------------------------------------------------------
# Construir data frames de salida
# -----------------------------------------------------------------------------

# Helper: insertar columnas derivadas en la posici\u00F3n correcta dentro de un df
.insert_cols <- function(df, after_var, derived_df, keep_original = TRUE) {
  orig_pos <- which(names(df) == after_var)
  col_names <- names(df)

  if (keep_original) {
    left  <- col_names[seq_len(orig_pos)]
    right <- if (orig_pos < length(col_names)) col_names[(orig_pos + 1):length(col_names)] else character(0)
  } else {
    left  <- if (orig_pos > 1) col_names[seq_len(orig_pos - 1)] else character(0)
    right <- if (orig_pos < length(col_names)) col_names[(orig_pos + 1):length(col_names)] else character(0)
  }

  # Si derived_df est\u00E1 vac\u00EDo, simplemente eliminar/mantener la original sin a\u00F1adir nada
  if (ncol(derived_df) == 0) {
    keep_cols <- if (keep_original) col_names else c(left, right)
    return(df[, keep_cols, drop = FALSE])
  }

  parts <- list()
  if (length(left)  > 0) parts <- c(parts, list(df[, left,  drop = FALSE]))
  parts <- c(parts, list(derived_df))
  if (length(right) > 0) parts <- c(parts, list(df[, right, drop = FALSE]))
  do.call(dplyr::bind_cols, parts)
}


.encode_multi_as_text <- function(df, multi_vars, variables, codebook) {
  # Para cada variable m\u00FAltiple, reemplaza los c\u00F3digos num\u00E9ricos separados por ";"
  # por sus etiquetas, produciendo una columna character con etiquetas separadas por ";".
  # Esto se aplica siempre: es el formato final en 'keep' y el de la variable
  # original cuando se conserva junto a las derivadas en 'dummy'/'split'.
  for (v in intersect(multi_vars, names(df))) {
    cb <- codebook[codebook$nomvar == v, ]
    if (nrow(cb) == 0) next

    cb_codes  <- trimws(as.character(cb$value))
    cb_labels <- trimws(as.character(cb$label))

    raw_vals <- as.character(df[[v]])

    decoded <- sapply(raw_vals, function(x) {
      if (is.na(x) || !nzchar(x)) return(NA_character_)
      parts  <- trimws(unlist(strsplit(x, ";", fixed = TRUE)))
      labels <- cb_labels[match(parts, cb_codes)]
      # C\u00F3digos sin etiqueta: dejar el c\u00F3digo original
      labels[is.na(labels)] <- parts[is.na(labels)]
      paste(labels, collapse = "; ")
    }, USE.NAMES = FALSE)

    # Recuperar var label si existe
    var_lbl <- variables$label[variables$nomvar == v]
    col <- decoded
    if (length(var_lbl) == 1 && !is.na(var_lbl) && nzchar(var_lbl))
      attr(col, "label") <- var_lbl

    df[[v]] <- col
  }
  df
}


# -----------------------------------------------------------------------------
# Helper: expandir variables de respuesta m\u00FAltiple
# -----------------------------------------------------------------------------

.expand_multi_response <- function(df, variables, codebook,
                                   mode = c("dummy", "split"),
                                   drop_original = FALSE) {
  mode <- match.arg(mode)

  # Detectar variables con ";" en alg\u00FAn valor (excluir columnas admin y _O)
  study_vars  <- variables$nomvar
  coded_vars  <- study_vars[!stringr::str_ends(study_vars, "_O")]
  multi_vars  <- character(0)

  for (v in intersect(coded_vars, names(df))) {
    vals <- as.character(df[[v]])
    if (any(grepl(";", vals, fixed = TRUE), na.rm = TRUE))
      multi_vars <- c(multi_vars, v)
  }

  if (length(multi_vars) == 0) {
    message("[integra_export] Respuesta m\u00FAltiple: ninguna variable detectada con ';'")
    return(list(df = df, variables = variables, codebook = codebook))
  }

  message("[integra_export] Variables de respuesta m\u00FAltiple detectadas (",
          length(multi_vars), "): ", paste(multi_vars, collapse = ", "))

  new_df        <- df
  new_variables <- variables
  new_codebook  <- codebook

  for (v in multi_vars) {

    var_label <- variables$label[variables$nomvar == v]
    var_label <- if (length(var_label) == 1 && !is.na(var_label)) var_label else v

    cb <- codebook[codebook$nomvar == v, ]
    # Construir tabla c\u00F3digo -> etiqueta para lookup
    cb_codes  <- suppressWarnings(as.numeric(trimws(cb$value)))
    cb_labels <- trimws(as.character(cb$label))
    valid_cb  <- !is.na(cb_codes)
    cb_codes  <- cb_codes[valid_cb]
    cb_labels <- cb_labels[valid_cb]

    # Si no hay niveles en el codebook, no se puede expandir -> saltar
    if (length(cb_codes) == 0) {
      warning(paste0("[integra_export] Variable '", v,
                     "' detectada como m\u00FAltiple pero sin niveles en el codebook -- se mantiene como est\u00E1."),
              call. = FALSE)
      next
    }

    # Splitear cada observaci\u00F3n por ";"
    raw_vals <- as.character(new_df[[v]])
    split_list <- lapply(raw_vals, function(x) {
      if (is.na(x) || !nzchar(x)) return(NA_character_)
      trimws(unlist(strsplit(x, ";", fixed = TRUE)))
    })

    if (mode == "dummy") {
      # Una variable dicot\u00F3mica por cada c\u00F3digo del codebook
      # Nombre: VAR_<codigo_numerico>
      new_cols      <- list()
      new_var_rows  <- list()
      new_cb_rows   <- list()

      for (i in seq_along(cb_codes)) {
        code      <- cb_codes[i]
        lbl       <- cb_labels[i]
        new_name  <- paste0(v, "_", code)
        col_label <- paste0(var_label, " - ", lbl)

        # S\u00ED/No seg\u00FAn si el c\u00F3digo est\u00E1 en las respuestas del individuo
        col_vals <- sapply(split_list, function(x) {
          if (all(is.na(x))) return(NA_character_)
          if (as.character(code) %in% x) "S\u00ED" else "No"
        })
        col_fct <- factor(col_vals, levels = c("S\u00ED", "No"))
        attr(col_fct, "label") <- col_label

        new_cols[[new_name]] <- col_fct

        new_var_rows[[i]] <- tibble::tibble(nomvar = new_name, label = col_label)
        new_cb_rows[[i]]  <- tibble::tibble(
          nomvar = new_name,
          value  = c("1", "2"),
          label  = c("S\u00ED", "No")
        )
      }

      # Insertar columnas derivadas junto a la original
      if (length(new_cols) == 0) {
        warning(paste0("[integra_export] Variable '", v,
                       "' no gener\u00F3 columnas derivadas (codebook sin c\u00F3digos v\u00E1lidos) -- se mantiene como est\u00E1."),
                call. = FALSE)
        next
      }
      derived_df <- tibble::as_tibble(new_cols)
      new_df <- .insert_cols(new_df, v, derived_df, keep_original = !drop_original)

      # Actualizar variables y codebook
      orig_var_idx   <- which(new_variables$nomvar == v)
      derived_vars   <- dplyr::bind_rows(new_var_rows)
      if (drop_original) {
        new_variables <- dplyr::bind_rows(
          new_variables[-orig_var_idx, ],
          derived_vars
        )
        new_codebook  <- dplyr::bind_rows(
          new_codebook[new_codebook$nomvar != v, ],
          dplyr::bind_rows(new_cb_rows)
        )
      } else {
        new_variables <- dplyr::bind_rows(
          new_variables[1:orig_var_idx, ],
          derived_vars,
          if (orig_var_idx < nrow(new_variables)) new_variables[(orig_var_idx+1):nrow(new_variables), ] else NULL
        )
        new_codebook <- dplyr::bind_rows(new_codebook, dplyr::bind_rows(new_cb_rows))
      }

    } else {
      # mode == "split": VAR_1, VAR_2, ...
      max_resp <- max(sapply(split_list, function(x) sum(!is.na(x))), na.rm = TRUE)
      new_cols     <- list()
      new_var_rows <- list()

      for (k in seq_len(max_resp)) {
        new_name  <- paste0(v, "_", k)
        col_label <- paste0(var_label, " - Respuesta ", k)

        raw_k <- sapply(split_list, function(x) {
          if (all(is.na(x)) || length(x) < k) return(NA_real_)
          suppressWarnings(as.numeric(x[k]))
        })

        col_fct <- factor(raw_k, levels = cb_codes, labels = cb_labels)
        attr(col_fct, "label") <- col_label

        new_cols[[new_name]] <- col_fct
        new_var_rows[[k]]    <- tibble::tibble(nomvar = new_name, label = col_label)
      }

      derived_df <- tibble::as_tibble(new_cols)
      new_df <- .insert_cols(new_df, v, derived_df, keep_original = !drop_original)

      orig_var_idx <- which(new_variables$nomvar == v)
      derived_vars <- dplyr::bind_rows(new_var_rows)

      # Codebook derivadas split = mismo codebook que original
      derived_cb <- do.call(dplyr::bind_rows, lapply(names(new_cols), function(nm) {
        dplyr::mutate(cb[valid_cb, ], nomvar = nm)
      }))

      if (drop_original) {
        new_variables <- dplyr::bind_rows(
          new_variables[-orig_var_idx, ],
          derived_vars
        )
        new_codebook <- dplyr::bind_rows(
          new_codebook[new_codebook$nomvar != v, ],
          derived_cb
        )
      } else {
        new_variables <- dplyr::bind_rows(
          new_variables[1:orig_var_idx, ],
          derived_vars,
          if (orig_var_idx < nrow(new_variables)) new_variables[(orig_var_idx+1):nrow(new_variables), ] else NULL
        )
        new_codebook <- dplyr::bind_rows(new_codebook, derived_cb)
      }
    }
  }

  list(df = new_df, variables = new_variables, codebook = new_codebook)
}


ADMIN_COLS <- c("REGISTRO", "FECHAFIN", "POND", "POND_1", "ESTUDI",
                "MES", "ANY", "DATA", "ESTADO",
                "SEXE", "EDAT", "EDAT_COD1", "CPROV", "HABI",
                "SEXE_EDAT", "SEXE_EDAT_CPROV", "CPROV_HABI",
                "AUT_QUALI", "WMUNI")

.build_coded_df <- function(raw, include_admin, multi_vars = character(0)) {
  study_vars <- raw$variables$nomvar
  open_vars  <- study_vars[stringr::str_ends(study_vars, "_O")]
  coded_vars <- setdiff(study_vars, open_vars)

  # Variables m\u00FAltiples: mantener como character para que expand_multi pueda leerlas
  numeric_vars <- setdiff(coded_vars, multi_vars)
  char_multi   <- intersect(multi_vars, coded_vars)

  # Variables fecha/datetime: preservar clase Date/POSIXct (vienen de BD)
  all_keep_preview <- c(coded_vars, multi_vars, open_vars,
                        if (include_admin) intersect(ADMIN_COLS, names(raw$data)) else character(0))
  all_keep_preview <- intersect(all_keep_preview, names(raw$data))
  date_vars <- all_keep_preview[sapply(all_keep_preview, function(v)
    inherits(raw$data[[v]], c("Date", "POSIXct", "POSIXt")))]
  if (length(date_vars) > 0)
    message("[integra_export] Variables de fecha detectadas (",
            length(date_vars), "): ", paste(date_vars, collapse = ", "))
  numeric_vars <- setdiff(numeric_vars, date_vars)

  # Variables de texto libre sin sufijo _O y sin codebook: tratarlas como character
  # (ej. B2_2_ENTITAT1: respuesta abierta no marcada con _O en Integra)
  vars_sin_codebook <- numeric_vars[
    !numeric_vars %in% raw$codebook$nomvar &
      numeric_vars %in% names(raw$data) &
      sapply(numeric_vars, function(v) {
        if (!v %in% names(raw$data)) return(FALSE)
        vals <- raw$data[[v]][!is.na(raw$data[[v]])]
        if (length(vals) == 0) return(FALSE)
        pct_num <- mean(!is.na(suppressWarnings(as.numeric(as.character(vals)))))
        pct_num < 0.5  # menos del 50% convertible a n\u00FAmero -> es texto
      })
  ]
  if (length(vars_sin_codebook) > 0)
    message("[integra_export] Variables de texto libre sin _O detectadas (",
            length(vars_sin_codebook), "): ",
            paste(vars_sin_codebook, collapse = ", "))

  numeric_vars <- setdiff(numeric_vars, vars_sin_codebook)
  char_text    <- vars_sin_codebook

  keep <- if (include_admin) {
    c(study_vars, intersect(ADMIN_COLS, names(raw$data)))
  } else {
    study_vars
  }
  keep <- intersect(keep, names(raw$data))

  df <- raw$data |>
    dplyr::select(dplyr::all_of(keep))

  cols_numeric <- intersect(numeric_vars, names(df))
  cols_char    <- intersect(c(open_vars, char_multi, char_text), names(df))

  df <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(cols_numeric), ~ suppressWarnings(as.numeric(.))),
      dplyr::across(dplyr::all_of(cols_char),    as.character)
      # date_vars se dejan sin tocar: mantienen su clase Date/POSIXct
    )
  df
}

.build_labelled_df <- function(raw, df_codes) {
  df <- df_codes
  coded_vars <- raw$variables$nomvar[
    !stringr::str_ends(raw$variables$nomvar, "_O")
  ]
  for (v in intersect(coded_vars, names(df))) {
    cb <- raw$codebook[raw$codebook$nomvar == v, ]
    if (nrow(cb) == 0) next
    num_vals <- suppressWarnings(as.numeric(df[[v]]))
    idx <- match(num_vals, suppressWarnings(as.numeric(cb$value)))
    df[[v]] <- cb$label[idx]
  }
  df
}


# -----------------------------------------------------------------------------
# Helper: truncar etiquetas de niveles a l\u00EDmite SPSS (120 caracteres)
# -----------------------------------------------------------------------------

SPSS_MAX_LABEL <- 120L

.truncate_factor_labels <- function(df, context = "") {
  truncated_vars <- character(0)
  for (v in names(df)) {
    col <- df[[v]]
    if (is.factor(col)) {
      lvls <- levels(col)
      long <- nchar(lvls) > SPSS_MAX_LABEL
      if (any(long)) {
        levels(col)[long] <- substr(lvls[long], 1L, SPSS_MAX_LABEL)
        df[[v]] <- col
        truncated_vars <- c(truncated_vars, v)
      }
    } else if (haven::is.labelled(col)) {
      lbls <- haven::val_labels(col)
      long <- nchar(names(lbls)) > SPSS_MAX_LABEL
      if (any(long)) {
        names(lbls)[long] <- substr(names(lbls)[long], 1L, SPSS_MAX_LABEL)
        haven::val_labels(col) <- lbls
        df[[v]] <- col
        truncated_vars <- c(truncated_vars, v)
      }
    }
  }
  if (length(truncated_vars) > 0) {
    warning(context, "Se han recortado etiquetas de nivel a ", SPSS_MAX_LABEL,
            " caracteres en: ", paste(truncated_vars, collapse = ", "),
            call. = FALSE)
  }
  df
}


# -----------------------------------------------------------------------------
# Exportar SPSS
# -----------------------------------------------------------------------------

.build_spss_df <- function(df_codes, variables, codebook, context = "[integra_export] ") {

  df_out <- df_codes
  n_factor <- 0

  for (v in names(df_out)) {

    # 1. Variable label (enunciado de la pregunta)
    lbl <- variables$label[variables$nomvar == v]
    var_label <- if (length(lbl) == 1 && !is.na(lbl) && nzchar(lbl)) as.character(lbl) else NULL

    # 2. Buscar niveles en el codebook
    cb <- codebook[codebook$nomvar == v, ]

    col <- df_out[[v]]

    # Si la columna ya es factor, character o fecha: solo aplicar var_label
    if (is.factor(col) || is.character(col) ||
        inherits(col, c("Date", "POSIXct", "POSIXt"))) {
      if (!is.null(var_label)) attr(col, "label") <- var_label
      df_out[[v]] <- col
      if (is.factor(col)) n_factor <- n_factor + 1

    } else if (nrow(cb) > 0) {
      # Variable num\u00E9rica con codebook: convertir a haven_labelled para que SPSS
      # conserve los c\u00F3digos originales de la BD (en lugar de posiciones 1..n
      # que genera factor()).
      codes  <- suppressWarnings(as.numeric(trimws(as.character(cb$value))))
      labels <- as.character(cb$label)
      valid  <- !is.na(codes)

      if (any(valid)) {
        codes   <- codes[valid]
        labels  <- labels[valid]
        raw_num <- suppressWarnings(as.numeric(col))
        val_labels <- stats::setNames(codes, labels)
        col <- haven::labelled(raw_num, labels = val_labels,
                               label = var_label)
        df_out[[v]] <- col
        n_factor <- n_factor + 1
      }

    } else {
      # Sin codebook: dejar como num\u00E9rico con solo var_label
      if (!is.null(var_label)) attr(col, "label") <- var_label
      df_out[[v]] <- col
    }
  }

  message(context, "Variables con value labels (haven_labelled): ", n_factor,
          " de ", ncol(df_out), " totales")

  .truncate_factor_labels(tibble::as_tibble(df_out), context = context)
}


.export_spss <- function(df_codes, variables, codebook, path) {

  df_out <- .build_spss_df(df_codes, variables, codebook)
  haven::write_sav(df_out, path)
  message("[integra_export] SPSS escrito: ", path)
}


# -----------------------------------------------------------------------------
# Exportar Excel de datos
# -----------------------------------------------------------------------------

.export_excel_data <- function(df_codes, df_labels, variables, path) {

  wb <- openxlsx::createWorkbook()

  # -- Pesta\u00F1a 1: C\u00F3digos -----------------------------------------------------
  openxlsx::addWorksheet(wb, "Codigos")
  openxlsx::writeData(wb, "Codigos", df_codes, headerStyle = .header_style())

  # Fila de etiquetas de pregunta justo encima de los datos (fila 1),
  # datos desde fila 2
  var_labels <- sapply(names(df_codes), function(v) {
    lbl <- variables$label[variables$nomvar == v]
    if (length(lbl) == 1 && !is.na(lbl)) lbl else v
  })
  # Insertar fila de etiquetas como comentarios de columna
  openxlsx::writeData(wb, "Codigos", as.data.frame(t(var_labels)),
                      startRow = 1, colNames = FALSE)
  openxlsx::writeData(wb, "Codigos", df_codes, startRow = 2,
                      headerStyle = .header_style())
  openxlsx::addStyle(wb, "Codigos",
                     style  = openxlsx::createStyle(textDecoration = "italic",
                                                    fontColour = "#666666"),
                     rows   = 1,
                     cols   = seq_along(names(df_codes)),
                     gridExpand = TRUE)

  # -- Pesta\u00F1a 2: Etiquetas ---------------------------------------------------
  openxlsx::addWorksheet(wb, "Etiquetas")
  openxlsx::writeData(wb, "Etiquetas", df_labels,
                      headerStyle = .header_style())

  .autofit_columns(wb, "Codigos",   df_codes)
  .autofit_columns(wb, "Etiquetas", df_labels)

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}


# -----------------------------------------------------------------------------
# Exportar diccionario de variables
# -----------------------------------------------------------------------------

.export_variables_dict <- function(variables, codebook, path,
                                   multi_vars = character(0)) {

  wb <- openxlsx::createWorkbook()

  # -- Pesta\u00F1a "preguntas" ----------------------------------------------------
  # Incluir columna 'tipo' si est\u00E1 presente en variables
  if ("tipo" %in% names(variables)) {
    preguntas <- variables |>
      dplyr::select(variable = nomvar, etiqueta = label, tipo) |>
      dplyr::mutate(tipo_nuevo = NA_character_)
  } else {
    preguntas <- variables |>
      dplyr::select(variable = nomvar, etiqueta = label) |>
      dplyr::mutate(tipo_nuevo = NA_character_)
  }

  openxlsx::addWorksheet(wb, "preguntas")
  openxlsx::writeData(wb, "preguntas", preguntas,
                      headerStyle = .header_style())
  .autofit_columns(wb, "preguntas", preguntas)

  # -- Pesta\u00F1a "niveles" ------------------------------------------------------
  niveles <- codebook |>
    dplyr::select(variable = nomvar, codigo = value, etiqueta = label)

  openxlsx::addWorksheet(wb, "niveles")
  openxlsx::writeData(wb, "niveles", niveles,
                      headerStyle = .header_style())
  .autofit_columns(wb, "niveles", niveles)

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}


# -----------------------------------------------------------------------------
# Helpers de formato Excel
# -----------------------------------------------------------------------------

.header_style <- function() {
  openxlsx::createStyle(
    textDecoration = "bold",
    fgFill         = "#DEEAF1",
    border         = "Bottom",
    borderColour   = "#2E75B6",
    wrapText       = FALSE
  )
}

.autofit_columns <- function(wb, sheet, df, min_width = 8, max_width = 40) {
  widths <- sapply(names(df), function(col) {
    vals <- nchar(as.character(df[[col]]))
    vals <- vals[!is.na(vals)]
    max_content <- if (length(vals) > 0) max(vals) else 0L
    header_w    <- nchar(col)
    min(max(max_content, header_w, min_width), max_width)
  })
  openxlsx::setColWidths(wb, sheet,
                         cols   = seq_along(names(df)),
                         widths = widths)
}


# =============================================================================
# Aplicar diccionario revisado a un SPSS existente
# =============================================================================

#' Lee un fichero SPSS y le aplica un diccionario de variables revisado
#'
#' El diccionario revisado es el Excel generado por export_integra() que el
#' usuario ha podido editar a mano. La funcion:
#'   1. Preserva solo las variables presentes en la pestana "preguntas"
#'   2. Aplica las etiquetas de variable (variable label) de esa pestana
#'   3. Corrige las etiquetas de niveles (value labels) segun la pestana "niveles"
#'   4. Guarda el resultado como un nuevo fichero SPSS
#'
#' @param spss_path       Ruta al fichero .sav original
#' @param variables_path  Ruta al Excel de variables revisado
#'                        (pestanas "preguntas" y "niveles")
#' @param output_path     Ruta del .sav de salida. Si NULL, sobreescribe el original.
#' @param sheet_preguntas Nombre de la pestana con variables y etiquetas (default "preguntas")
#' @param sheet_niveles   Nombre de la pestana con niveles (default "niveles")
#' @param na_codes        Vector de codigos numericos a tratar como NA del sistema
#'                        (por defecto c(97, 98, 99))
#'
#' @return Ruta al fichero .sav generado (invisible)
#' @export
apply_variables_dict <- function(spss_path,
                                 variables_path,
                                 output_path     = NULL,
                                 sheet_preguntas = "preguntas",
                                 sheet_niveles   = "niveles",
                                 na_codes        = c(97, 98, 99)) {

  if (!file.exists(spss_path))
    stop("[apply_variables_dict] Fichero SPSS no encontrado: ", spss_path)
  if (!file.exists(variables_path))
    stop("[apply_variables_dict] Fichero de variables no encontrado: ", variables_path)

  if (is.null(output_path))
    output_path <- spss_path

  # -- 1. Leer el SPSS original ------------------------------------------------
  message("[apply_variables_dict] Leyendo SPSS: ", spss_path)
  df <- haven::read_sav(spss_path)
  message("[apply_variables_dict] ", nrow(df), " filas, ", ncol(df), " columnas")

  # -- 2. Leer el diccionario revisado ----------------------------------------
  preguntas <- openxlsx::read.xlsx(variables_path, sheet = sheet_preguntas)
  niveles   <- openxlsx::read.xlsx(variables_path, sheet = sheet_niveles)

  names(preguntas) <- tolower(trimws(names(preguntas)))
  names(niveles)   <- tolower(trimws(names(niveles)))

  if (!all(c("variable", "etiqueta") %in% names(preguntas)))
    stop("[apply_variables_dict] La pesta\u00F1a '", sheet_preguntas,
         "' debe tener columnas 'variable' y 'etiqueta'")
  if (!all(c("variable", "codigo", "etiqueta") %in% names(niveles)))
    stop("[apply_variables_dict] La pesta\u00F1a '", sheet_niveles,
         "' debe tener columnas 'variable', 'codigo' y 'etiqueta'")

  preguntas$variable <- trimws(preguntas$variable)
  preguntas$etiqueta <- trimws(preguntas$etiqueta)
  niveles$variable   <- trimws(niveles$variable)
  niveles$etiqueta   <- trimws(niveles$etiqueta)
  niveles$codigo     <- trimws(as.character(niveles$codigo))

  has_tipo      <- "tipo"       %in% names(preguntas)
  has_tipo_nuevo <- "tipo_nuevo" %in% names(preguntas)
  if (has_tipo)       preguntas$tipo       <- trimws(toupper(preguntas$tipo))
  if (has_tipo_nuevo) preguntas$tipo_nuevo <- trimws(toupper(preguntas$tipo_nuevo))

  # -- 3. Preservar variables del diccionario en su orden ---------------------
  vars_dict    <- preguntas$variable
  vars_keep    <- intersect(vars_dict, names(df))
  vars_missing <- setdiff(vars_dict, names(df))

  if (length(vars_missing) > 0)
    warning("[apply_variables_dict] Variables en diccionario pero no en SPSS: ",
            paste(vars_missing, collapse = ", "))

  df <- df[, vars_keep, drop = FALSE]
  message("[apply_variables_dict] Variables conservadas: ", length(vars_keep),
          " (de ", length(vars_dict), " en diccionario)")

  # -- 4. Procesar cada variable ----------------------------------------------
  n_var_label  <- 0
  n_val_labels <- 0
  n_tipo_conv  <- 0

  for (v in vars_keep) {

    raw_col <- df[[v]]

    # Despojar clase haven_labelled -> vector at\u00F3mico limpio
    if (inherits(raw_col, c("Date", "POSIXct", "POSIXt"))) {
      col <- raw_col
    } else if (haven::is.labelled(raw_col)) {
      col <- as.numeric(raw_col)
    } else if (is.character(raw_col)) {
      col <- as.character(raw_col)
    } else {
      col <- suppressWarnings(as.numeric(raw_col))
    }

    # 4a. Etiqueta de variable
    lbl <- preguntas$etiqueta[preguntas$variable == v]
    var_label <- if (length(lbl) == 1 && !is.na(lbl) && nzchar(trimws(lbl)))
      trimws(lbl) else NULL

    # 4b. Niveles del Excel
    cb <- niveles[niveles$variable == v, ]
    cb_has_levels <- nrow(cb) > 0
    if (cb_has_levels) {
      num_codes <- suppressWarnings(as.numeric(cb$codigo))
      labels    <- trimws(cb$etiqueta)
      valid_cb  <- !is.na(num_codes) & !is.na(labels) & nzchar(labels)
      num_codes <- num_codes[valid_cb]
      labels    <- labels[valid_cb]
      cb_has_levels <- length(num_codes) > 0
    }

    # 4c. Tipo real detectado en el SPSS
    tipo_real <- if (inherits(raw_col, c("Date", "POSIXct", "POSIXt"))) {
      "FECHA"
    } else if (is.factor(raw_col) || haven::is.labelled(raw_col)) {
      "SIMPLE"
    } else if (is.character(raw_col)) {
      "TEXTO"
    } else {
      "N\u00DAMERO"
    }

    # 4d. Verificar coherencia entre tipo declarado en Excel y tipo real
    if (has_tipo) {
      tipo_declarado <- preguntas$tipo[preguntas$variable == v]
      if (length(tipo_declarado) == 1 && !is.na(tipo_declarado) &&
          nzchar(tipo_declarado) && tipo_declarado != tipo_real) {
        message("[apply_variables_dict] '", v, "': tipo declarado en Excel (",
                tipo_declarado, ") no coincide con tipo real en SPSS (",
                tipo_real, "). Se usa el tipo real.")
      }
    }

    # 4e. Tipo objetivo: tipo_nuevo si est\u00E1 relleno, si no -> sin conversi\u00F3n
    tipo_nuevo <- NULL
    if (has_tipo_nuevo) {
      tn <- preguntas$tipo_nuevo[preguntas$variable == v]
      if (length(tn) == 1 && !is.na(tn) && nzchar(tn)) tipo_nuevo <- tn
    }

    # 4f. Aplicar conversi\u00F3n o value labels
    if (!is.null(tipo_nuevo) && tipo_nuevo != tipo_real) {

      # -- Conversiones permitidas ------------------------------------------
      converted <- FALSE

      if (tipo_real == "SIMPLE" && tipo_nuevo == "N\u00DAMERO") {
        # Validar que las etiquetas del codebook sean num\u00E9ricas
        if (cb_has_levels) {
          pct_etiq_num <- mean(!is.na(suppressWarnings(as.numeric(labels))))
          if (pct_etiq_num < 0.95) {
            warning(paste0("[apply_variables_dict] '", v,
                           "': SIMPLE->N\u00DAMERO rechazado: etiquetas no num\u00E9ricas (",
                           round(pct_etiq_num * 100), "% num\u00E9ricas). Se mantiene como SIMPLE."),
                    call. = FALSE)
            tipo_nuevo <- NULL  # cancelar
          }
        }
        if (!is.null(tipo_nuevo)) {
          col_num <- suppressWarnings(as.numeric(col))
          if (length(na_codes) > 0) {
            mask_na   <- col_num %in% na_codes
            n_na_repl <- sum(mask_na, na.rm = TRUE)
            if (n_na_repl > 0) {
              codigos_na <- na_codes[na_codes %in% unique(col_num[!is.na(col_num)])]
              col_num[mask_na] <- NA_real_
              message("[apply_variables_dict] '", v, "': ", n_na_repl,
                      " valor(es) -> NA (c\u00F3digos: ", paste(codigos_na, collapse = ", "), ")")
            }
          }
          col <- col_num
          converted <- TRUE
          message("[apply_variables_dict] '", v, "': SIMPLE -> N\u00DAMERO")
        }

      } else if (tipo_real == "SIMPLE" && tipo_nuevo == "TEXTO") {
        if (cb_has_levels) {
          idx_match <- match(suppressWarnings(as.numeric(col)), num_codes)
          col <- labels[idx_match]
        } else {
          col <- as.character(col)
        }
        converted <- TRUE
        message("[apply_variables_dict] '", v, "': SIMPLE -> TEXTO")

      } else if (tipo_real == "N\u00DAMERO" && tipo_nuevo == "TEXTO") {
        col <- as.character(col)
        converted <- TRUE
        message("[apply_variables_dict] '", v, "': N\u00DAMERO -> TEXTO")

      } else if (tipo_real == "TEXTO" && tipo_nuevo == "N\u00DAMERO") {
        vals_noNA <- col[!is.na(col) & nzchar(col)]
        pct_num   <- if (length(vals_noNA) > 0)
          mean(!is.na(suppressWarnings(as.numeric(vals_noNA)))) else 0
        if (pct_num >= 1.0) {
          col <- suppressWarnings(as.numeric(col))
          converted <- TRUE
          message("[apply_variables_dict] '", v, "': TEXTO -> N\u00DAMERO")
        } else {
          warning(paste0("[apply_variables_dict] '", v,
                         "': TEXTO->N\u00DAMERO rechazado: solo ",
                         round(pct_num * 100), "% de valores son num\u00E9ricos.",
                         " Se mantiene como TEXTO."),
                  call. = FALSE)
        }

      } else if (tipo_nuevo %in% c("M\u00DALTIPLE", "TEXTO") &&
                 tipo_real  %in% c("M\u00DALTIPLE", "TEXTO")) {
        col <- as.character(col)
        converted <- TRUE
        message("[apply_variables_dict] '", v, "': ", tipo_real, " -> ", tipo_nuevo,
                " (normalizaci\u00F3n a character)")

      } else {
        warning(paste0("[apply_variables_dict] '", v, "': conversi\u00F3n ",
                       tipo_real, "->", tipo_nuevo,
                       " no soportada. Se mantiene el tipo original."),
                call. = FALSE)
      }

      if (converted) n_tipo_conv <- n_tipo_conv + 1

    } else {
      # Sin conversi\u00F3n: aplicar value labels si es SIMPLE
      if (tipo_real == "SIMPLE" && cb_has_levels) {
        col <- factor(suppressWarnings(as.numeric(col)),
                      levels = num_codes,
                      labels = labels)
        n_val_labels <- n_val_labels + 1
      }
    }

    # Aplicar etiqueta de variable
    if (!is.null(var_label)) {
      attr(col, "label") <- var_label
      n_var_label <- n_var_label + 1
    }

    df[[v]] <- col
  }

  message("[apply_variables_dict] Variable labels aplicadas: ", n_var_label)
  message("[apply_variables_dict] Value labels aplicadas:    ", n_val_labels)
  message("[apply_variables_dict] Conversiones de tipo:      ", n_tipo_conv)

  # -- 5. Escribir SPSS --------------------------------------------------------
  out_list <- lapply(vars_keep, function(v) df[[v]])
  names(out_list) <- vars_keep
  df_write <- tibble::new_tibble(out_list, nrow = nrow(df))
  df_write <- .truncate_factor_labels(df_write,
                                      context = "[apply_variables_dict] ")
  haven::write_sav(df_write, output_path)
  message("[apply_variables_dict] SPSS guardado: ", output_path)

  invisible(output_path)
}
