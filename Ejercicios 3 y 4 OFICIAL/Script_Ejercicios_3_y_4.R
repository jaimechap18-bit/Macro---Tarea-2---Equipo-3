# =============================================================================
# Tarea 2 - Macroeconomía
# Ejercicio 3: Consumo agregado en México y EE.UU.
# Script de cálculos pesados (gemelo)
# =============================================================================

# --- 0. Configuración portable ------------------------------------------------
setwd("C:/Users/spart/Desktop/Tarea_2_Macro")

base_dir    <- getwd()
datos_mex   <- file.path(base_dir, "Ejercicio 3", "Datos")
datos_usa   <- file.path(base_dir, "Ejercicio 3", "Datos")
res_dir     <- file.path(base_dir, "Ejercicio 3", "Resultados Precalculados")

dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)

# --- 0.1 Librerías -----------------------------------------------------------
paquetes <- c("readxl", "zoo", "mFilter", "ggplot2", "dplyr", "tidyr",
              "gridExtra", "scales", "lmtest", "sandwich")

for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# --- Función auxiliar para leer archivos BIE del INEGI (.xls / .xlsx) ---------
leer_bie <- function(archivo) {
  df <- read_excel(archivo, col_names = FALSE, sheet = 1)
  
  inicio <- which(grepl("Periodos", df[[1]], ignore.case = TRUE))
  if (length(inicio) == 0) inicio <- 4
  
  datos <- df[(inicio + 1):nrow(df), 1:2]
  colnames(datos) <- c("periodo", "valor")
  
  datos <- datos[grepl("^\\d{4}/\\d{2}$", datos$periodo), ]
  datos$valor <- as.numeric(datos$valor)
  
  datos$anio <- as.integer(substr(datos$periodo, 1, 4))
  datos$trim <- as.integer(substr(datos$periodo, 6, 7))
  datos$fecha <- as.Date(paste0(datos$anio, "-", sprintf("%02d", (datos$trim - 1) * 3 + 1), "-01"))
  
  datos <- datos[!is.na(datos$valor), ]
  return(datos[, c("fecha", "anio", "trim", "valor")])
}

# =============================================================================
# 1. CARGAR DATOS DE MÉXICO
# =============================================================================

cat("--- Cargando datos de México ---\n")

archivos_mex <- list.files(datos_mex, full.names = TRUE)

buscar_archivo <- function(patron) {
  idx <- grep(patron, archivos_mex, ignore.case = TRUE)
  if (length(idx) == 0) stop(paste("No se encontró archivo con patrón:", patron))
  archivos_mex[idx[1]]
}

Y_mex  <- leer_bie(buscar_archivo("Producto.Interno.Bruto.*Millones"))
C_mex  <- leer_bie(buscar_archivo("Consumo.privado"))
G_mex  <- leer_bie(buscar_archivo("Consumo.de.gobierno"))
I_mex  <- leer_bie(buscar_archivo("Formaci.n.bruta"))
X_mex  <- leer_bie(buscar_archivo("Exportaciones"))
M_mex  <- leer_bie(buscar_archivo("Importaciones"))

cat("  Y:", range(Y_mex$fecha), "obs:", nrow(Y_mex), "\n")
cat("  C:", range(C_mex$fecha), "obs:", nrow(C_mex), "\n")
cat("  G:", range(G_mex$fecha), "obs:", nrow(G_mex), "\n")
cat("  I:", range(I_mex$fecha), "obs:", nrow(I_mex), "\n")
cat("  X:", range(X_mex$fecha), "obs:", nrow(X_mex), "\n")
cat("  M:", range(M_mex$fecha), "obs:", nrow(M_mex), "\n")

# --- 1.1 Consumo por tipo: Bienes (Cb) y Servicios (Cs) ----------------------
cat("--- Cargando IMCPMI (índice mensual) ---\n")

imcpmi_file <- buscar_archivo("cp.indice")
imcpmi_raw  <- read_excel(imcpmi_file, col_names = FALSE, sheet = 1)

bienes_vals    <- as.numeric(unlist(imcpmi_raw[9, -(1:2)]))
servicios_vals <- as.numeric(unlist(imcpmi_raw[10, -(1:2)]))

n_meses <- length(bienes_vals)
fecha_inicio <- as.Date("1993-01-01")
fechas_imcpmi <- seq(fecha_inicio, by = "month", length.out = n_meses)

imcpmi <- data.frame(
  fecha     = fechas_imcpmi,
  bienes    = bienes_vals,
  servicios = servicios_vals
)
imcpmi <- imcpmi[!is.na(imcpmi$bienes) & !is.na(imcpmi$servicios), ]
cat("  IMCPMI:", range(imcpmi$fecha), "obs:", nrow(imcpmi), "\n")

imcpmi$anio <- as.integer(format(imcpmi$fecha, "%Y"))
imcpmi$mes  <- as.integer(format(imcpmi$fecha, "%m"))
imcpmi$trim <- ceiling(imcpmi$mes / 3)

imcpmi_trim <- imcpmi %>%
  group_by(anio, trim) %>%
  summarise(bienes = mean(bienes, na.rm = TRUE),
            servicios = mean(servicios, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(fecha = as.Date(paste0(anio, "-", sprintf("%02d", (trim - 1) * 3 + 1), "-01")))

imcpmi_trim$total <- imcpmi_trim$bienes + imcpmi_trim$servicios
imcpmi_trim$prop_bienes    <- imcpmi_trim$bienes / imcpmi_trim$total
imcpmi_trim$prop_servicios <- imcpmi_trim$servicios / imcpmi_trim$total

C_split <- merge(C_mex, imcpmi_trim[, c("fecha", "prop_bienes", "prop_servicios")],
                 by = "fecha", all.x = FALSE)
C_split$Cb <- C_split$valor * C_split$prop_bienes
C_split$Cs <- C_split$valor * C_split$prop_servicios

cat("  Cb/Cs disponibles:", range(C_split$fecha), "obs:", nrow(C_split), "\n")

# --- 1.2 Exportaciones netas -------------------------------------------------
NX_mex <- merge(X_mex[, c("fecha", "valor")], M_mex[, c("fecha", "valor")],
                by = "fecha", suffixes = c("_X", "_M"))
NX_mex$valor <- NX_mex$valor_X - NX_mex$valor_M
NX_mex <- NX_mex[, c("fecha", "valor")]

# =============================================================================
# 2. CARGAR DATOS DE EE.UU. (BEA NIPA)
# =============================================================================

cat("\n--- Cargando datos de EE.UU. ---\n")

leer_bea <- function(archivo, fila_variable) {
  df <- read_excel(archivo, col_names = FALSE, sheet = 1)
  
  anios_row  <- as.character(unlist(df[6, ]))
  trims_row  <- as.character(unlist(df[7, ]))
  datos_row  <- as.numeric(unlist(df[fila_variable, ]))
  
  trim_map <- c(Q1=1, Q2=2, Q3=3, Q4=4)
  
  anio_actual <- NA
  fechas <- c()
  valores <- c()
  
  for (j in 3:length(anios_row)) {
    if (!is.na(anios_row[j]) && grepl("^\\d{4}$", anios_row[j])) {
      anio_actual <- as.integer(anios_row[j])
    }
    trim_str <- trims_row[j]
    if (!is.na(trim_str) && trim_str %in% names(trim_map) && !is.na(anio_actual)) {
      trim_num <- trim_map[trim_str]
      fecha <- as.Date(paste0(anio_actual, "-", sprintf("%02d", (trim_num - 1) * 3 + 1), "-01"))
      fechas <- c(fechas, fecha)
      valores <- c(valores, datos_row[j])
    }
  }
  fechas <- as.Date(fechas, origin = "1970-01-01")
  
  resultado <- data.frame(fecha = fechas, valor = valores)
  resultado <- resultado[!is.na(resultado$valor), ]
  resultado$valor <- as.numeric(resultado$valor)
  resultado <- resultado[!is.na(resultado$valor), ]
  
  return(resultado)
}

bea_file <- buscar_archivo("Table.1.1.6")

Y_usa  <- leer_bea(bea_file, 8)
C_usa  <- leer_bea(bea_file, 9)
Cb_usa <- leer_bea(bea_file, 10)
Cs_usa <- leer_bea(bea_file, 13)
I_usa  <- leer_bea(bea_file, 14)
G_usa  <- leer_bea(bea_file, 29)
X_usa  <- leer_bea(bea_file, 23)
M_usa  <- leer_bea(bea_file, 26)

NX_usa <- merge(X_usa, M_usa, by = "fecha", suffixes = c("_X", "_M"))
NX_usa$valor <- NX_usa$valor_X - NX_usa$valor_M
NX_usa <- NX_usa[, c("fecha", "valor")]

cat("  Y_usa:", range(Y_usa$fecha), "obs:", nrow(Y_usa), "\n")
cat("  Cb_usa:", range(Cb_usa$fecha), "obs:", nrow(Cb_usa), "\n")
cat("  Cs_usa:", range(Cs_usa$fecha), "obs:", nrow(Cs_usa), "\n")

# =============================================================================
# 3. CONSTRUIR PANEL CONSOLIDADO
# =============================================================================

construir_panel <- function(lista_series, nombres) {
  panel <- lista_series[[1]][, c("fecha", "valor")]
  colnames(panel)[2] <- nombres[1]
  for (i in 2:length(lista_series)) {
    tmp <- lista_series[[i]][, c("fecha", "valor")]
    colnames(tmp)[2] <- nombres[i]
    panel <- merge(panel, tmp, by = "fecha", all = FALSE)
  }
  panel <- panel[order(panel$fecha), ]
  return(panel)
}

panel_mex <- construir_panel(
  lista_series = list(Y_mex, C_mex, 
                      data.frame(fecha = C_split$fecha, valor = C_split$Cs),
                      data.frame(fecha = C_split$fecha, valor = C_split$Cb),
                      I_mex, G_mex, NX_mex),
  nombres = c("Y", "C", "Cs", "Cb", "I", "G", "NX")
)

panel_usa <- construir_panel(
  lista_series = list(Y_usa, C_usa, Cs_usa, Cb_usa, I_usa, G_usa, NX_usa),
  nombres = c("Y", "C", "Cs", "Cb", "I", "G", "NX")
)

cat("\n--- Paneles construidos ---\n")
cat("  México:", range(panel_mex$fecha), "obs:", nrow(panel_mex), "\n")
cat("  EE.UU.:", range(panel_usa$fecha), "obs:", nrow(panel_usa), "\n")

# =============================================================================
# 4. INCISO (b): GRÁFICAS DE SERIES EN NIVELES Y LOG
# =============================================================================

cat("\n--- Generando gráficas (b) ---\n")

# --- Tema uniforme ---
tema_base <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, color = "grey40"),
    plot.caption     = element_text(size = 7, color = "grey50", hjust = 0),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey92", linewidth = 0.15)
  )

# --- Paleta de colores ---
colores <- c("Y" = "#1b9e77", "C" = "#d95f02", "Cs" = "#7570b3",
             "Cb" = "#e7298a", "I" = "#66a61e", "G" = "#e6ab02", "NX" = "#a6761d")

# --- Fuentes ---
fuente_mex <- "Fuente: Elaboración propia con datos del INEGI (SCN, base 2018) e IMCPMI."
fuente_usa <- "Fuente: Elaboración propia con datos del BEA (NIPA Tables, chained 2017 dollars)."

# --- Función genérica para graficar series ---
graficar_series <- function(panel, pais, variables, titulo, 
                            usar_log = FALSE, fuente = NULL) {
  df_long <- panel %>%
    select(fecha, all_of(variables)) %>%
    pivot_longer(-fecha, names_to = "variable", values_to = "valor")
  
  if (usar_log) {
    df_long <- df_long %>% filter(valor > 0)
    df_long$valor <- log(df_long$valor)
  }
  
  df_puntos <- df_long %>% filter(format(fecha, "%m") == "01")
  
  p <- ggplot(df_long, aes(x = fecha, y = valor, color = variable)) +
    geom_line(linewidth = 0.6) +
    geom_point(data = df_puntos, size = 1.3, shape = 16) +
    scale_color_manual(values = colores) +
    labs(title    = titulo,
         subtitle = pais,
         x = "", y = ifelse(usar_log, "Log", "Millones (moneda local)"),
         caption  = fuente) +
    tema_base +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y")
  
  return(p)
}

vars_positivas <- c("Y", "C", "Cs", "Cb", "I", "G")

# --- México ---
p1_mex <- graficar_series(panel_mex, "México", vars_positivas,
                          "Series en niveles (precios de 2018)", fuente = fuente_mex)
p2_mex <- graficar_series(panel_mex, "México", vars_positivas,
                          "Series en logaritmo natural", usar_log = TRUE, fuente = fuente_mex)
p3_mex <- graficar_series(panel_mex, "México", "NX",
                          "Exportaciones netas (NX)", fuente = fuente_mex)

# --- EE.UU. ---
p1_usa <- graficar_series(panel_usa, "Estados Unidos", vars_positivas,
                          "Series en niveles (dólares encadenados 2017)", fuente = fuente_usa)
p2_usa <- graficar_series(panel_usa, "Estados Unidos", vars_positivas,
                          "Series en logaritmo natural", usar_log = TRUE, fuente = fuente_usa)
p3_usa <- graficar_series(panel_usa, "Estados Unidos", "NX",
                          "Exportaciones netas (NX)", fuente = fuente_usa)

ggsave(file.path(res_dir, "b_niveles_mex.png"), p1_mex, width = 10, height = 5, dpi = 150)
ggsave(file.path(res_dir, "b_log_mex.png"), p2_mex, width = 10, height = 5, dpi = 150)
ggsave(file.path(res_dir, "b_NX_mex.png"), p3_mex, width = 10, height = 5, dpi = 150)
ggsave(file.path(res_dir, "b_niveles_usa.png"), p1_usa, width = 10, height = 5, dpi = 150)
ggsave(file.path(res_dir, "b_log_usa.png"), p2_usa, width = 10, height = 5, dpi = 150)
ggsave(file.path(res_dir, "b_NX_usa.png"), p3_usa, width = 10, height = 5, dpi = 150)

# =============================================================================
# 5. INCISO (c): VERSIONES FILTRADAS
# =============================================================================

cat("\n--- Calculando filtros (c) ---\n")

# --- 5.1 Tasas de crecimiento interanual ------------------------------------

calc_tasa_crec <- function(x) {
  n <- length(x)
  tasa <- rep(NA, n)
  for (i in 5:n) {
    if (!is.na(x[i]) && !is.na(x[i-4]) && x[i-4] != 0) {
      tasa[i] <- (x[i] - x[i-4]) / x[i-4]
    }
  }
  return(tasa)
}

vars_filtro <- c("Y", "C", "Cs", "Cb", "I", "G")

for (v in vars_filtro) {
  panel_mex[[paste0("tc_", v)]] <- calc_tasa_crec(panel_mex[[v]])
  panel_usa[[paste0("tc_", v)]] <- calc_tasa_crec(panel_usa[[v]])
}

# --- Función para graficar tasas de crecimiento ---
graficar_tasas <- function(panel, pais, variables_tc, titulo, fuente = NULL) {
  df_long <- panel %>%
    select(fecha, all_of(variables_tc)) %>%
    pivot_longer(-fecha, names_to = "variable", values_to = "valor") %>%
    filter(!is.na(valor))
  
  df_long$variable <- gsub("tc_", "", df_long$variable)
  df_puntos <- df_long %>% filter(format(fecha, "%m") == "01")
  
  p <- ggplot(df_long, aes(x = fecha, y = valor, color = variable)) +
    geom_line(linewidth = 0.5) +
    geom_point(data = df_puntos, size = 1.3, shape = 16) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = colores) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = titulo, subtitle = pais, x = "",
         y = "Tasa de crecimiento interanual", caption = fuente) +
    tema_base +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y")
  return(p)
}

vars_tc <- paste0("tc_", vars_filtro)

p_tc_mex <- graficar_tasas(panel_mex, "México", vars_tc,
                           "Tasas de crecimiento interanual", fuente = fuente_mex)
p_tc_usa <- graficar_tasas(panel_usa, "Estados Unidos", vars_tc,
                           "Tasas de crecimiento interanual", fuente = fuente_usa)

ggsave(file.path(res_dir, "c_tasas_crec_mex.png"), p_tc_mex, width = 10, height = 5, dpi = 150)
ggsave(file.path(res_dir, "c_tasas_crec_usa.png"), p_tc_usa, width = 10, height = 5, dpi = 150)

# Matrices de varianzas-covarianzas
tc_mex_mat <- panel_mex[complete.cases(panel_mex[, vars_tc]), vars_tc]
tc_usa_mat <- panel_usa[complete.cases(panel_usa[, vars_tc]), vars_tc]
colnames(tc_mex_mat) <- vars_filtro
colnames(tc_usa_mat) <- vars_filtro

vcov_tc_mex <- cov(tc_mex_mat)
vcov_tc_usa <- cov(tc_usa_mat)

# --- 5.2 Filtro HP (lambda = 1600) -------------------------------------------

aplicar_hp <- function(panel, variables, lambda = 1600) {
  for (v in variables) {
    serie <- panel[[v]]
    validos <- !is.na(serie) & serie > 0
    log_serie <- rep(NA, length(serie))
    log_serie[validos] <- log(serie[validos])
    
    idx <- which(validos)
    if (length(idx) > 10) {
      hp_result <- hpfilter(log_serie[idx], freq = lambda, type = "lambda")
      ciclo <- rep(NA, length(serie))
      ciclo[idx] <- hp_result$cycle
      panel[[paste0("hp_", v)]] <- ciclo
    }
  }
  return(panel)
}

panel_mex <- aplicar_hp(panel_mex, vars_filtro)
panel_usa <- aplicar_hp(panel_usa, vars_filtro)

# --- Función para graficar componentes cíclicos ---
graficar_ciclos <- function(panel, pais, prefijo, titulo, fuente = NULL) {
  vars_ciclo <- paste0(prefijo, "_", vars_filtro)
  vars_ciclo <- vars_ciclo[vars_ciclo %in% colnames(panel)]
  
  df_long <- panel %>%
    select(fecha, all_of(vars_ciclo)) %>%
    pivot_longer(-fecha, names_to = "variable", values_to = "valor") %>%
    filter(!is.na(valor))
  
  df_long$variable <- gsub(paste0(prefijo, "_"), "", df_long$variable)
  df_puntos <- df_long %>% filter(format(fecha, "%m") == "01")
  
  p <- ggplot(df_long, aes(x = fecha, y = valor, color = variable)) +
    geom_line(linewidth = 0.5) +
    geom_point(data = df_puntos, size = 1.3, shape = 16) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_color_manual(values = colores) +
    labs(title = titulo, subtitle = pais, x = "",
         y = "Componente cíclico (log)", caption = fuente) +
    tema_base +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y")
  return(p)
}

p_hp_mex <- graficar_ciclos(panel_mex, "México", "hp",
                            "Filtro Hodrick-Prescott (λ=1600)", fuente = fuente_mex)
p_hp_usa <- graficar_ciclos(panel_usa, "Estados Unidos", "hp",
                            "Filtro Hodrick-Prescott (λ=1600)", fuente = fuente_usa)

ggsave(file.path(res_dir, "c_hp_mex.png"), p_hp_mex, width = 10, height = 5, dpi = 150)
ggsave(file.path(res_dir, "c_hp_usa.png"), p_hp_usa, width = 10, height = 5, dpi = 150)

hp_vars <- paste0("hp_", vars_filtro)
hp_mex_mat <- panel_mex[complete.cases(panel_mex[, hp_vars[hp_vars %in% colnames(panel_mex)]]),
                        hp_vars[hp_vars %in% colnames(panel_mex)]]
hp_usa_mat <- panel_usa[complete.cases(panel_usa[, hp_vars[hp_vars %in% colnames(panel_usa)]]),
                        hp_vars[hp_vars %in% colnames(panel_usa)]]
colnames(hp_mex_mat) <- gsub("hp_", "", colnames(hp_mex_mat))
colnames(hp_usa_mat) <- gsub("hp_", "", colnames(hp_usa_mat))

vcov_hp_mex <- cov(hp_mex_mat)
vcov_hp_usa <- cov(hp_usa_mat)

# --- 5.3 Filtro Band-Pass (Baxter-King) --------------------------------------

aplicar_bp <- function(panel, variables, pl = 6, pu = 32, nfix = 12) {
  for (v in variables) {
    serie <- panel[[v]]
    validos <- !is.na(serie) & serie > 0
    log_serie <- rep(NA, length(serie))
    log_serie[validos] <- log(serie[validos])
    
    idx <- which(validos)
    if (length(idx) > (2 * nfix + 5)) {
      bp_result <- bkfilter(log_serie[idx], pl = pl, pu = pu, nfix = nfix)
      ciclo <- rep(NA, length(serie))
      bp_cycle <- bp_result$cycle[, 1]
      ciclo[idx[(nfix + 1):(length(idx) - nfix)]] <- bp_cycle
      panel[[paste0("bp_", v)]] <- ciclo
    }
  }
  return(panel)
}

panel_mex <- aplicar_bp(panel_mex, vars_filtro)
panel_usa <- aplicar_bp(panel_usa, vars_filtro)

p_bp_mex <- graficar_ciclos(panel_mex, "México", "bp",
                            "Filtro Band-Pass (Baxter-King, 6-32 trimestres)", fuente = fuente_mex)
p_bp_usa <- graficar_ciclos(panel_usa, "Estados Unidos", "bp",
                            "Filtro Band-Pass (Baxter-King, 6-32 trimestres)", fuente = fuente_usa)

ggsave(file.path(res_dir, "c_bp_mex.png"), p_bp_mex, width = 10, height = 5, dpi = 150)
ggsave(file.path(res_dir, "c_bp_usa.png"), p_bp_usa, width = 10, height = 5, dpi = 150)

bp_vars <- paste0("bp_", vars_filtro)
bp_mex_mat <- panel_mex[complete.cases(panel_mex[, bp_vars[bp_vars %in% colnames(panel_mex)]]),
                        bp_vars[bp_vars %in% colnames(panel_mex)]]
bp_usa_mat <- panel_usa[complete.cases(panel_usa[, bp_vars[bp_vars %in% colnames(panel_usa)]]),
                        bp_vars[bp_vars %in% colnames(panel_usa)]]
colnames(bp_mex_mat) <- gsub("bp_", "", colnames(bp_mex_mat))
colnames(bp_usa_mat) <- gsub("bp_", "", colnames(bp_usa_mat))

vcov_bp_mex <- if(ncol(bp_mex_mat) > 0) cov(bp_mex_mat) else NULL
vcov_bp_usa <- if(ncol(bp_usa_mat) > 0) cov(bp_usa_mat) else NULL

# --- 5.4 Filtro Beveridge-Nelson ---------------------------------------------

aplicar_bn <- function(panel, variables, ar_order = 4) {
  for (v in variables) {
    serie <- panel[[v]]
    validos <- !is.na(serie) & serie > 0
    log_serie <- rep(NA, length(serie))
    log_serie[validos] <- log(serie[validos])
    
    idx <- which(validos)
    if (length(idx) > (ar_order + 10)) {
      dlog <- diff(log_serie[idx])
      
      ar_fit <- tryCatch(
        ar(dlog, order.max = ar_order, method = "ols", aic = FALSE),
        error = function(e) ar(dlog, order.max = ar_order, method = "ols")
      )
      
      phi <- ar_fit$ar
      mu  <- ar_fit$x.mean
      
      tendencia_bn <- rep(NA, length(idx))
      tendencia_bn[1] <- log_serie[idx[1]]
      
      drift_lp <- mu / (1 - sum(phi))
      
      for (t in 2:length(idx)) {
        tendencia_bn[t] <- tendencia_bn[t-1] + drift_lp
      }
      
      ciclo_bn <- log_serie[idx] - tendencia_bn
      
      ciclo <- rep(NA, length(serie))
      ciclo[idx] <- ciclo_bn
      panel[[paste0("bn_", v)]] <- ciclo
    }
  }
  return(panel)
}

panel_mex <- aplicar_bn(panel_mex, vars_filtro)
panel_usa <- aplicar_bn(panel_usa, vars_filtro)

p_bn_mex <- graficar_ciclos(panel_mex, "México", "bn",
                            "Filtro Beveridge-Nelson", fuente = fuente_mex)
p_bn_usa <- graficar_ciclos(panel_usa, "Estados Unidos", "bn",
                            "Filtro Beveridge-Nelson", fuente = fuente_usa)

ggsave(file.path(res_dir, "c_bn_mex.png"), p_bn_mex, width = 10, height = 5, dpi = 150)
ggsave(file.path(res_dir, "c_bn_usa.png"), p_bn_usa, width = 10, height = 5, dpi = 150)

bn_vars <- paste0("bn_", vars_filtro)
bn_mex_mat <- panel_mex[complete.cases(panel_mex[, bn_vars[bn_vars %in% colnames(panel_mex)]]),
                        bn_vars[bn_vars %in% colnames(panel_mex)]]
bn_usa_mat <- panel_usa[complete.cases(panel_usa[, bn_vars[bn_vars %in% colnames(panel_usa)]]),
                        bn_vars[bn_vars %in% colnames(panel_usa)]]
colnames(bn_mex_mat) <- gsub("bn_", "", colnames(bn_mex_mat))
colnames(bn_usa_mat) <- gsub("bn_", "", colnames(bn_usa_mat))

vcov_bn_mex <- if(ncol(bn_mex_mat) > 0) cov(bn_mex_mat) else NULL
vcov_bn_usa <- if(ncol(bn_usa_mat) > 0) cov(bn_usa_mat) else NULL

# =============================================================================
# 6. INCISO (d): SCATTER PLOTS — CONSUMO vs PIB
# =============================================================================

cat("\n--- Generando scatter plots (d) ---\n")

graficar_scatter <- function(panel, pais, var_consumo, label_consumo, fuente = NULL) {
  df <- panel[, c("fecha", paste0("tc_", var_consumo), "tc_Y")]
  colnames(df) <- c("fecha", "tc_C", "tc_Y")
  df <- df[complete.cases(df), ]
  
  p <- ggplot(df, aes(x = tc_C, y = tc_Y)) +
    geom_point(alpha = 0.6, color = colores[var_consumo], size = 1.8) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.5) +
    scale_x_continuous(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste0("Δ%Y vs Δ%", label_consumo),
         subtitle = pais,
         x = paste0("Δ%", label_consumo), y = "Δ%Y",
         caption = fuente) +
    tema_base
  return(p)
}

tipos_consumo <- c("C", "Cs", "Cb")
labels_consumo <- c("C (Total)", "Cs (Servicios)", "Cb (Bienes)")

for (i in seq_along(tipos_consumo)) {
  p_mex <- graficar_scatter(panel_mex, "México", tipos_consumo[i],
                            labels_consumo[i], fuente = fuente_mex)
  p_usa <- graficar_scatter(panel_usa, "Estados Unidos", tipos_consumo[i],
                            labels_consumo[i], fuente = fuente_usa)
  
  ggsave(file.path(res_dir, paste0("d_scatter_", tipos_consumo[i], "_mex.png")),
         p_mex, width = 6, height = 5, dpi = 150)
  ggsave(file.path(res_dir, paste0("d_scatter_", tipos_consumo[i], "_usa.png")),
         p_usa, width = 6, height = 5, dpi = 150)
}

# =============================================================================
# 7. INCISO (e): VOLATILIDAD DE LAS TASAS DE CRECIMIENTO
# =============================================================================

cat("\n--- Calculando volatilidad (e) ---\n")

calc_volatilidad <- function(panel, variables) {
  vars_tc <- paste0("tc_", variables)
  vols <- sapply(vars_tc, function(v) sd(panel[[v]], na.rm = TRUE))
  names(vols) <- variables
  return(vols)
}

vol_mex <- calc_volatilidad(panel_mex, c("Y", "C", "Cs", "Cb", "I", "G"))
vol_usa <- calc_volatilidad(panel_usa, c("Y", "C", "Cs", "Cb", "I", "G"))

volatilidad <- data.frame(
  Variable = c("Y (PIB)", "C (Consumo total)", "Cs (Servicios)", 
               "Cb (Bienes)", "I (Inversión)", "G (Gobierno)"),
  Mexico   = round(vol_mex * 100, 2),
  EEUU     = round(vol_usa * 100, 2)
)
cat("\nVolatilidad (desv. est. de Δ%, en pp):\n")
print(volatilidad)

# =============================================================================
# 8. INCISO (f): REGRESIONES — HIP
# =============================================================================

cat("\n--- Estimando regresiones HIP (f) ---\n")

estimar_hip <- function(panel, var_c, pais) {
  resultados <- list()
  
  # Modelo 1: C_t = a + b*Y_t (NIVELES)
  df1 <- panel[complete.cases(panel[, c(var_c, "Y")]), ]
  if (nrow(df1) > 10) {
    mod1 <- lm(df1[[var_c]] ~ df1[["Y"]])
    resultados[["mod1_niveles"]] <- summary(mod1)
  }
  
  # Modelo 2: Δ%C_t = a + b*Δ%Y_t
  tc_c <- paste0("tc_", var_c)
  df2 <- panel[complete.cases(panel[, c(tc_c, "tc_Y")]), ]
  if (nrow(df2) > 10) {
    mod2 <- lm(df2[[tc_c]] ~ df2[["tc_Y"]])
    resultados[["mod2_tc"]] <- summary(mod2)
  }
  
  # Modelo 3: Δ%C_t = a + b*Δ%Y_{t-1}
  df3 <- panel[, c("fecha", tc_c, "tc_Y")]
  df3$tc_Y_lag <- c(NA, head(df3$tc_Y, -1))
  df3 <- df3[complete.cases(df3), ]
  if (nrow(df3) > 10) {
    mod3 <- lm(df3[[tc_c]] ~ df3[["tc_Y_lag"]])
    resultados[["mod3_tc_lag"]] <- summary(mod3)
  }
  
  # Modelo 4: c_t = a + b*y_t (LOGARITMOS)
  df4 <- panel[panel[[var_c]] > 0 & panel[["Y"]] > 0, ]
  if (nrow(df4) > 10) {
    mod4 <- lm(log(df4[[var_c]]) ~ log(df4[["Y"]]))
    resultados[["mod4_log"]] <- summary(mod4)
  }
  
  return(resultados)
}

tipos <- c("C", "Cs", "Cb")
reg_mex <- list()
reg_usa <- list()

for (v in tipos) {
  reg_mex[[v]] <- estimar_hip(panel_mex, v, "México")
  reg_usa[[v]] <- estimar_hip(panel_usa, v, "Estados Unidos")
}

extraer_resumen_reg <- function(reg_list, var_c) {
  modelos <- c("mod1_niveles", "mod2_tc", "mod3_tc_lag", "mod4_log")
  etiquetas <- c("C=a+bY (niveles)", "Δ%C=a+bΔ%Y", "Δ%C=a+bΔ%Y(-1)", "c=a+by (log)")
  
  resumen <- data.frame(
    Modelo = etiquetas,
    a = NA, b = NA, t_a = NA, t_b = NA, R2 = NA, stringsAsFactors = FALSE
  )
  
  for (i in seq_along(modelos)) {
    s <- reg_list[[var_c]][[modelos[i]]]
    if (!is.null(s)) {
      resumen$a[i]   <- round(s$coefficients[1, 1], 4)
      resumen$b[i]   <- round(s$coefficients[2, 1], 4)
      resumen$t_a[i] <- round(s$coefficients[1, 3], 2)
      resumen$t_b[i] <- round(s$coefficients[2, 3], 2)
      resumen$R2[i]  <- round(s$r.squared, 4)
    }
  }
  return(resumen)
}

# =============================================================================
# 9. GUARDAR TODOS LOS RESULTADOS
# =============================================================================

cat("\n--- Guardando resultados precalculados ---\n")

save(
  panel_mex, panel_usa,
  vcov_tc_mex, vcov_tc_usa,
  vcov_hp_mex, vcov_hp_usa,
  vcov_bp_mex, vcov_bp_usa,
  vcov_bn_mex, vcov_bn_usa,
  volatilidad, vol_mex, vol_usa,
  reg_mex, reg_usa,
  extraer_resumen_reg,
  tema_base, colores,
  fuente_mex, fuente_usa,
  file = file.path(res_dir, "resultados_ej3.RData")
)

cat("\n=== Script Ejercicio 3 completado ===\n")
cat("Resultados guardados en:", res_dir, "\n")
cat("Archivos generados:\n")
cat(paste(" ", list.files(res_dir), collapse = "\n"), "\n")

# Ejercicio 4 -------------------------------------------------------------


# =============================================================================
# Tarea 2 - Macroeconomía
# Ejercicio 4: Consumo de los individuos en México (ENIGH 2020)
# Script de cálculos pesados (gemelo)
# =============================================================================

# --- 0. Configuración portable ------------------------------------------------
setwd("C:/Users/spart/Desktop/Tarea_2_Macro")

base_dir <- getwd()
datos_dir <- file.path(base_dir, "Ejercicio 4", "Datos")
res_dir   <- file.path(base_dir, "Ejercicio 4", "Resultados Precalculados")

dir.create(res_dir, showWarnings = FALSE, recursive = TRUE)

# --- 0.1 Librerías -----------------------------------------------------------
paquetes <- c("readr", "dplyr", "ggplot2", "scales", "knitr", "kableExtra")

for (p in paquetes) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# --- Tema uniforme (mismo que Ejercicio 3) ---
tema_base <- theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 12),
    plot.subtitle    = element_text(size = 9, color = "grey40"),
    plot.caption     = element_text(size = 7, color = "grey50", hjust = 0),
    legend.position  = "bottom",
    legend.title     = element_blank(),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey92", linewidth = 0.15)
  )

fuente_enigh <- "Fuente: Elaboración propia con datos de la ENIGH 2020, INEGI."

# =============================================================================
# 1. CARGAR DATOS
# =============================================================================

cat("--- Cargando ENIGH 2020 ---\n")

# Buscar el CSV dentro de la carpeta Datos (puede estar en subcarpetas)
csv_files <- list.files(datos_dir, pattern = "concentradohogar.*\\.csv$",
                        recursive = TRUE, full.names = TRUE)

if (length(csv_files) == 0) stop("No se encontró el archivo concentradohogar CSV en:", datos_dir)

enigh <- read_csv(csv_files[1], show_col_types = FALSE)

cat("  Archivo:", basename(csv_files[1]), "\n")
cat("  Observaciones:", nrow(enigh), "\n")
cat("  Columnas:", ncol(enigh), "\n")

# Crear variables auxiliares
enigh <- enigh %>%
  mutate(
    ubica_geo = as.character(ubica_geo),
    entidad = as.integer(substr(sprintf("%05s", ubica_geo), 1, 2)),
    es_cdmx = entidad == 9,
    es_mujer_jefe = sexo_jefe == 2,
    es_urbano = tam_loc %in% c(1, 2, 3)
  )

# =============================================================================
# 2. INCISO (b): ESTADÍSTICAS DESCRIPTIVAS
# =============================================================================

cat("\n--- Inciso (b): Estadísticas descriptivas ---\n")

# Sin factor de expansión (muestra)
n_hogares_muestra <- nrow(enigh)
ing_prom_muestra  <- mean(enigh$ing_cor, na.rm = TRUE)
gas_prom_muestra  <- mean(enigh$gasto_mon, na.rm = TRUE)

cat("  Hogares en la muestra:", n_hogares_muestra, "\n")
cat("  Ingreso promedio (muestra):", round(ing_prom_muestra, 2), "\n")
cat("  Gasto promedio (muestra):", round(gas_prom_muestra, 2), "\n")

# Con factor de expansión (población)
n_hogares_expansion <- sum(enigh$factor, na.rm = TRUE)
ing_prom_expansion  <- weighted.mean(enigh$ing_cor, enigh$factor, na.rm = TRUE)
gas_prom_expansion  <- weighted.mean(enigh$gasto_mon, enigh$factor, na.rm = TRUE)

cat("  Hogares expandidos (población):", round(n_hogares_expansion, 0), "\n")
cat("  Ingreso promedio (expandido):", round(ing_prom_expansion, 2), "\n")
cat("  Gasto promedio (expandido):", round(gas_prom_expansion, 2), "\n")

# Tabla resumen
tabla_descriptiva <- data.frame(
  Concepto = c("Número de hogares", 
               "Ingreso corriente promedio trimestral",
               "Gasto monetario promedio trimestral"),
  Muestra = c(format(n_hogares_muestra, big.mark = ","),
              paste0("$", format(round(ing_prom_muestra, 0), big.mark = ",")),
              paste0("$", format(round(gas_prom_muestra, 0), big.mark = ","))),
  Expandido = c(format(round(n_hogares_expansion, 0), big.mark = ","),
                paste0("$", format(round(ing_prom_expansion, 0), big.mark = ",")),
                paste0("$", format(round(gas_prom_expansion, 0), big.mark = ",")))
)

# =============================================================================
# 3. INCISO (c): REGRESIÓN INGRESO-GASTO (TODAS LAS FAMILIAS)
# =============================================================================

cat("\n--- Inciso (c): Regresión todas las familias ---\n")

reg_todas <- lm(gasto_mon ~ ing_cor, data = enigh)
reg_todas_sum <- summary(reg_todas)
cat("  R²:", round(reg_todas_sum$r.squared, 4), "\n")
cat("  b (PMgC):", round(coef(reg_todas)[2], 4), "\n")

# Scatter plot
p_todas <- ggplot(enigh, aes(x = ing_cor, y = gasto_mon)) +
  geom_point(alpha = 0.15, size = 0.8, color = "#d95f02") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.6) +
  scale_x_continuous(labels = label_comma(prefix = "$"), limits = c(0, quantile(enigh$ing_cor, 0.99))) +
  scale_y_continuous(labels = label_comma(prefix = "$"), limits = c(0, quantile(enigh$gasto_mon, 0.99))) +
  labs(title = "Relación ingreso-gasto: Todas las familias",
       subtitle = paste0("ENIGH 2020 | n = ", format(nrow(enigh), big.mark = ","),
                         " | b = ", round(coef(reg_todas)[2], 4),
                         " | R² = ", round(reg_todas_sum$r.squared, 4)),
       x = "Ingreso corriente trimestral", y = "Gasto monetario trimestral",
       caption = fuente_enigh) +
  tema_base

ggsave(file.path(res_dir, "c_scatter_todas.png"), p_todas, width = 8, height = 6, dpi = 150)

# =============================================================================
# 4. INCISO (d): JEFAS MUJERES, 20-30 AÑOS, CDMX
# =============================================================================

cat("\n--- Inciso (d): Jefas mujeres 20-30 años, CDMX ---\n")

sub_d <- enigh %>%
  filter(es_mujer_jefe, edad_jefe >= 20, edad_jefe <= 30, es_cdmx)

cat("  Hogares en submuestra:", nrow(sub_d), "\n")

if (nrow(sub_d) > 10) {
  reg_d <- lm(gasto_mon ~ ing_cor, data = sub_d)
  reg_d_sum <- summary(reg_d)
  cat("  R²:", round(reg_d_sum$r.squared, 4), "\n")
  cat("  b (PMgC):", round(coef(reg_d)[2], 4), "\n")
  
  p_d <- ggplot(sub_d, aes(x = ing_cor, y = gasto_mon)) +
    geom_point(alpha = 0.5, size = 1.5, color = "#7570b3") +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.6) +
    scale_x_continuous(labels = label_comma(prefix = "$")) +
    scale_y_continuous(labels = label_comma(prefix = "$")) +
    labs(title = "Relación ingreso-gasto: Jefas mujeres 20-30 años, CDMX",
         subtitle = paste0("ENIGH 2020 | n = ", nrow(sub_d),
                           " | b = ", round(coef(reg_d)[2], 4),
                           " | R² = ", round(reg_d_sum$r.squared, 4)),
         x = "Ingreso corriente trimestral", y = "Gasto monetario trimestral",
         caption = fuente_enigh) +
    tema_base
  
  ggsave(file.path(res_dir, "d_scatter_mujeres_cdmx.png"), p_d, width = 8, height = 6, dpi = 150)
} else {
  reg_d <- NULL
  reg_d_sum <- NULL
  cat("  ADVERTENCIA: Submuestra muy pequeña para regresión\n")
}

# =============================================================================
# 5. INCISO (e): ÁMBITO URBANO
# =============================================================================

cat("\n--- Inciso (e): Ámbito urbano ---\n")

sub_e <- enigh %>% filter(es_urbano)

cat("  Hogares urbanos:", nrow(sub_e), "\n")

reg_e <- lm(gasto_mon ~ ing_cor, data = sub_e)
reg_e_sum <- summary(reg_e)
cat("  R²:", round(reg_e_sum$r.squared, 4), "\n")
cat("  b (PMgC):", round(coef(reg_e)[2], 4), "\n")

p_e <- ggplot(sub_e, aes(x = ing_cor, y = gasto_mon)) +
  geom_point(alpha = 0.15, size = 0.8, color = "#1b9e77") +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.6) +
  scale_x_continuous(labels = label_comma(prefix = "$"), limits = c(0, quantile(sub_e$ing_cor, 0.99))) +
  scale_y_continuous(labels = label_comma(prefix = "$"), limits = c(0, quantile(sub_e$gasto_mon, 0.99))) +
  labs(title = "Relación ingreso-gasto: Hogares urbanos",
       subtitle = paste0("ENIGH 2020 | n = ", format(nrow(sub_e), big.mark = ","),
                         " | b = ", round(coef(reg_e)[2], 4),
                         " | R² = ", round(reg_e_sum$r.squared, 4)),
       x = "Ingreso corriente trimestral", y = "Gasto monetario trimestral",
       caption = fuente_enigh) +
  tema_base

ggsave(file.path(res_dir, "e_scatter_urbano.png"), p_e, width = 8, height = 6, dpi = 150)

# =============================================================================
# 6. TABLA COMPARATIVA DE REGRESIONES
# =============================================================================

cat("\n--- Tabla comparativa ---\n")

extraer_reg <- function(modelo_sum, etiqueta, n) {
  if (is.null(modelo_sum)) {
    return(data.frame(Submuestra = etiqueta, n = n,
                      a = NA, b = NA, t_a = NA, t_b = NA, R2 = NA))
  }
  data.frame(
    Submuestra = etiqueta,
    n = n,
    a  = round(modelo_sum$coefficients[1, 1], 2),
    b  = round(modelo_sum$coefficients[2, 1], 4),
    t_a = round(modelo_sum$coefficients[1, 3], 2),
    t_b = round(modelo_sum$coefficients[2, 3], 2),
    R2  = round(modelo_sum$r.squared, 4)
  )
}

tabla_regresiones <- rbind(
  extraer_reg(reg_todas_sum, "Todas las familias", nrow(enigh)),
  extraer_reg(reg_d_sum, "Jefas mujeres 20-30, CDMX", nrow(sub_d)),
  extraer_reg(reg_e_sum, "Hogares urbanos", nrow(sub_e))
)

print(tabla_regresiones)

# =============================================================================
# 7. GUARDAR RESULTADOS
# =============================================================================

cat("\n--- Guardando resultados ---\n")

save(
  enigh,
  tabla_descriptiva,
  reg_todas, reg_todas_sum,
  reg_d, reg_d_sum, sub_d,
  reg_e, reg_e_sum, sub_e,
  tabla_regresiones,
  tema_base, fuente_enigh,
  n_hogares_muestra, n_hogares_expansion,
  ing_prom_muestra, ing_prom_expansion,
  gas_prom_muestra, gas_prom_expansion,
  file = file.path(res_dir, "resultados_ej4.RData")
)

cat("\n=== Script Ejercicio 4 completado ===\n")
cat("Resultados guardados en:", res_dir, "\n")
cat("Archivos generados:\n")
cat(paste(" ", list.files(res_dir), collapse = "\n"), "\n")













