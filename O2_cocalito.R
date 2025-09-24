#O2 Cocalito y Ensenada 23/08/2025 al 25/08/2025

#Limpiar el entorno
rm(list = ls())

#librerias
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(purrr)
library(cowplot)

#Coloco la carpeta de trabajo
setwd("C:/Users/carol/Documents/Master Oceanografía/TFM/Campaña/Datos campaña/Sensores/O2/")
getwd()
list.files()# esto es para verificar que este en la carperta correcta


# Zona horaria
tz_local <- "America/Tegucigalpa"

#Leer miniDOT
read_minidot <- function(file){
  read_csv(file, skip = 2, show_col_types = FALSE) %>%
    rename(
      Time_sec = `Time (sec)`,
      BV       = `BV (Volts)`,
      Temp_C   = `T (deg C)`,
      DO_mgL   = `DO (mg/l)`,
      Q        = `Q ()`
    ) %>%
    mutate(
      datetime_utc   = as_datetime(Time_sec, tz = "UTC"),
      datetime_local = with_tz(datetime_utc, tz_local)
    )
}

#   COCALITO  (23y  24-ago 11:10

coc23 <- read_minidot("2025-08-23 164200Z.txt")
coc24 <- read_minidot("2025-08-24 164200Z.txt")


cocalito_all <- bind_rows(coc23, coc24) %>% arrange(datetime_local)

cocalito_start <- ymd_hm("2025-08-23 11:10", tz = tz_local)
cocalito_end   <- ymd_hm("2025-08-24 11:10", tz = tz_local)

cocalito_win <- cocalito_all %>%
  filter(datetime_local >= cocalito_start,
         datetime_local <= cocalito_end)

# Ensenada 25 y 24
ens24 <- read_minidot("2025-08-24 164200Z.txt")
ens25 <- read_minidot("2025-08-25 164200Z.txt")

ensenada_all <- bind_rows(ens24, ens25) %>% arrange(datetime_local)

ensenada_start <- ymd_hm("2025-08-24 12:40", tz = tz_local)
ensenada_end   <- ymd_hm("2025-08-25 12:40", tz = tz_local)

ensenada_win <- ensenada_all %>%
  filter(datetime_local >= ensenada_start,
         datetime_local <= ensenada_end)
#Graficos
panel_DO <- function(dat, titulo){
  ggplot(dat, aes(x = datetime_local, y = DO_mgL)) +
    geom_line(linewidth = 0.6, color = "black") +
    geom_point(size = 1.2, color = "black") +
    scale_x_datetime(date_labels = "%H:%M") +
    labs(x = "Time", y = "Dissolved Oxygen (mg/L)", title = titulo) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 9),
      aspect.ratio = 1   # hace el panel aproximadamente cuadrado
    )
}

pA <- panel_DO(cocalito_win, "Cocalito")
pB <- panel_DO(ensenada_win, "La Ensenada")

# ============================
#   Combinar paneles A–B (2x1 o 1x2)
# ============================

# 1x2 horizontal (como en tu ejemplo; ajusta ncol/nrow a tu preferencia)
fig_AB <- plot_grid(
  pA, pB,
  labels = c("A", "B"),
  label_fontface = "bold",
  ncol = 2, align = "hv"
)

# Mostrar
print(fig_AB)

# (Opcional) Guardar
ggsave("MiniDOT_DO_paneles_AB.png", fig_AB, width = 9, height = 4.5, dpi = 300)

# ============================
#   Paneles A–D (DO y Temp)
# ============================

# Función general de panel (recibe variable y título)
panel_var <- function(dat, var, titulo, ylab){
  ggplot(dat, aes(x = datetime_local, y = .data[[var]])) +
    geom_line(linewidth = 0.6, color = "black") +
    geom_point(size = 1.2, color = "black") +
    scale_x_datetime(date_labels = "%H:%M") +
    labs(x = "Time", y = ylab, title = titulo) +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 9),
      aspect.ratio = 1
    )
}

# Paneles
pA <- panel_var(cocalito_win, "DO_mgL", "Cocalito (DO)", "Dissolved Oxygen (mg/L)")
pB <- panel_var(ensenada_win, "DO_mgL", "Ensenada (DO)", "Dissolved Oxygen (mg/L)")
pC <- panel_var(cocalito_win, "Temp_C", "Cocalito (Temp)", "Temperature (°C)")
pD <- panel_var(ensenada_win, "Temp_C", "Ensenada (Temp)", "Temperature (°C)")

# Combinar 2×2 con letras A–D
fig_ABCD <- plot_grid(
  pA, pB, pC, pD,
  labels = c("A", "B", "C", "D"),
  label_fontface = "bold",
  ncol = 2, align = "hv"
)

# Mostrar figura
print(fig_ABCD)

# Guardar
ggsave("MiniDOT_DO_Temp_paneles_ABCD.png", fig_ABCD, width = 9, height = 9, dpi = 300)

# Combinar datasets de ventana ya recortada, añadiendo columna "site"
df_sites <- bind_rows(
  cocalito_win %>% mutate(site = "Cocalito"),
  ensenada_win %>% mutate(site = "Ensenada")
)

# Resumen estadístico por sitio
resumen <- df_sites %>%
  group_by(site) %>%
  summarise(
    Temp_min = min(Temp_C, na.rm = TRUE),
    Temp_max = max(Temp_C, na.rm = TRUE),
    Temp_mean = mean(Temp_C, na.rm = TRUE),
    DO_min = min(DO_mgL, na.rm = TRUE),
    DO_max = max(DO_mgL, na.rm = TRUE),
    DO_mean = mean(DO_mgL, na.rm = TRUE),
    .groups = "drop"
  )

print(resumen)
