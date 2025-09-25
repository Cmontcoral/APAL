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

# Leer los txt por sitio Cocalito y La Ensenada

coc23 <- read_minidot("2025-08-23 164200Z.txt")
coc24 <- read_minidot("2025-08-24 164200Z.txt")
ens24 <- read_minidot("2025-08-24 164200Z.txt")
ens25 <- read_minidot("2025-08-25 164200Z.txt")

#Cocalito
cocalito_all <- bind_rows(coc24, coc25) |> arrange(datetime_local)
ensenada_win <- ensenada_all |>
  filter(
    datetime_local >= ymd_hm("2025-08-23 11:10", tz = tz_local),
    datetime_local <= ymd_hm("2025-08-24 11:10", tz = tz_local)
  ) |>
  mutate(site = "Cocalito")

# Ensenada 
ensenada_all <- bind_rows(ens24, ens25) |> arrange(datetime_local)
ensenada_win <- ensenada_all |>
  filter(
    datetime_local >= ymd_hm("2025-08-24 12:40", tz = tz_local),
    datetime_local <= ymd_hm("2025-08-25 12:40", tz = tz_local)
  ) |>
  mutate(site = "Ensenada")
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

##
library(ggrepel)
library(cowplot)

dual_panel_annotated <- function(dat, titulo){
  # Rangos y transformación lineal para eje secundario
  rDO  <- range(dat$DO_mgL, na.rm = TRUE)
  rTMP <- range(dat$Temp_C, na.rm = TRUE)
  a <- diff(rDO) / diff(rTMP)
  b <- rDO[1] - a * rTMP[1]
  
  # Hallar extremos
  i_do_min <- which.min(dat$DO_mgL); i_do_max <- which.max(dat$DO_mgL)
  i_tp_min <- which.min(dat$Temp_C); i_tp_max <- which.max(dat$Temp_C)
  
  pts <- tibble::tibble(
    var  = c("DO_min","DO_max","Temp_min","Temp_max"),
    time = c(dat$datetime_local[i_do_min], dat$datetime_local[i_do_max],
             dat$datetime_local[i_tp_min], dat$datetime_local[i_tp_max]),
    y_do = c(dat$DO_mgL[i_do_min], dat$DO_mgL[i_do_max],
             a*dat$Temp_C[i_tp_min]+b,    a*dat$Temp_C[i_tp_max]+b),
    lab  = c(
      paste0("DO min\n",  format(dat$datetime_local[i_do_min], "%H:%M")),
      paste0("DO max\n",  format(dat$datetime_local[i_do_max], "%H:%M")),
      paste0("T min\n",   format(dat$datetime_local[i_tp_min], "%H:%M")),
      paste0("T max\n",   format(dat$datetime_local[i_tp_max], "%H:%M"))
    ),
    col  = c("DO (mg/L)","DO (mg/L)","Temp (°C)","Temp (°C)")
  )
  
  ggplot(dat, aes(x = datetime_local)) +
    # Series O2
    geom_line(aes(y = DO_mgL, color = "DO (mg/L)"), linewidth = 0.6) +
    geom_point(aes(y = DO_mgL, color = "DO (mg/L)"), size = 1.2, alpha = 0.7) +
    # Series Temp re-escaladas
    geom_line(aes(y = a*Temp_C + b, color = "Temp (°C)"), linewidth = 0.6) +
    geom_point(aes(y = a*Temp_C + b, color = "Temp (°C)"), size = 1.0, alpha = 0.7) +
    # Puntos extremos
    geom_point(data = pts, aes(x = time, y = y_do, color = col), size = 2.0) +
    ggrepel::geom_label_repel(
      data = pts,
      aes(x = time, y = y_do, label = lab, fill = col),
      color = "white", size = 3, label.size = 0,
      box.padding = 0.3, point.padding = 0.2, seed = 123,
      segment.size = 0.3
    ) +
    scale_color_manual(
      name = NULL,
      values = c("DO (mg/L)" = "navy", "Temp (°C)" = "firebrick")
    ) +
    scale_fill_manual(
      name = NULL,
      values = c("DO (mg/L)" = "navy", "Temp (°C)" = "firebrick"),
      guide = "none"
    ) +
    scale_x_datetime(date_labels = "%H:%M") +  # solo hora:min
    scale_y_continuous(
      name = "Dissolved Oxygen (mg/L)",
      sec.axis = sec_axis(~ (. - b)/a, name = "Temperature (°C)")
    ) +
    labs(title = titulo, x = "Local time (America/Tegucigalpa)") +
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0, face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title = element_text(size = 11),
      axis.text  = element_text(size = 9),
      legend.position = "top",
      aspect.ratio = 0.8
    )
}

# Paneles
p_cocalito_anno <- dual_panel_annotated(cocalito_win, "Cocalito — DO & Temp")
p_ensenada_anno <- dual_panel_annotated(ensenada_win, "Ensenada — DO & Temp")

# Combinar A–B siempre visibles
fig_AB_ext <- plot_grid(
  p_cocalito_anno, p_ensenada_anno,
  labels = c("A","B"), label_fontface = "bold",
  ncol = 2, align = "hv"
)

print(fig_AB_ext)
ggsave("Fig_AB_DO_Temp_anotada_refinada.png", fig_AB_ext, width = 11, height = 5.5, dpi = 300)
