library(gt)
library(dplyr)
library(moments)
library(ggplot2)
library(tidyr)

# Asegurar que las variables numéricas estén en el formato correcto
base_modelos_para_R <- base_modelos_para_R %>%
  mutate(
    `Numero de Vehiculos Siniestrados` = as.numeric(`Numero de Vehiculos Siniestrados`),
    `Monto de Siniestros` = as.numeric(`Monto de Siniestros`),
    `Monto Pagado` = as.numeric(`Monto Pagado`)
  )

# Calcular las medidas
desv_est <- sapply(base_modelos_para_R[, c("Numero de Vehiculos Siniestrados", "Monto de Siniestros", "Monto Pagado")], sd)
varianza <- sapply(base_modelos_para_R[, c("Numero de Vehiculos Siniestrados", "Monto de Siniestros", "Monto Pagado")], var)
rango <- sapply(base_modelos_para_R[, c("Numero de Vehiculos Siniestrados", "Monto de Siniestros", "Monto Pagado")], function(x) max(x) - min(x))
coef_var <- sapply(base_modelos_para_R[, c("Numero de Vehiculos Siniestrados", "Monto de Siniestros", "Monto Pagado")], function(x) sd(x) / mean(x))
sesgo <- sapply(base_modelos_para_R[, c("Numero de Vehiculos Siniestrados", "Monto de Siniestros", "Monto Pagado")], skewness)
curtosis <- sapply(base_modelos_para_R[, c("Numero de Vehiculos Siniestrados", "Monto de Siniestros", "Monto Pagado")], kurtosis)
suma <- sapply(base_modelos_para_R[, c("Numero de Vehiculos Siniestrados", "Monto de Siniestros", "Monto Pagado")], sum)

# hacemos el resumen para la tabla
resumen_completo <- tibble(
  Estadística = c("Mínimo", "1er Cuartil", "Mediana", "Media", "3er Cuartil", "Máximo", 
                  "Desv. Est.", "Varianza", "Rango", "Coef. Var.", "Sesgo", "Curtosis", "Suma"),
  `Vehículos Siniestrados` = c(1.00, 1.00, 3.00, 30.14, 11.00, 7837.00, 
                               desv_est[1], varianza[1], rango[1], coef_var[1], sesgo[1], curtosis[1], suma[1]),
  `Monto de Siniestros` = c(0, 8991, 36900, 356580, 166820, 51558310, 
                            desv_est[2], varianza[2], rango[2], coef_var[2], sesgo[2], curtosis[2], suma[2]),
  `Monto Pagado` = c(0, 8737, 34428, 309196, 154296, 40957671, 
                     desv_est[3], varianza[3], rango[3], coef_var[3], sesgo[3], curtosis[3], suma[3])
)


tabla_gt <- resumen_completo %>%
  gt() %>%
  tab_header(
    title = "Resumen Completo de las Variables",
    subtitle = "Estadísticas Descriptivas: Vehículos Siniestrados, Monto de Siniestros y Monto Pagado"
  ) %>%
  fmt_number(
    columns = -Estadística, # Formatear solo las columnas numéricas
    decimals = 2
  ) %>%
  cols_label(
    Estadística = "Estadística",
    `Vehículos Siniestrados` = "Vehículos Siniestrados",
    `Monto de Siniestros` = "Monto de Siniestros",
    `Monto Pagado` = "Monto Pagado"
  ) %>%
  tab_options(
    table.font.size = "medium",
    column_labels.font.weight = "bold",
    column_labels.background.color = "lightgray",
    table.border.top.width = px(2),
    table.border.top.color = "black",
    table.border.bottom.width = px(2),
    table.border.bottom.color = "black"
  )

# Mostrar la tabla estilizada
print(tabla_gt)

#-------------------------------------------------------------------------
#analisis mediante hisograma y boxplot

# Transformar las variables aplicando logaritmo natural
base_modelos_para_R <- base_modelos_para_R %>%
  mutate(
    `Log Numero de Vehiculos Siniestrados` = log(`Numero de Vehiculos Siniestrados` + 1),
    `Log Monto de Siniestros` = log(`Monto de Siniestros` + 1),
    `Log Monto Pagado` = log(`Monto Pagado` + 1)
  )

# Histogramas con datos suavizados
ggplot(base_modelos_para_R, aes(x = `Log Numero de Vehiculos Siniestrados`)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución Logarítmica: Número de Vehículos Siniestrados",
       x = "Log Número de Vehículos Siniestrados", y = "Frecuencia") +
  theme_minimal()

ggplot(base_modelos_para_R, aes(x = `Log Monto de Siniestros`)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribución Logarítmica: Monto de Siniestros",
       x = "Log Monto de Siniestros", y = "Frecuencia") +
  theme_minimal()

ggplot(base_modelos_para_R, aes(x = `Log Monto Pagado`)) +
  geom_histogram(binwidth = 0.1, fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Distribución Logarítmica: Monto Pagado",
       x = "Log Monto Pagado", y = "Frecuencia") +
  theme_minimal()

# Boxplot - Log Número de Vehículos Siniestrados
boxplot_vehiculos <- ggplot(base_modelos_para_R, aes(y = `Log Numero de Vehiculos Siniestrados`)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, fill = "blue", alpha = 0.7) +
  labs(
    title = "Boxplot: Log Número de Vehículos Siniestrados",
    y = "Log Número de Vehículos",
    x = NULL
  ) +
  theme_minimal(base_size = 15)

# Mostrar y guardar el boxplot del Número de Vehículos Siniestrados
print(boxplot_vehiculos)
ggsave("Boxplot_Log_Numero_de_Vehiculos_Siniestrados.png", plot = boxplot_vehiculos, width = 8, height = 6)

# Boxplot - Log Monto de Siniestros
boxplot_siniestros <- ggplot(base_modelos_para_R, aes(y = `Log Monto de Siniestros`)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, fill = "green", alpha = 0.7) +
  labs(
    title = "Boxplot: Log Monto de Siniestros",
    y = "Log Monto de Siniestros",
    x = NULL
  ) +
  theme_minimal(base_size = 15)

# Mostrar y guardar el boxplot del Monto de Siniestros
print(boxplot_siniestros)
ggsave("Boxplot_Log_Monto_de_Siniestros.png", plot = boxplot_siniestros, width = 8, height = 6)

# Boxplot - Log Monto Pagado
boxplot_pagado <- ggplot(base_modelos_para_R, aes(y = `Log Monto Pagado`)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, fill = "purple", alpha = 0.7) +
  labs(
    title = "Boxplot: Log Monto Pagado",
    y = "Log Monto Pagado",
    x = NULL
  ) +
  theme_minimal(base_size = 15)

# Mostrar y guardar el boxplot del Monto Pagado
print(boxplot_pagado)
ggsave("Boxplot_Log_Monto_Pagado.png", plot = boxplot_pagado, width = 8, height = 6)

# Boxplot combinado de resumen (todas las variables)
boxplot_resumen <- base_modelos_para_R %>%
  pivot_longer(
    cols = c(`Log Numero de Vehiculos Siniestrados`, `Log Monto de Siniestros`, `Log Monto Pagado`),
    names_to = "Variable",
    values_to = "Valor"
  ) %>%
  ggplot(aes(x = Variable, y = Valor, fill = Variable)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2) +
  labs(
    title = "Boxplot Resumen: Variables Logarítmicas",
    x = "Variable",
    y = "Valor Transformado (Log)"
  ) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "none")  # Elimina la leyenda para mayor claridad

# Mostrar el gráfico combinado
print(boxplot_resumen)

# Guardar todos los gráficos
ggsave("Boxplot_Log_Numero_de_Vehiculos_Siniestrados.png", plot = boxplot_vehiculos, width = 8, height = 6)
ggsave("Boxplot_Log_Monto_de_Siniestros.png", plot = boxplot_siniestros, width = 8, height = 6)
ggsave("Boxplot_Log_Monto_Pagado.png", plot = boxplot_pagado, width = 8, height = 6)
ggsave("Boxplot_Log_Resumen_Variables.png", plot = boxplot_resumen, width = 10, height = 8)


#---------------------------------------------------------------------
# Cargar librerías necesarias
# Cargar librerías necesarias
library(dplyr)
library(moments)  # Para calcular curtosis y sesgo
library(gt)       # Para generar tablas estilizadas

# Generar el resumen estadístico en formato vertical
resumen_estadistico <- tibble(
  `Medida Estadística` = c(
    "Media", "Mediana", "Desv. Estándar", "Varianza",
    "Sesgo", "Curtosis", "Coef. Variación"
  ),
  `Vehículos Siniestrados` = c(
    mean(base_modelos_para_R$`Log Numero de Vehiculos Siniestrados`, na.rm = TRUE),
    median(base_modelos_para_R$`Log Numero de Vehiculos Siniestrados`, na.rm = TRUE),
    sd(base_modelos_para_R$`Log Numero de Vehiculos Siniestrados`, na.rm = TRUE),
    var(base_modelos_para_R$`Log Numero de Vehiculos Siniestrados`, na.rm = TRUE),
    skewness(base_modelos_para_R$`Log Numero de Vehiculos Siniestrados`, na.rm = TRUE),
    kurtosis(base_modelos_para_R$`Log Numero de Vehiculos Siniestrados`, na.rm = TRUE),
    sd(base_modelos_para_R$`Log Numero de Vehiculos Siniestrados`, na.rm = TRUE) /
      mean(base_modelos_para_R$`Log Numero de Vehiculos Siniestrados`, na.rm = TRUE)
  ),
  `Monto de Siniestros` = c(
    mean(base_modelos_para_R$`Log Monto de Siniestros`, na.rm = TRUE),
    median(base_modelos_para_R$`Log Monto de Siniestros`, na.rm = TRUE),
    sd(base_modelos_para_R$`Log Monto de Siniestros`, na.rm = TRUE),
    var(base_modelos_para_R$`Log Monto de Siniestros`, na.rm = TRUE),
    skewness(base_modelos_para_R$`Log Monto de Siniestros`, na.rm = TRUE),
    kurtosis(base_modelos_para_R$`Log Monto de Siniestros`, na.rm = TRUE),
    sd(base_modelos_para_R$`Log Monto de Siniestros`, na.rm = TRUE) /
      mean(base_modelos_para_R$`Log Monto de Siniestros`, na.rm = TRUE)
  ),
  `Monto Pagado` = c(
    mean(base_modelos_para_R$`Log Monto Pagado`, na.rm = TRUE),
    median(base_modelos_para_R$`Log Monto Pagado`, na.rm = TRUE),
    sd(base_modelos_para_R$`Log Monto Pagado`, na.rm = TRUE),
    var(base_modelos_para_R$`Log Monto Pagado`, na.rm = TRUE),
    skewness(base_modelos_para_R$`Log Monto Pagado`, na.rm = TRUE),
    kurtosis(base_modelos_para_R$`Log Monto Pagado`, na.rm = TRUE),
    sd(base_modelos_para_R$`Log Monto Pagado`, na.rm = TRUE) /
      mean(base_modelos_para_R$`Log Monto Pagado`, na.rm = TRUE)
  )
)

# Crear la tabla estilizada sin argumentos inválidos
tabla_estadistica <- resumen_estadistico %>%
  gt() %>%
  tab_header(
    title = "Resumen Estadístico",
    subtitle = "Análisis de Variables Transformadas con Logaritmos"
  ) %>%
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  cols_label(
    `Medida Estadística` = "Medida Estadística",
    `Vehículos Siniestrados` = "Vehículos Siniestrados",
    `Monto de Siniestros` = "Monto de Siniestros",
    `Monto Pagado` = "Monto Pagado"
  ) %>%
  tab_options(
    column_labels.font.weight = "bold",
    table.border.top.width = px(2),
    table.border.bottom.width = px(2),
    data_row.padding = px(5),
    table.font.size = "medium"
  )

# Mostrar la tabla estilizada
print(tabla_estadistica)

#----------------------------------------------------------------------

# Frecuencia de siniestros por Tipo de Pérdida y Causa del Siniestro
frecuencia_tipo_causa <- base_modelos_para_R %>%
  group_by(`Tipo de Perdida`, `Causa del siniestro`) %>%
  summarise(
    Frecuencia = n(),  # Conteo de siniestros
    Costo_Total = sum(`Monto de Siniestros`, na.rm = TRUE)  # Suma de montos de siniestros
  ) %>%
  arrange(desc(Frecuencia))

# Mostrar los 10 eventos más comunes
print(head(frecuencia_tipo_causa, 10))

# Visualización: Frecuencia de siniestros por causa del siniestro
ggplot(frecuencia_tipo_causa, aes(x = reorder(`Causa del siniestro`, -Frecuencia), y = Frecuencia, fill = `Tipo de Perdida`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Frecuencia de Siniestros por Tipo de Pérdida y Causa",
    x = "Causa del Siniestro",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-------------------------------------------------------------------
# Comparación entre marcas: Frecuencia y costo promedio
comparacion_marcas <- base_modelos_para_R %>%
  group_by(Marca) %>%
  summarise(
    Frecuencia = n(),
    Costo_Total = sum(`Monto de Siniestros`, na.rm = TRUE),
    Costo_Promedio = mean(`Monto de Siniestros`, na.rm = TRUE)
  ) %>%
  arrange(desc(Frecuencia))

# Mostrar el resumen de las 10 marcas más afectadas
print(head(comparacion_marcas, 10))

# Visualización: Frecuencia y montos de siniestros por marca
ggplot(comparacion_marcas, aes(x = reorder(Marca, -Frecuencia), y = Frecuencia, fill = Costo_Promedio)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Frecuencia y Montos de Siniestros por Marca",
    x = "Marca",
    y = "Frecuencia de Siniestros"
  ) +
  theme_minimal() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#---------------------------------------------------------------------
# Análisis de cobertura: Frecuencia y costos
analisis_cobertura <- base_modelos_para_R %>%
  group_by(Cobertura) %>%
  summarise(
    Frecuencia = n(),
    Costo_Total = sum(`Monto de Siniestros`, na.rm = TRUE),
    Costo_Promedio = mean(`Monto de Siniestros`, na.rm = TRUE)
  ) %>%
  arrange(desc(Frecuencia))

# Mostrar el resumen de las coberturas más frecuentes
print(head(analisis_cobertura, 10))

# Visualización: Coberturas más utilizadas y sus costos
ggplot(analisis_cobertura, aes(x = reorder(Cobertura, -Frecuencia), y = Frecuencia, fill = Costo_Total)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Frecuencia y Costos por Tipo de Cobertura",
    x = "Tipo de Cobertura",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  scale_fill_gradient(low = "green", high = "orange") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#---------------------------------------------------------------------
# Correlación entre Monto de Siniestros y Monto Pagado
correlacion_montos <- cor(base_modelos_para_R$`Monto de Siniestros`, base_modelos_para_R$`Monto Pagado`, use = "complete.obs")

# Mostrar el coeficiente de correlación
print(paste("Coeficiente de correlación:", correlacion_montos))

# Visualización: Relación entre Monto de Siniestros y Monto Pagado
library(ggplot2)
ggplot(base_modelos_para_R, aes(x = `Monto de Siniestros`, y = `Monto Pagado`)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Relación entre Monto de Siniestros y Monto Pagado",
    x = "Monto de Siniestros",
    y = "Monto Pagado"
  ) +
  theme_minimal()
#-------------------------------------------------------------------------
# Impacto de la causa del siniestro en los montos
impacto_causa <- base_modelos_para_R %>%
  group_by(`Causa del siniestro`) %>%
  summarise(
    Frecuencia = n(),
    Costo_Total = sum(`Monto de Siniestros`, na.rm = TRUE),
    Costo_Promedio = mean(`Monto de Siniestros`, na.rm = TRUE)
  ) %>%
  arrange(desc(Costo_Promedio))

# Mostrar las 10 causas con los costos promedio más altos
print(head(impacto_causa, 10))

# Visualización: Costo promedio por causa del siniestro
ggplot(impacto_causa, aes(x = reorder(`Causa del siniestro`, -Costo_Promedio), y = Costo_Promedio)) +
  geom_bar(stat = "identity", fill = "orange", alpha = 0.7) +
  labs(
    title = "Costo Promedio por Causa del Siniestro",
    x = "Causa del Siniestro",
    y = "Costo Promedio (MXN)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#----------------------------------------------------------------
# Comparación de montos pagados según tipo de pérdida
comparacion_perdida <- base_modelos_para_R %>%
  group_by(`Tipo de Perdida`) %>%
  summarise(
    Costo_Total = sum(`Monto Pagado`, na.rm = TRUE),
    Costo_Promedio = mean(`Monto Pagado`, na.rm = TRUE),
    Frecuencia = n()
  ) %>%
  arrange(desc(Costo_Total))

# Mostrar el resumen por tipo de pérdida
print(comparacion_perdida)

# Visualización: Montos Pagados por Tipo de Pérdida
ggplot(comparacion_perdida, aes(x = reorder(`Tipo de Perdida`, -Costo_Promedio), y = Costo_Promedio, fill = Frecuencia)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Montos Pagados por Tipo de Pérdida",
    x = "Tipo de Pérdida",
    y = "Costo Promedio (MXN)"
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#--------------------------------------------------------------------
# Boxplot: Dispersión de montos pagados según Tipo de Pérdida
ggplot(base_modelos_para_R, aes(x = `Tipo de Perdida`, y = `Monto Pagado`, fill = `Tipo de Perdida`)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = 0.7) +
  labs(
    title = "Dispersión de Montos Pagados según Tipo de Pérdida",
    x = "Tipo de Pérdida",
    y = "Monto Pagado (MXN)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
#-----------------------------------------------------------------------
# Boxplot: Dispersión de montos pagados según Causa del Siniestro
ggplot(base_modelos_para_R, aes(x = `Causa del siniestro`, y = `Monto Pagado`, fill = `Causa del siniestro`)) +
  geom_boxplot(outlier.color = "red", outlier.size = 2, alpha = 0.7) +
  labs(
    title = "Dispersión de Montos Pagados según Causa del Siniestro",
    x = "Causa del Siniestro",
    y = "Monto Pagado (MXN)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
#----------------------------------------------------------------------
# Gráfico de barras: Frecuencia de siniestros según Marca
frecuencia_marca <- base_modelos_para_R %>%
  group_by(Marca) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia))

ggplot(frecuencia_marca, aes(x = reorder(Marca, -Frecuencia), y = Frecuencia, fill = Frecuencia)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  labs(
    title = "Frecuencia de Siniestros según Marca",
    x = "Marca",
    y = "Frecuencia de Siniestros"
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#-----------------------------------------------------------------------
