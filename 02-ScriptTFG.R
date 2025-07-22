##Estimado lector, aquí se recoge todo el código utilizado en el trabajo explicado por secciones, deberá buscar:
##ADAPTAR SI SE QUIERE USAR
#Para modificar las lecturas de los datos a usar

############################################################
# 1. CARGA DE LIBRERÍAS NECESARIAS
############################################################

# Lectura y manipulación de datos
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)

# Análisis estadístico y multivariante
library(FactoMineR)
library(factoextra)
library(psych)

# Visualización
library(ggplot2)
library(ggrepel)
library(gplots)

############################################################
# 2. IMPORTACIÓN Y PREPROCESADO DE DATOS
############################################################
# Cargar datos
datos <- read_csv("data/DatosTFG.csv")
load("data/barrios.rda")
# Cargar la base de datos con resultados electorales y variables socioeconómicas
#DatosTFG <- read_csv("Downloads/DatosTFG.csv") ##ADAPTAR SI SE QUIERE USAR
# Cargar el shapefile de barrios ya procesado
#load("/Users/alonciscar/Downloads/barrios.rda") ##ADAPTAR SI SE QUIERE USAR

# Extraer la columna de nombres de barrios y eliminarla del conjunto de datos
BARRIOS <- DatosTFG$Barrio
dt <- DatosTFG %>% select(-Barrio)

# Sustituir valores NA por ceros en todas las columnas (por simplicidad del análisis)
dt <- dt %>% mutate(across(everything(), ~replace_na(., 0)))

# Corregir nombres de columnas mal escritas: cambiar "Auro_" por "Auto_"
names(dt) <- gsub("Auro_", "Auto_", names(dt))

# Detectar columnas con porcentajes escritos como caracteres (con %)
chr_cols <- names(dt)[sapply(dt, is.character)]
porc_cols <- chr_cols[sapply(dt[chr_cols], function(col) any(grepl("%$", col)))]

# Limpiar y convertir porcentajes a formato numérico decimal
dt <- dt %>%
  mutate(across(all_of(porc_cols), ~ {
    x <- gsub("%", "", .)
    x <- gsub(",", ".", x)
    as.numeric(x) / 100
  }))

# Convertir cualquier otro carácter con coma decimal a número
dt <- dt %>%
  mutate(across(where(is.character), ~ as.numeric(gsub(",", ".", .))))

# Renombrar la variable mal etiquetada
dt <- dt %>% rename(Porc_may65 = Porc_may18)
dt <- dt %>%
  rename(Porc_ingresos_pensiones = Porc_ingresos_pensiones_prestaciones)

############################################################
# 3. TEST DE INDEPENDENCIA CHI-CUADRADO
############################################################

# Separar matrices por tipo de elección: municipales, autonómicas y generales
dt_muni <- dt %>% select(starts_with("Muni_"))
dt_auto <- dt %>% select(starts_with("Auto_"))
dt_gene <- dt %>% select(starts_with("Gene_"))

# Dividir entre variables administrativas (las primeras columnas) e ideológicas (voto a partidos)
# Se asume que las 6 primeras columnas son datos administrativos
muni_admin <- dt_muni[, 1:6]
muni_ideo  <- dt_muni[, 7:ncol(dt_muni)]

auto_admin <- dt_auto[, 1:6]
auto_ideo  <- dt_auto[, 7:ncol(dt_auto)]

gene_admin <- dt_gene[, 1:7] %>% select(-Gene_validos)
gene_ideo  <- dt_gene[, 8:ncol(dt_gene)]

# Aplicar el test de independencia chi-cuadrado a cada matriz de votos por tipo de elección
cat("\n\n---- TESTS CHI-CUADRADO ----\n")
cat("Municipales - Voto por partido:\n")
print(chisq.test(as.matrix(muni_ideo)))

cat("\nAutonómicas - Voto por partido:\n")
print(chisq.test(as.matrix(auto_ideo)))

cat("\nGenerales - Voto por partido:\n")
print(chisq.test(as.matrix(gene_ideo)))

############################################################
# 4. CARGA DE GEOMETRÍA Y PREPARACIÓN DE BARRIOS
############################################################


# Eliminar el barrio de "EL PORT", que no se considera en el análisis
barrios_new <- barrios[barrios$NOMBRE != "EL PORT", ]

# Convertir a objeto espacial sf y asignar sistema de referencia
library(sf)
barrios_sf <- st_as_sf(barrios_new)
barrios_sf <- st_set_crs(barrios_sf, 4326)
barrios_sf <- st_make_valid(barrios_sf)

# Convertir códigos de distrito y barrio a formato numérico
barrios_sf <- barrios_sf %>%
  mutate(
    CODDISTRIT = as.numeric(as.character(CODDISTRIT)),
    CODDISTBAR = as.numeric(as.character(CODDISTBAR))
  )

############################################################
# 5.1 MAPA DE DISTRITOS PERIFÉRICOS (16–19)
############################################################

# Selección de barrios de los distritos periféricos (16–19)
periferia <- barrios_sf %>% filter(CODDISTRIT %in% 16:19)
periferia$DISTRITO_FACTOR <- factor(periferia$CODDISTRIT, levels = 16:19)

# Asignar paleta de colores específica
library(RColorBrewer)
library(terra)
library(tmap)
colores_16_19 <- setNames(brewer.pal(4, "Set2"), as.character(16:19))

# Crear mapa base en gris
base_mapa <- tm_shape(barrios_sf) +
  tm_polygons(col = "gray90", border.col = "white")

# Añadir periferia coloreada
periferia_mapa <- base_mapa +
  tm_shape(periferia) +
  tm_polygons("DISTRITO_FACTOR",
              palette = colores_16_19,
              title = "Distrito",
              border.col = "white",
              textNA = "") +
  tm_text("CODDISTBAR", size = 0.8, col = "black", shadow = TRUE) +
  tm_layout(
    title = "Distritos periféricos de València",
    title.position = c("left", "bottom"),
    title.size = 1.2,
    legend.outside = TRUE,
    legend.bg.color = "white",
    legend.bg.alpha = 1
  )

# Mostrar mapa
periferia_mapa

############################################################
# 5.2 MAPA DE DISTRITOS CENTRALES (1–15)
############################################################

# Asegurar formato correcto y ordenado de factores
centrales <- barrios_sf %>% 
  mutate(lat = st_coordinates(st_centroid(geometry))[,2]) %>% 
  filter(lat >= 39.43, lat <= 39.50)
centrales$CODDISTRIT <- as.integer(centrales$CODDISTRIT)
niveles_presentes <- sort(unique(centrales$CODDISTRIT))
centrales$DISTRITO_FACTOR <- factor(centrales$CODDISTRIT, levels = niveles_presentes)

# Crear paleta personalizada de 15 colores
colores_1_15 <- colorRampPalette(brewer.pal(12, "Paired"))(length(niveles_presentes))
names(colores_1_15) <- niveles_presentes

# Construir mapa
centrales_mapa <- tm_shape(centrales) +
  tm_polygons("DISTRITO_FACTOR",
              palette = colores_1_15,
              title = "Distrito",
              border.col = "white",
              textNA = "") +
  tm_text("CODDISTBAR", size = 0.8, col = "black", shadow = TRUE) +
  tm_layout(
    title = "Distritos centrales de València",
    title.position = c("center", "bottom"),
    title.size = 1.2,
    legend.outside = TRUE,
    legend.bg.color = "white",
    legend.bg.alpha = 1
  )

# Mostrar mapa
centrales_mapa

############################################################
# 6. PARTIDO GANADOR POR BARRIO (MUNICIPAL, AUTONÓMICO, GENERAL)
############################################################
# Ajuste de 85 → 88 barrios (duplicando el 74 y el 85) para poder trabajar con el mapa
ajusta <- function(raw) {
  out <- numeric(nrow(barrios_sf))   # 87
  out[1:74] <- raw[1:74]
  out[75] <- raw[74]
  out[76] <- raw[74]
  out[77:87] <- raw[75:85]
  out[88] <- raw[85]
  out
}

# Paleta de colores para partidos
colores_partidos <- c(
  "PP" = "#1F77B4", "PSOE" = "#D62728", "VOX" = "#2CA02C",
  "Cs" = "#FFA500", "COMPROMIS" = "#FF8C00", "Compromís Municipal" = "#FF8C00","COMPROMÍS" = "#FF8C00",
  "SUMAR" = "#FF69B4", "PODEM-EUPV" = "#800080", "UP-EUPV" = "#800080",
  "Compromis/Sumar/Podemos" = "#FF69B4", "Otros" = "#A9A9A9"
)

# Función para limpiar nombres de partidos
limpiar_nombre_partido <- function(x) {
  x <- gsub("^Muni_|^Auto_|^Gene_|^Gen_", "", x)
  toupper(x)
}

# Calcular el partido más votado por barrio para cada elección
barrios_sf$ganador_muni <- limpiar_nombre_partido(ajusta(names(muni_ideo)[max.col(muni_ideo)]))
barrios_sf$ganador_auto <- limpiar_nombre_partido(ajusta(names(auto_ideo)[max.col(auto_ideo)]))
barrios_sf$ganador_gene <- limpiar_nombre_partido(ajusta(names(gene_ideo)[max.col(gene_ideo)]))

############################################################
# 6.1. VISUALIZACIÓN: MAPA DEL PARTIDO MÁS VOTADO
############################################################


# Función para graficar mapa de partido más votado
graficar_ganador <- function(var) {
  ggplot(barrios_sf) +
    geom_sf(aes(fill = .data[[var]]), color = "black", lwd = 0.1) +
    scale_fill_manual(values = colores_partidos, name = "Partido ganador") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}

# Generar mapas por elección
mapa_muni <- graficar_ganador("ganador_muni")
mapa_auto <- graficar_ganador("ganador_auto") + theme(legend.position = "none")
mapa_gene <- graficar_ganador("ganador_gene") + theme(legend.position = "none")

# Función para obtener el nombre del segundo mayor valor por fila
segundo_max_col <- function(df) {
  apply(df, 1, function(x) {
    orden <- order(x, decreasing = TRUE)
    names(df)[orden[2]]
  })
}
############################################################
# 7. VISUALIZACIÓN: MAPA DEL SEGUNDO PARTIDO MÁS VOTADO
############################################################
# Función auxiliar para obtener el segundo partido más votado
segundo_max_col <- function(df) {
  apply(df, 1, function(x) {
    orden <- order(x, decreasing = TRUE)
    names(df)[orden[2]]
  })
}

# Cálculo para cada tipo de elección
barrios_sf$segundo_muni <- limpiar_nombre_partido(ajusta(segundo_max_col(muni_ideo)))
barrios_sf$segundo_auto <- limpiar_nombre_partido(ajusta(segundo_max_col(auto_ideo)))
barrios_sf$segundo_gene <- limpiar_nombre_partido(ajusta(segundo_max_col(gene_ideo)))

# Función de graficado reutilizable
graficar_segundo <- function(var) {
  ggplot(barrios_sf) +
    geom_sf(aes(fill = .data[[var]]), color = "black", lwd = 0.1) +
    scale_fill_manual(values = colores_partidos, name = "2º partido") +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
    )
}
# Crear los tres mapas
mapa_segundo_muni <- graficar_segundo("segundo_muni")
mapa_segundo_auto <- graficar_segundo("segundo_auto") + theme(legend.position = "none")
mapa_segundo_gene <- graficar_segundo("segundo_gene") + theme(legend.position = "none")

# Cargar patchwork para combinar
library(patchwork)

mapa_segundo_final <- (mapa_segundo_muni + mapa_segundo_auto + mapa_segundo_gene) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Segundo partido más votado por barrio en cada elección",
    subtitle = "Municipales (izq), Autonómicas (centro), Generales (dcha)",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right"
    )
  )

# Mostrar figura
print(mapa_segundo_final)


############################################################
# 8. COMBINACIÓN Y EXPORTACIÓN DE FIGURA FINAL
############################################################

library(patchwork)

# Combinar los tres mapas en una única figura
final_plot <- (mapa_muni + mapa_auto + mapa_gene) +
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(
    title = "Partido ganador por barrio en cada elección",
    subtitle = "Municipales (izq), Autonómicas (centro), Generales (dcha)",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right"
    )
  )

# Mostrar figura final
print(final_plot)

############################################################
# 9. ANÁLISIS DE CORRESPONDENCIAS - ELECCIONES MUNICIPALES
############################################################

# 9.1 Realización del análisis de correspondencias (CA)
# Se aplica sobre la matriz de votos por partido en las municipales
res.ca.muni <- CA(muni_ideo, graph = TRUE)
summary(res.ca.muni)

# 9.2 Visualización de la varianza explicada por cada dimensión
# Permite identificar cuántas dimensiones son relevantes
fviz_screeplot(res.ca.muni, addlabels = TRUE, labelsize = 5) +
  labs(title = "Varianza explicada por dimensión (Municipales)",
       x = "Dimensión", y = "Porcentaje de inercia explicada") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# 9.3 Análisis de los barrios en el espacio factorial
# Coordenadas de los barrios (valores en cada dimensión)
head(res.ca.muni$row$coord)

# Contribución relativa de cada barrio a cada dimensión
head(res.ca.muni$row$contrib)

# Calidad de la representación (cos²): cerca de 1 = bien representado
head(res.ca.muni$row$cos2)

# Visualización gráfica de los barrios en el plano principal
fviz_ca_row(res.ca.muni, repel = TRUE) +
  labs(title = "Barrios en el espacio de correspondencia (Municipales)")

# 9.4 Análisis de los partidos en el espacio factorial
# Coordenadas de los partidos en cada dimensión
head(res.ca.muni$col$coord)

# Contribución de cada partido a la Dim 1 (eje ideológico)
head(res.ca.muni$col$contrib)

# Calidad de representación de los partidos (cos²)
head(res.ca.muni$col$cos2)

# Visualización gráfica de los partidos
fviz_ca_col(res.ca.muni, repel = TRUE) +
  labs(title = "Partidos municipales en el espacio de correspondencia")
# 9.5 Biplot conjunto: partidos y barrios en el mismo plano
# Útil para interpretar afinidades ideológicas y patrones territoriales
fviz_ca_biplot(res.ca.muni, repel = TRUE) +
  labs(title = "Biplot: Partidos y barrios municipales")
# 9.6 Extracción de la primera dimensión (eje ideológico) para regresiones posteriores
coord_muni_barrio <- res.ca.muni$row$coord[, 1]

############################################################
# 10. MAPA DE LA VARIABLE IDEOL_MUNI (AC MUNICIPAL)
############################################################


# 10.1. Construcción de la variable ideol_muni
# Añadir ideol_muni a dt
dt <- dt %>%
  mutate(ideol_muni = coord_muni_barrio)

# Función para ajustar de 85 a 88 filas (por agregación de barrios)
ajusta <- function(raw) {
  out <- numeric(nrow(barrios_sf))   # n = 88
  out[1:74]   <- raw[1:74]
  out[75]     <- raw[74]
  out[76]     <- raw[74]
  out[77:87]  <- raw[75:85]
  out[88]     <- raw[85]
  out
}

# Aplicar el ajuste y añadir columna a barrios_sf
barrios_sf$ideol_muni <- ajusta(dt$ideol_muni)

# 10.2. Generación del mapa con tmap

library(classInt)      # para cortes por cuantiles
library(RColorBrewer)  # para paletas
library(tmap)

# Crear cortes por cuantiles e incluir el valor 0 como límite
valores <- barrios_sf$ideol_muni
cuantiles <- quantile(valores, probs = seq(0, 1, length.out = 7), na.rm = TRUE)
if (!0 %in% cuantiles) {
  cuantiles <- sort(unique(c(cuantiles, 0)))
}

# Crear paleta con tonos azules para negativos y rojos para positivos
n_clases <- length(cuantiles) - 1
mitad <- floor(n_clases / 2)
colores <- c(
  rev(brewer.pal(min(9, mitad + 1), "Blues")[1:mitad]),
  brewer.pal(min(9, n_clases - mitad), "Reds")[1:(n_clases - mitad)]
)

# Mapa final
mapa_ideol <- tm_shape(barrios_sf) +
  tm_polygons("ideol_muni",
              breaks = cuantiles,
              palette = colores,
              title = "Ideol_muni (Dim 1)",textNA = "")  +
  tm_layout(
    legend.outside = TRUE,
    legend.title.size = 1.1,
    legend.text.size = 0.8,
    frame = FALSE
  )

# Mostrar el mapa
tmap_mode("plot")
mapa_ideol

############################################################
# 11. ANÁLISIS DE CORRESPONDENCIAS - ELECCIONES AUTONÓMICAS
############################################################

# 11.1 Realización del análisis de correspondencias (CA)
# Aplicamos CA sobre la matriz de votos por partido en las autonómicas
res.ca.auto <- CA(auto_ideo, graph = TRUE)
summary(res.ca.auto)

# 11.2 Visualización de la varianza explicada por cada dimensión
fviz_screeplot(res.ca.auto, addlabels = TRUE, labelsize = 5) +
  labs(title = "Varianza explicada por dimensión (Autonómicas)",
       x = "Dimensión", y = "Porcentaje de inercia explicada") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )


# 11.3 Análisis de barrios en el espacio factorial
fviz_ca_row(res.ca.auto, repel = TRUE) +
  labs(title = "Barrios en el espacio de correspondencia (Autonómicas)")
# Coordenadas de los barrios (valores en cada dimensión)
head(res.ca.auto$row$coord)

# Contribución relativa de cada barrio a cada dimensión
head(res.ca.auto$row$contrib)

# Calidad de la representación (cos²): cerca de 1 = bien representado
head(res.ca.auto$row$cos2)

# Coordenadas de los partidos en cada dimensión
head(res.ca.auto$col$coord)

# Contribución de cada partido a la Dim 1 (eje ideológico)
head(res.ca.auto$col$contrib)

# Calidad de representación de los partidos (cos²)
head(res.ca.muni$col$cos2)
# 11.4 Análisis de partidos en el espacio factorial
fviz_ca_col(res.ca.auto, repel = TRUE) +
  labs(title = "Partidos autonómicos en el espacio de correspondencia")

# 11.5 Biplot conjunto
fviz_ca_biplot(res.ca.auto, repel = TRUE) +
  labs(title = "Biplot: Partidos y barrios autonómicos")


fviz_ca_col(res.ca.auto, 
            repel = TRUE, 
            labelsize = 6,         # tamaño de texto de las etiquetas
            title = "Partidos autonómicos en el espacio de correspondencias",
            axes.linetype = "solid", 
            ggtheme = theme_minimal()) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

fviz_ca_row(res.ca.auto, 
            repel = TRUE, 
            labelsize = 5.5,         # puedes ajustar a gusto
            title = "Barrios en el espacio de correspondencias (Autonómicas)",
            axes.linetype = "solid",
            ggtheme = theme_minimal()) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )



# 11.6 Extracción de la Dim.1 como eje ideológico
coord_auto_barrio <- res.ca.auto$row$coord[, 1]

############################################################
# 12. MAPA DE LA VARIABLE IDEOL_AUTO (AC AUTONÓMICAS)
############################################################

# 12.1 Añadir variable ideológica al dataframe
dt <- dt %>%
  mutate(ideol_auto = coord_auto_barrio)

# 12.2 Ajustar a 88 barrios y añadir a barrios_sf
barrios_sf$ideol_auto <- ajusta(dt$ideol_auto)

# 12.3 Generación del mapa

# Cortes por cuantiles (incluyendo 0)
valores <- barrios_sf$ideol_auto
cuantiles <- quantile(valores, probs = seq(0, 1, length.out = 7), na.rm = TRUE)
if (!0 %in% cuantiles) {
  cuantiles <- sort(unique(c(cuantiles, 0)))
}

# Crear paleta con tonos azules y rojos
n_clases <- length(cuantiles) - 1
mitad <- floor(n_clases / 2)
colores <- c(
  rev(brewer.pal(min(9, mitad + 1), "Blues")[1:mitad]),
  brewer.pal(min(9, n_clases - mitad), "Reds")[1:(n_clases - mitad)]
)

# Mapa final
mapa_ideol_auto <- tm_shape(barrios_sf) +
  tm_polygons("ideol_auto",
              breaks = cuantiles,
              palette = colores,
              title = "Ideol_Auto (Dim 1)", textNA = "") +
  tm_layout(
    legend.outside = TRUE,
    legend.title.size = 1.1,
    legend.text.size = 0.8,
    frame = FALSE
  )

# Mostrar mapa
tmap_mode("plot")
mapa_ideol_auto

############################################################
# 13. ANÁLISIS DE CORRESPONDENCIAS - ELECCIONES GENERALES
############################################################

# 13.1 Realización del análisis de correspondencias (CA)
# Aplicamos CA sobre la matriz de votos por partido en las generales
res.ca.gene <- CA(gene_ideo, graph = TRUE)
summary(res.ca.gene)

# 13.2 Visualización de la varianza explicada por cada dimensión
fviz_screeplot(res.ca.gene, addlabels = TRUE, labelsize = 5) +
  labs(title = "Varianza explicada por dimensión (Generales)",
       x = "Dimensión", y = "Porcentaje de inercia explicada") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# 13.3 Análisis de barrios en el espacio factorial
fviz_ca_row(res.ca.gene, repel = TRUE) +
  labs(title = "Barrios en el espacio de correspondencia (Generales)")

# Coordenadas de los barrios (valores en cada dimensión)
head(res.ca.gene$row$coord)

# Contribución relativa de cada barrio a cada dimensión
head(res.ca.gene$row$contrib)

# Calidad de la representación (cos²)
head(res.ca.gene$row$cos2)
# Coordenadas de los partidos en cada dimensión
head(res.ca.gene$col$coord)

# Contribución de cada partido a la Dim 1 (eje ideológico)
head(res.ca.gene$col$contrib)
# 13.4 Análisis de partidos en el espacio factorial
fviz_ca_col(res.ca.gene, repel = TRUE) +
  labs(title = "Partidos generales en el espacio de correspondencia")

# 13.5 Biplot conjunto
fviz_ca_biplot(res.ca.gene, repel = TRUE) +
  labs(title = "Biplot: Partidos y barrios generales")

# 13.6 Extracción de la Dim.1 como eje ideológico
coord_gene_barrio <- res.ca.gene$row$coord[, 1]



############################################################
# 14. MAPA DE LA VARIABLE IDEOL_GENE (AC GENERALES)
############################################################

# 14.1 Añadir variable ideológica al dataframe
dt <- dt %>%
  mutate(ideol_gene = coord_gene_barrio)

# 14.2 Ajustar a 88 barrios y añadir a barrios_sf
barrios_sf$ideol_gene <- ajusta(dt$ideol_gene)

# 14.3 Generación del mapa

# Cortes por cuantiles (incluyendo 0)
valores <- barrios_sf$ideol_gene
cuantiles <- quantile(valores, probs = seq(0, 1, length.out = 7), na.rm = TRUE)
if (!0 %in% cuantiles) {
  cuantiles <- sort(unique(c(cuantiles, 0)))
}

# Crear paleta con tonos azules y rojos
n_clases <- length(cuantiles) - 1
mitad <- floor(n_clases / 2)
colores <- c(
  rev(brewer.pal(min(9, mitad + 1), "Blues")[1:mitad]),
  brewer.pal(min(9, n_clases - mitad), "Reds")[1:(n_clases - mitad)]
)

# Mapa final
mapa_ideol_gene <- tm_shape(barrios_sf) +
  tm_polygons("ideol_gene",
              breaks = cuantiles,
              palette = colores,
              title = "Ideol_Gene (Dim 1)", textNA = "") +
  tm_layout(
    legend.outside = TRUE,
    legend.title.size = 1.1,
    legend.text.size = 0.8,
    frame = FALSE
  )

# Mostrar mapa
tmap_mode("plot")
mapa_ideol_gene

############################################################
# 15. EXPLORACIÓN DE VARIABLES SOCIOECONÓMICAS
############################################################

# 15.1 Selección de variables socioeconómicas puras
socio_vars <- dt %>%
  select(-starts_with("Muni_"),
         -starts_with("Auto_"),
         -starts_with("Gene_"),
         -ideol_muni, -ideol_gene, -ideol_auto)

# 15.2 Matriz de correlaciones y visualización
library(corrplot)

cor_matrix <- cor(socio_vars, use = "complete.obs")
cor_matrix
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addrect = 4,
         tl.cex = 0.8,
         tl.srt = 60,
         tl.col = "black",
         diag = FALSE,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         addCoef.col = "black",     # Mostrar los números
         number.cex = 0.6)          # Tamaño de los números

#Matriz con las parejas de mayor correlación

# 2. Convertirla en un data frame largo para filtrar
library(tidyverse)

cor_df <- as.data.frame(as.table(cor_matrix)) %>%
  filter(Var1 != Var2) %>%                         # quitar la diagonal
  mutate(pair = pmap_chr(list(Var1, Var2), ~ paste(sort(c(...)), collapse = " - "))) %>% 
  distinct(pair, .keep_all = TRUE) %>%             # eliminar duplicados (pares simétricos)
  filter(abs(Freq) > 0.8) %>%                      # filtrar por correlación > 0.8
  arrange(desc(abs(Freq)))                         # ordenar por valor absoluto

# 3. Mostrar el resultado
print(cor_df)


# 15.3 Análisis de colinealidad: VIF (Top 6 variables con mayor VIF)
library(car)
library(ggplot2)

modelo_aux <- lm(rep(1, nrow(socio_vars)) ~ ., data = socio_vars)
vif_vals <- vif(modelo_aux)
vif_vals
vif_df <- data.frame(Variable = names(vif_vals), VIF = as.numeric(vif_vals))

# Filtrar las 6 variables con mayor VIF
vif_top6 <- vif_df %>%
  arrange(desc(VIF)) %>%
  slice(1:6)

# Gráfico
ggplot(vif_top6, aes(x = reorder(Variable, VIF), y = VIF)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Top 6 variables con mayor VIF",
       x = "Variable", y = "VIF") +
  theme_minimal(base_size = 14)



############################################################
# 16. TESTS DE APLICABILIDAD DEL AFE
############################################################

library(psych)
library(MVN)

# 16.1 Índice KMO por variable
kmo_all <- KMO(socio_vars)
kmo_all
kmo_vals <- kmo_all$MSAi
kmo_df <- data.frame(Variable = names(kmo_vals), KMO = as.numeric(kmo_vals))

ggplot(kmo_df, aes(x = reorder(Variable, KMO), y = KMO)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0.7, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Índice KMO por variable (todas las variables)",
       x = "Variable",
       y = "KMO") +
  theme_minimal(base_size = 14)

# 16.2 Test de esfericidad de Bartlett
cortest.bartlett(cor(socio_vars), n = nrow(socio_vars))

# 16.3 Test de Mardia para normalidad multivariante
mardia_socio <- mardia(socio_vars)
print(mardia_socio)
# 17.1. Carga de librerías necesarias
library(psych)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tibble)
library(ggrepel)
library(gridExtra)

# 17.2. AFE con ejes principales y rotación Varimax
res.fa <- fa(socio_vars, nfactors = 3, rotate = "varimax", fm = "pa")
res.fa

# 17.3. Porcentaje de varianza explicada por cada factor
var_exp <- res.fa$Vaccounted["Proportion Var", ]
var_df <- data.frame(
  Factor = paste0("Factor ", 1:length(var_exp)),
  Porcentaje = round(100 * as.numeric(var_exp), 1)
)

ggplot(var_df, aes(x = Factor, y = Porcentaje)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -0.5, size = 6) +
  ylim(0, max(var_df$Porcentaje) + 10) +
  labs(title = "Porcentaje de varianza explicada por factor",
       x = "", y = "Varianza (%)") +
  theme_minimal(base_size = 16)

##########################################################################################
#QUITAMOS LAS VARIABLES QUE GENERAN PROBLEMAS Y REPETIMOS LOS PASOS ANTERIORES
##########################################################################################


socio_vars <- dt %>%
  select(-starts_with("Muni_"),
         -starts_with("Auto_"),
         -starts_with("Gene_"),
         -ideol_muni, -ideol_gene, -ideol_auto, -Porc_may65, 
         -Porc_fueraCV, -ind_demografico, -Ind_equipamiento, 
         -Porc_ingresos_ud_consumo_debajo_60, -Porc_nacida_extranj,
         -Rentamedia_ud_consumo)

cor_matrix <- cor(socio_vars, use = "complete.obs")
cor_matrix
corrplot(cor_matrix,
         method = "color",
         type = "upper",
         order = "hclust",
         addrect = 4,
         tl.cex = 0.8,
         tl.srt = 60,
         tl.col = "black",
         diag = FALSE,
         col = colorRampPalette(c("blue", "white", "red"))(200))

# 15.3 Análisis de colinealidad: VIF (Top 6 variables con mayor VIF)
library(car)
library(ggplot2)

modelo_aux <- lm(rep(1, nrow(socio_vars)) ~ ., data = socio_vars)
vif_vals <- vif(modelo_aux)
vif_vals
vif_df <- data.frame(Variable = names(vif_vals), VIF = as.numeric(vif_vals))

# Filtrar las 6 variables con mayor VIF
vif_top6 <- vif_df %>%
  arrange(desc(VIF)) %>%
  slice(1:6)

# Gráfico
ggplot(vif_top6, aes(x = reorder(Variable, VIF), y = VIF)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Top 6 variables con mayor VIF",
       x = "Variable", y = "VIF") +
  theme_minimal(base_size = 14)

# 16.1 Índice KMO por variable
kmo_all <- KMO(socio_vars)
kmo_all
kmo_vals <- kmo_all$MSAi
kmo_df <- data.frame(Variable = names(kmo_vals), KMO = as.numeric(kmo_vals))

ggplot(kmo_df, aes(x = reorder(Variable, KMO), y = KMO)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0.7, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Índice KMO por variable",
       x = "Variable",
       y = "KMO") +
  theme_minimal(base_size = 14)

# 16.2 Test de esfericidad de Bartlett
cortest.bartlett(cor(socio_vars), n = nrow(socio_vars))

# 16.3 Test de Mardia para normalidad multivariante
mardia_socio <- mardia(socio_vars)
print(mardia_socio)

############################################################
# 17. ANÁLISIS FACTORIAL EXPLORATORIO (AFE)
############################################################

# 17.1. Carga de librerías necesarias
library(psych)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tibble)
library(ggrepel)
library(gridExtra)

# 17.2. AFE con ejes principales y rotación Varimax
res.fa <- fa(socio_vars, nfactors = 3, rotate = "varimax", fm = "pa")
res.fa


# 17.3. Porcentaje de varianza explicada por cada factor
var_exp <- res.fa$Vaccounted["Proportion Var", ]
var_df <- data.frame(
  Factor = paste0("Factor ", 1:length(var_exp)),
  Porcentaje = round(100 * as.numeric(var_exp), 1)
)

ggplot(var_df, aes(x = Factor, y = Porcentaje)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = paste0(Porcentaje, "%")), vjust = -0.5, size = 6) +
  ylim(0, max(var_df$Porcentaje) + 10) +
  labs(title = "Porcentaje de varianza explicada por factor",
       x = "", y = "Varianza (%)") +
  theme_minimal(base_size = 16)

# 17.4. Tabla de cargas factoriales
loadings_df <- as.data.frame(res.fa$loadings[1:ncol(socio_vars), ])
colnames(loadings_df) <- paste0("F", 1:3)  # <- Renombrar columnas
loadings_df <- tibble::rownames_to_column(loadings_df, var = "Variable")
loadings_df_redondeado <- loadings_df
loadings_df_redondeado[ , -1] <- round(loadings_df_redondeado[ , -1], 2)

# 17.5. Diagrama general de factores (opcional)
fa.diagram(res.fa)

# 17.6. Gráfico de cargas por variable y factor
loadings_long <- loadings_df %>%
  pivot_longer(-Variable, names_to = "Factor", values_to = "Carga")

ggplot(loadings_long, aes(x = reorder(Variable, Carga), y = Carga, fill = Factor)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Cargas factoriales por variable", y = "Carga", x = "") +
  theme_minimal(base_size = 14)

# 17.7. Función para graficar cargas individuales por factor con líneas guía
graficar_cargas_con_lineas <- function(factor_col, titulo) {
  loadings_df %>%
    select(Variable, all_of(factor_col)) %>%
    arrange(.data[[factor_col]]) %>%
    mutate(Variable = factor(Variable, levels = Variable)) %>%
    ggplot(aes(x = Variable, y = .data[[factor_col]])) +
    geom_col(fill = "steelblue") +
    geom_hline(yintercept = 0, color = "black") +
    geom_hline(yintercept = c(0.6, -0.6), color = "darkorange", linetype = "longdash", linewidth = 0.5) +
    geom_hline(yintercept = c(0.9, -0.9), color = "red3", linetype = "dashed", linewidth = 0.5) +
    coord_flip() +
    labs(title = titulo,
         x = NULL, y = "Carga factorial") +
    theme_minimal(base_size = 14)
}

# 17.8. Gráficos por factor
grafico_F1 <- graficar_cargas_con_lineas("F1", "Cargas factoriales – Factor 1 (F1)")
grafico_F2 <- graficar_cargas_con_lineas("F2", "Cargas factoriales – Factor 2 (F2)")
grafico_F3 <- graficar_cargas_con_lineas("F3", "Cargas factoriales – Factor 3 (F3)")
#grafico_PA4 <- graficar_cargas_con_lineas("PA4", "Cargas factoriales – Factor 4 (PA4)")
#grafico_PA5 <- graficar_cargas_con_lineas("PA5", "Cargas factoriales – Factor 5 (PA5)")

print(grafico_F1)
print(grafico_F2)
print(grafico_F3)
#print(grafico_PA4)
#print(grafico_PA5)

# 17.9. Preparar dataframe para planos factoriales
fact_plane <- loadings_df %>%
  select(Variable, F1, F2, F3) %>%
  rename(Factor1 = F1, Factor2 = F2, Factor3 = F3)

# 17.10. Función para planos factoriales 2D con líneas guía
graficar_plano <- function(eje_x, eje_y, titulo) {
  ggplot(fact_plane, aes_string(x = eje_x, y = eje_y)) +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.4) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.4) +
    geom_hline(yintercept = c(0.6, -0.6), linetype = "longdash", color = "darkorange", linewidth = 0.4) +
    geom_vline(xintercept = c(0.6, -0.6), linetype = "longdash", color = "darkorange", linewidth = 0.4) +
    geom_hline(yintercept = c(0.9, -0.9), linetype = "dashed", color = "red3", linewidth = 0.4) +
    geom_vline(xintercept = c(0.9, -0.9), linetype = "dashed", color = "red3", linewidth = 0.4) +
    geom_point(aes_string(color = eje_x), size = 4, alpha = 0.9) +
    geom_text_repel(aes(label = Variable), size = 5) +
    scale_color_gradient2(low = "red", mid = "purple", high = "blue", midpoint = 0) +
    labs(x = eje_x, y = eje_y, title = titulo) +
    theme_minimal(base_size = 14)
}

# 17.11. Crear y visualizar planos factoriales
p12 <- graficar_plano("Factor1", "Factor2", "Plano factorial: Factor 1 vs Factor 2")
p13 <- graficar_plano("Factor1", "Factor3", "Plano factorial: Factor 1 vs Factor 3")
p23 <- graficar_plano("Factor2", "Factor3", "Plano factorial: Factor 2 vs Factor 3")

print(p12)
print(p13)
print(p23)

############################################################
# 18. REPRESENTACIÓN DE LAS PUNTUACIONES FACTORIALES
############################################################


# ——————————————————————————————————————————————————————————————
# 0) Cargamos paquetes
library(dplyr)
library(ggplot2)
library(sf)
library(classInt)
library(viridis)
library(patchwork)
library(tibble)
library(psych)
library(tidyr)
library(ggrepel)

# ——————————————————————————————————————————————————————————————
# 1) Cálculo de puntuaciones factoriales
res.fa <- fa(socio_vars, nfactors = 3, rotate = "varimax", fm = "pa")
scores <- as.data.frame(res.fa$scores)
colnames(scores) <- paste0("F", 1:3)


#Por si quiero
#colnames(scores) <- paste0("F", 1:3)
#escalar_minmax <- function(x) {
#  2 * ((x - min(x)) / (max(x) - min(x))) - 1
#}

# Aplicar a las puntuaciones factoriales
#scores_scaled <- as.data.frame(
 # lapply(as.data.frame(res.fa$scores), escalar_minmax)
#)
#colnames(scores_scaled) <- paste0("F", 1:3)

# ——————————————————————————————————————————————————————————————
# 2) Incorporamos ideologías y creamos dataframe base
ideos_raw <- data.frame(
  ideol_muni = dt$ideol_muni,
  ideol_auto = dt$ideol_auto,
  ideol_gene = dt$ideol_gene
)

df_85 <- bind_cols(scores, ideos_raw)

# ——————————————————————————————————————————————————————————————
# 3) Ajuste de 85 → 88 barrios (duplicando el 74 y el 85)
ajusta <- function(raw) {
  out <- numeric(nrow(barrios_sf))   # 87
  out[1:74] <- raw[1:74]
  out[75] <- raw[74]
  out[76] <- raw[74]
  out[77:87] <- raw[75:85]
  out[88] <- raw[85]
  out
}

vars_a_ajustar <- c(paste0("F", 1:3), names(ideos_raw))

barrios_sf <- barrios_sf %>%
  mutate(
    !!! setNames(
      lapply(vars_a_ajustar, function(v) ajusta(df_85[[v]])),
      vars_a_ajustar
    )
  ) %>%
  st_make_valid()

# ——————————————————————————————————————————————————————————————
# 4) Crear mapas para cada F
mapa_F <- function(tc, nombre_factor) {
  ci <- classIntervals(barrios_sf[[tc]], n = 5, style = "quantile")
  brks <- ci$brks
  labs <- paste0(round(brks[-length(brks)], 2), " – ", round(brks[-1], 2))
  clase <- paste0(tc, "_cls")
  
  barrios_sf <- barrios_sf %>%
    mutate(!!clase := cut(barrios_sf[[tc]], breaks = brks, include.lowest = TRUE, labels = labs))
  
  centrales <- barrios_sf %>% 
    mutate(lat = st_coordinates(st_centroid(geometry))[,2]) %>% 
    filter(lat >= 39.43, lat <= 39.50)
  
  bb <- st_bbox(centrales)
  
  p1 <- ggplot(barrios_sf) +
    geom_sf(aes_string(fill = clase), colour = "white", size = 0.2) +
    scale_fill_viridis_d(
      name = nombre_factor,
      option = "turbo",
      direction = -1
    ) +  guides(fill = guide_legend(
      title = NULL,
      nrow = 2,
      byrow = TRUE,
      label.position = "bottom"
    )) +
    labs(title = paste(nombre_factor)) +
    theme_minimal() +
    theme(legend.position = "bottom",axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
  
  p2 <- ggplot(barrios_sf) +
    geom_sf(aes_string(fill = clase), colour = "white", size = 0.2) +
    scale_fill_viridis_d(
      name = nombre_factor,
      option = "turbo",
      direction = -1,
      drop = FALSE
    ) + guides(fill = guide_legend(
      title = NULL,
      nrow = 2,
      byrow = TRUE,
      label.position = "bottom"
    )) +
    coord_sf(
      xlim = c(bb["xmin"], bb["xmax"]),
      ylim = c(bb["ymin"], bb["ymax"]),
      expand = FALSE
    ) +
    labs(title = "Zoom centro") +
    theme_minimal() +
    theme(legend.position = "bottom",axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
  
  return(list(p1 = p1, p2 = p2))
}

# Para generar los tres mapas de los factores con nombres descriptivos:
mapa_F("F1", "Capital socioeconómico positivo")
mapa_F("F2", "Envejecimiento y dependencia")
mapa_F("F3", "Diversidad de origen geográfico")


############################################################
# 19. MODELOS DE REGRESIÓN LINEAL MÚLTIPLE
############################################################

# 19.1 Modelo OLS – Elecciones municipales
fit_muni <- lm(ideol_muni ~ F1 + F2 + F3, data = barrios_sf)
summary(fit_muni)

# 19.2 Modelo OLS – Elecciones autonómicas
fit_auto <- lm(ideol_auto ~ F1 + F2 + F3, data = barrios_sf)
summary(fit_auto)

# 19.3 Modelo OLS – Elecciones generales
fit_gene <- lm(ideol_gene ~ F1 + F2 + F3, data = barrios_sf)
summary(fit_gene)


# 19.4 Guardamos residuos en el objeto espacial para posterior análisis
barrios_sf$res_muni <- resid(fit_muni)
barrios_sf$res_auto <- resid(fit_auto)
barrios_sf$res_gene <- resid(fit_gene)

############################################################
# 20. ANÁLISIS DE BONDAD DE AJUSTE Y SUPUESTOS
############################################################

library(lmtest)
library(car)
library(ggplot2)

# Lista de modelos
modelos <- list(
  muni = fit_muni,
  auto = fit_auto,
  gene = fit_gene
)

# 20.1 Función para evaluar cada modelo
library(ggplot2)
library(gridExtra)
library(lmtest)
evaluar_modelo <- function(modelo, nombre = "modelo") {
  cat(paste0("\nEvaluación del modelo: ", nombre, "\n"))
  
  # R-cuadrado
  resumen <- summary(modelo)
  cat("R² ajustado: ", round(resumen$adj.r.squared, 3), "\n")
  
  # Normalidad de residuos
  cat("Shapiro-Wilk p-value: ", shapiro.test(resid(modelo))$p.value, "\n")
  
  # Homocedasticidad (Breusch–Pagan)
  cat("Breusch–Pagan p-value: ", bptest(modelo)$p.value, "\n")
}

# Función para evaluar cada modelo con ggplot2
graficar_diagnostico <- function(modelo, nombre = "Modelo") {
  residuos <- resid(modelo)
  ajustados <- fitted(modelo)
  df <- data.frame(ajustados, residuos)
  
  # Q-Q Plot
  p1 <- ggplot(df, aes(sample = residuos)) +
    stat_qq() + stat_qq_line(col = "red") +
    labs(title = paste("Q-Q Plot de residuos –", nombre)) +
    theme_minimal(base_size = 14)
  
  # Residuos vs Ajustados
  p2 <- ggplot(df, aes(x = ajustados, y = residuos)) +
    geom_point(color = "steelblue", size = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = paste("Residuos vs Ajustados –", nombre),
      x = "Valores ajustados", y = "Residuos"
    ) +
    theme_minimal(base_size = 14)
  
  grid.arrange(p1, p2, ncol = 2)
}

# Llamada para los tres modelos

evaluar_modelo(fit_muni, "Municipales")
evaluar_modelo(fit_auto, "Autonómicas")
evaluar_modelo(fit_gene, "Generales")

graficar_diagnostico(fit_muni, "Municipales")
graficar_diagnostico(fit_auto, "Autonómicas")
graficar_diagnostico(fit_gene, "Generales")

############################################################
# 21. TEST DE MORAN I SOBRE RESIDUOS OLS
############################################################

# 21.1 Librerías necesarias
library(spdep)
library(sf)

# 21.2 Crear matriz de vecinos y lista de pesos espaciales (reina)
nb <- poly2nb(barrios_sf, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# 21.3 Calcular residuos de los tres modelos OLS
barrios_sf$res_muni <- resid(fit_muni)
barrios_sf$res_auto <- resid(fit_auto)
barrios_sf$res_gene <- resid(fit_gene)

# 21.4 Test de Moran I para cada conjunto de residuos
print(moran.test(barrios_sf$res_muni, lw, zero.policy = TRUE))

print(moran.test(barrios_sf$res_auto, lw, zero.policy = TRUE))

print(moran.test(barrios_sf$res_gene, lw, zero.policy = TRUE))

############################################################
# 22. MODELO SAR – Usando F1, F2, F3 como predictores
############################################################

# ——————————————————————————————————————————————————————————————
# 22.1. Preparación de variables
library(spdep)
library(spatialreg)
library(sf)

vars <- c("F1", "F2", "F3", "ideol_muni", "ideol_auto", "ideol_gene")
for (v in vars) {
  barrios_sf[[v]] <- ajusta(df_85[[v]])
}

nb <- poly2nb(barrios_sf, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ——————————————————————————————————————————————————————————————
# 22.2. Ajuste de modelos SAR por elección
sar_muni <- lagsarlm(ideol_muni ~ F1 + F2 + F3, data = barrios_sf, listw = lw, zero.policy = TRUE)
sar_auto <- lagsarlm(ideol_auto ~ F1 + F2 + F3, data = barrios_sf, listw = lw, zero.policy = TRUE)
sar_gene <- lagsarlm(ideol_gene ~ F1 + F2 + F3, data = barrios_sf, listw = lw, zero.policy = TRUE)

summary(sar_muni)
summary(sar_auto)
summary(sar_gene)

# ——————————————————————————————————————————————————————————————
# 22.3. Test de Moran I sobre residuos SAR
moran.test(residuals(sar_muni), lw, zero.policy = TRUE)
moran.test(residuals(sar_auto), lw, zero.policy = TRUE)
moran.test(residuals(sar_gene), lw, zero.policy = TRUE)

# ——————————————————————————————————————————————————————————————
# 22.4. Representación de resultados – Por modelo

# 22.4.1. Valores predichos
barrios_sf$pred_muni <- fitted(sar_muni)
tm_shape(barrios_sf) +
  tm_fill("pred_muni", palette = "viridis", style = "quantile", title = "Ideología predicha", textNA = "") +
  tm_borders(col = "gray60") +
  tm_layout(legend.outside = TRUE)

# 22.4.2. Residuos
barrios_sf$res_muni <- residuals(sar_muni)
tm_shape(barrios_sf) +
  tm_fill("res_muni", palette = "-RdBu", style = "kmeans", title = "Residuos SAR",textNA = "") +
  tm_borders(col = "gray60") +
  tm_layout(legend.outside = TRUE)

# 22.4.1. Valores predichos
barrios_sf$pred_auto <- fitted(sar_auto)
tm_shape(barrios_sf) +
  tm_fill("pred_auto", palette = "viridis", style = "quantile", title = "Ideología predicha",textNA = "") +
  tm_borders(col = "gray60") +
  tm_layout(legend.outside = TRUE)

# 22.4.2. Residuos
barrios_sf$res_auto <- residuals(sar_auto)
tm_shape(barrios_sf) +
  tm_fill("res_auto", palette = "-RdBu", style = "kmeans", title = "Residuos SAR",textNA = "") +
  tm_borders(col = "gray60") +
  tm_layout(legend.outside = TRUE)

# 22.4.1. Valores predichos
barrios_sf$pred_gene <- fitted(sar_gene)
tm_shape(barrios_sf) +
  tm_fill("pred_gene", palette = "viridis", style = "quantile", title = "Ideología predicha",textNA = "") +
  tm_borders(col = "gray60") +
  tm_layout(legend.outside = TRUE)

# 22.4.2. Residuos
barrios_sf$res_gene <- residuals(sar_gene)
tm_shape(barrios_sf) +
  tm_fill("res_gene", palette = "-RdBu", style = "kmeans", title = "Residuos SAR",textNA = "") +
  tm_borders(col = "gray60") +
  tm_layout(legend.outside = TRUE)

# ——————————————————————————————————————————————————————————————


library(ggplot2)
library(ggrepel)
library(patchwork)
library(dplyr)

# 1. Crear columnas de residuo absoluto
barrios_sf <- barrios_sf %>%
  mutate(
    abs_resid_muni = abs(ideol_muni - pred_muni),
    abs_resid_auto = abs(ideol_auto - pred_auto),
    abs_resid_gene = abs(ideol_gene - pred_gene)
  )

# 2. Etiquetas solo para los 5 barrios con más error por tipo
barrios_sf <- barrios_sf %>%
  mutate(
    etiqueta_muni = if_else(rank(-abs_resid_muni) <=6, as.character(CODDISTBAR), NA),
    etiqueta_auto = if_else(rank(-abs_resid_auto) <= 6, as.character(CODDISTBAR), NA),
    etiqueta_gene = if_else(rank(-abs_resid_gene) <= 6, as.character(CODDISTBAR), NA)
  )

# Función para generar gráfico para un factor y elección concreta
graficar_factor <- function(df, factor, pred, etiqueta, color_punto, color_linea, titulo) {
  ggplot(df, aes_string(x = factor, y = pred, label = etiqueta)) +
    geom_point(color = color_punto, size = 2) +
    geom_text_repel(
      size = 5,
      fontface = "bold",
      color = "black",
      box.padding = 0.5,
      min.segment.length = 0,
      max.overlaps = Inf,
      na.rm = TRUE
    ) +
    geom_smooth(method = "lm", se = FALSE, color = color_linea) +
    labs(x = factor, y = "Ideología predicha (ŷ)", title = titulo) +
    theme_minimal(base_size = 14)
}

# F1
p_f1_muni <- graficar_factor(barrios_sf, "F1", "pred_muni", "etiqueta_muni", "darkblue", "red", "Municipales")
p_f1_auto <- graficar_factor(barrios_sf, "F1", "pred_auto", "etiqueta_auto", "darkblue", "red", "Autonómicas")
p_f1_gene <- graficar_factor(barrios_sf, "F1", "pred_gene", "etiqueta_gene", "darkblue", "red", "Generales")

(p_f1_muni | p_f1_auto | p_f1_gene) +
  plot_annotation(title = "Relación entre F1 y la ideología predicha por modelo SAR\n(Top 5 barrios con mayor residuo absoluto)")

# F2
p_f2_muni <- graficar_factor(barrios_sf, "F2", "pred_muni", "etiqueta_muni", "firebrick", "black", "Municipales")
p_f2_auto <- graficar_factor(barrios_sf, "F2", "pred_auto", "etiqueta_auto", "firebrick", "black", "Autonómicas")
p_f2_gene <- graficar_factor(barrios_sf, "F2", "pred_gene", "etiqueta_gene", "firebrick", "black", "Generales")

(p_f2_muni | p_f2_auto | p_f2_gene) +
  plot_annotation(title = "Relación entre F2 y la ideología predicha por modelo SAR\n(Top 5 barrios con mayor residuo absoluto)")

# Correlaciones entre factores y predicciones SAR
cor_f1 <- cor(barrios_sf$F1, barrios_sf$pred_muni)
cor_f2 <- cor(barrios_sf$F2, barrios_sf$pred_muni)
cor_f3 <- cor(barrios_sf$F3, barrios_sf$pred_muni)

cor_f1_auto <- cor(barrios_sf$F1, barrios_sf$pred_auto)
cor_f2_auto <- cor(barrios_sf$F2, barrios_sf$pred_auto)
cor_f3_auto <- cor(barrios_sf$F3, barrios_sf$pred_auto)

cor_f1_gene <- cor(barrios_sf$F1, barrios_sf$pred_gene)
cor_f2_gene <- cor(barrios_sf$F2, barrios_sf$pred_gene)
cor_f3_gene <- cor(barrios_sf$F3, barrios_sf$pred_gene)
# Mostrar resultados en tabla
data.frame(
  Factor = c("F1", "F2","F3"),
  Municipales = c(cor_f1, cor_f2,cor_f3),
  Autonómicas = c(cor_f1_auto, cor_f2_auto, cor_f3_auto),
  Generales = c(cor_f1_gene, cor_f2_gene, cor_f3_gene)
)


 # Función auxiliar para obtener df de coeficientes y ggplot por tipo de modelo SAR
  get_coef_df_plot <- function(modelo, titulo) {
    coefs_all <- summary(modelo)$Coef
    coefs_df <- as.data.frame(coefs_all[c("F1", "F2", "F3"), ])
    coefs_df$Factor <- rownames(coefs_df)
    rownames(coefs_df) <- NULL
    colnames(coefs_df)[1:2] <- c("Estimate", "StdError")
    
    coefs_df <- coefs_df %>%
      mutate(
        IC_low = Estimate - 1.96 * StdError,
        IC_high = Estimate + 1.96 * StdError,
        Factor = factor(Factor, levels = c("F1", "F2", "F3"))
      )
    
    # Tabla
    tabla <- coefs_df %>%
      mutate(
        `Coef.` = round(Estimate, 3),
        `IC 95%` = paste0("[", round(IC_low, 3), ", ", round(IC_high, 3), "]"),
        `p-valor` = signif(summary(modelo)$Coef[c("F1", "F2", "F3"), "Pr(>|z|)"], 3)
      ) %>%
      select(Factor, `Coef.`, `IC 95%`, `p-valor`)
    
    # Gráfico
    grafico <- ggplot(coefs_df, aes(x = Factor, y = Estimate)) +
      geom_point(size = 4, color = "steelblue") +
      geom_errorbar(aes(ymin = IC_low, ymax = IC_high), width = 0.15, color = "gray30") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      labs(
        title = titulo,
        y = "Coeficiente estimado", x = ""
      ) +
      theme_minimal(base_size = 16)
    
    return(list(tabla = tabla, grafico = grafico))
  }
  
 
# Ejecutar para cada modelo
out_muni <- get_coef_df_plot(sar_muni, "Modelo SAR – Municipales")
out_auto <- get_coef_df_plot(sar_auto, "Modelo SAR – Autonómicas")
out_gene <- get_coef_df_plot(sar_gene, "Modelo SAR – Generales")

# Mostrar tablas y gráficas
out_muni$tabla
out_auto$tabla
out_gene$tabla

out_muni$grafico
out_auto$grafico
out_gene$grafico

library(patchwork)


# Quitar ejes y de todos y eje x de los dos primeros
graf_muni <- out_muni$grafico +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())

graf_auto <- out_auto$grafico +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank())

graf_gene <- out_gene$grafico +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank())

# Unir todo con patchwork
layout_vertical <- graf_muni / graf_auto / graf_gene +
  plot_annotation(title = "Coeficientes estimados e IC95% por modelo SAR") &
  theme(plot.title = element_text(face = "bold", size = 18, hjust = 0.5))

layout_vertical




# ——————————————————————————————————————————————————————————————
# 22.5. Comparación visual de predicciones SAR

# 22.5.1. Obtener predicciones por modelo
barrios_sf$pred_muni <- fitted(sar_muni)
barrios_sf$pred_auto <- fitted(sar_auto)
barrios_sf$pred_gene <- fitted(sar_gene)

# 22.5.2. Dataset largo con predicciones
library(dplyr)
library(tidyr)

df_preds <- barrios_sf %>%
  select(CODDISTBAR, pred_muni, pred_auto, pred_gene) %>%
  st_drop_geometry() %>%
  pivot_longer(cols = starts_with("pred_"),
               names_to = "Eleccion", values_to = "Prediccion") %>%
  mutate(Eleccion = dplyr::recode(
    Eleccion,
    "pred_muni" = "Municipales",
    "pred_auto" = "Autonómicas",
    "pred_gene" = "Generales"
  ))

# 22.5.3. Añadir geometría
barrios_preds <- df_preds %>%
  left_join(
    barrios_sf %>% select(CODDISTBAR, geometry) %>% distinct(CODDISTBAR, .keep_all = TRUE),
    by = "CODDISTBAR"
  ) %>%
  st_as_sf()

# 22.5.4. Mapa facetado por elección
ggplot(barrios_preds) +
  geom_sf(aes(fill = Prediccion), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "turbo") +
  facet_wrap(~ Eleccion, ncol = 3) +
  theme_minimal(base_size = 14) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank()) +
  labs(title = "Predicciones de ideología territorial por modelo SAR", fill = "Ideología (ŷ)")

# 22.5.5. Matriz de correlación entre elecciones
barrios_preds_wide <- barrios_preds %>%
  st_drop_geometry() %>%
  group_by(CODDISTBAR, Eleccion) %>%
  summarise(Prediccion = mean(Prediccion, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Eleccion, values_from = Prediccion)

mat_cor <- barrios_preds_wide %>%
  select(Municipales, Autonómicas, Generales) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.data.frame()

cor(mat_cor, use = "pairwise.complete.obs")

# 22.5.6. Matriz de dispersión
library(GGally)
ggpairs(mat_cor, title = "Matriz de dispersión de predicciones SAR por elección")



library(ggplot2)
library(ggrepel)
library(dplyr)
library(patchwork)

# Añadir residuos
barrios_sf$res_sar_muni <- residuals(sar_muni)
barrios_sf$res_sar_auto <- residuals(sar_auto)
barrios_sf$res_sar_gene <- residuals(sar_gene)

# Etiquetas para top 6 residuos
barrios_sf <- barrios_sf %>%
  mutate(
    abs_resid_muni = abs(res_sar_muni),
    abs_resid_auto = abs(res_sar_auto),
    abs_resid_gene = abs(res_sar_gene),
    etiqueta_muni = if_else(rank(-abs_resid_muni) <= 6, as.character(CODDISTBAR), NA),
    etiqueta_auto = if_else(rank(-abs_resid_auto) <= 6, as.character(CODDISTBAR), NA),
    etiqueta_gene = if_else(rank(-abs_resid_gene) <= 6, as.character(CODDISTBAR), NA)
  )

# ——— Gráficos de residuos frente a ideología
p_resid_muni <- ggplot(barrios_sf, aes(x = ideol_muni, y = res_sar_muni, label = etiqueta_muni)) +
  geom_point(color = "darkblue", size = 2) +
  geom_text_repel(size = 4.5, fontface = "bold", color = "black", box.padding = 0.4, max.overlaps = Inf, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Ideología observada (Municipales)", y = "Residuo (ŷ – y)", title = "Municipales") +
  theme_minimal(base_size = 14)

p_resid_auto <- ggplot(barrios_sf, aes(x = ideol_auto, y = res_sar_auto, label = etiqueta_auto)) +
  geom_point(color = "darkblue", size = 2) +
  geom_text_repel(size = 4.5, fontface = "bold", color = "black", box.padding = 0.4, max.overlaps = Inf, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Ideología observada (Autonómicas)", y = "Residuo (ŷ – y)", title = "Autonómicas") +
  theme_minimal(base_size = 14)

p_resid_gene <- ggplot(barrios_sf, aes(x = ideol_gene, y = res_sar_gene, label = etiqueta_gene)) +
  geom_point(color = "darkblue", size = 2) +
  geom_text_repel(size = 4.5, fontface = "bold", color = "black", box.padding = 0.4, max.overlaps = Inf, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Ideología observada (Generales)", y = "Residuo (ŷ – y)", title = "Generales") +
  theme_minimal(base_size = 14)

(p_resid_muni | p_resid_auto | p_resid_gene) +
  plot_annotation(title = "Residuos del modelo SAR frente a la ideología observada")

# ——— Gráficos de ajuste predicho vs observado
p_fit_muni <- ggplot(barrios_sf, aes(x = ideol_muni, y = pred_muni,label = etiqueta_muni)) +
  geom_point(color = "darkblue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_text_repel(size = 4.5, fontface = "bold", color = "black", box.padding = 0.4, max.overlaps = Inf, na.rm = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Ideología observada (Municipales)", y = "Ideología predicha (ŷ)", title = "Municipales") +
  theme_minimal(base_size = 14)

p_fit_auto <- ggplot(barrios_sf, aes(x = ideol_auto, y = pred_auto, label = etiqueta_auto)) +
  geom_point(color = "darkblue", size = 2) +
  geom_text_repel(size = 4.5, fontface = "bold", color = "black", box.padding = 0.4, max.overlaps = Inf, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Ideología observada (Autonómicas)", y = "Ideología predicha (ŷ)", title = "Autonómicas") +
  theme_minimal(base_size = 14)

p_fit_gene <- ggplot(barrios_sf, aes(x = ideol_gene, y = pred_gene, label = etiqueta_gene)) +
  geom_point(color = "darkblue", size = 2) +
  geom_text_repel(size = 4.5, fontface = "bold", color = "black", box.padding = 0.4, max.overlaps = Inf, na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  labs(x = "Ideología observada (Generales)", y = "Ideología predicha (ŷ)", title = "Generales") +
  theme_minimal(base_size = 14)

(p_fit_muni | p_fit_auto | p_fit_gene) +
  plot_annotation(title = "Ideología predicha vs observada por modelo SAR")

############################################################
# 24.Estudio de casos destacados
############################################################

# Definir los barrios de interés
barrios_interes <- c(13, 14, 16, 142, 197, 198)

# Seleccionar columnas relevantes
variables_interes <- c(
  "CODDISTBAR",
  "F1", "F2", "F3",
  "ideol_muni", "pred_muni", "res_sar_muni",
  "ideol_auto", "pred_auto", "res_sar_auto",
  "ideol_gene", "pred_gene", "res_sar_gene"
)

# Filtrar y extraer info
tabla_barrios <- barrios_sf %>%
  st_drop_geometry() %>%
  filter(CODDISTBAR %in% barrios_interes) %>%
  select(all_of(variables_interes)) %>%
  arrange(CODDISTBAR)

# Ver tabla
print(tabla_barrios)

library(tmap)
library(dplyr)

# Incluir también los barrios de foco en el mapa base para evitar recortes
codigos_base <- c(11, 12, 13, 14, 15, 16,22,21,32,31,42,51,52,53)
barrios_base <- barrios_sf %>% filter(CODDISTBAR %in% codigos_base)
barrios_focus <- barrios_sf %>% filter(CODDISTBAR %in% c(13, 14, 16))

# Capa base en gris para contexto
base_map <- tm_shape(barrios_base) +
  tm_fill(col = "gray90", border.col = "gray80") +
  tm_borders(lwd = 0.5, col = "gray70")
tmap_mode("plot")
# Mapa 1: Factor F1
map_f1 <- base_map + tm_shape(barrios_base) +
  tm_fill("F1", title = "Capital socioeconómico (F1)", palette = "Blues", textNA = "") +
  tm_borders(lwd = 1, col = "black") +
  tm_text("CODDISTBAR", size = 1) +
  tm_layout(
    title = "Distribución del factor F1",
    title.position = c("center", "top"),
    title.fontface = "bold",
    title.size = 1.5,
    legend.outside = TRUE,
    legend.outside.position = "bottom",
    legend.stack = "horizontal",
    legend.text.size = 0.8,
    legend.title.size = 1,
    frame = FALSE,
    inner.margins = c(0.01, 0.02, 0.02, 0.02)  # top, left, bottom, right
  )
map_f1

map_f2 <- base_map + 
  tm_shape(barrios_base) +
  tm_fill("F2", title = "Envejecimiento / dependencia (F2)", palette = "Oranges", textNA = "") +
  tm_borders(lwd = 1, col = "black") +
  tm_text("CODDISTBAR", size = 1) +
  tm_layout(
    title = "Distribución del factor F2",
    title.position = c("center", "top"),
    title.fontface = "bold",
    title.size = 1.5,
    legend.outside = TRUE,
    legend.outside.position = "bottom",
    legend.stack = "horizontal",
    legend.text.size = 0.8,
    legend.title.size = 1,
    frame = FALSE,
    inner.margins = c(0.01, 0.02, 0.02, 0.02)  # top, left, bottom, right
  )

map_f2
# Mapa 3: Residuo en autonómicas
barrios_base <- barrios_base %>%
  mutate(resid_auto = pred_auto - ideol_auto)

map_resid <- base_map + tm_shape(barrios_base) +
  tm_fill("resid_auto", title = "Residuo (ŷ - y)", palette = "-RdBu", textNA = "") +
  tm_borders(lwd = 1, col = "black") +
  tm_text("CODDISTBAR", size = 1) +
  tm_layout(
    title = "Residuo ideológico (Autonómicas)",
    title.position = c("center", "top"),
    title.fontface = "bold",
    title.size = 1.5,
    legend.outside = TRUE,
    legend.outside.position = "bottom",
    legend.stack = "horizontal",
    legend.text.size = 0.8,
    legend.title.size = 1,
    frame = FALSE,
    inner.margins = c(0.01, 0.02, 0.02, 0.02)  # top, left, bottom, right
  )
map_resid

tmap_arrange(map_f1, map_f2,map_resid, ncol = 3)
# 1. Preparar datos
codigos_base_centro <- c(11, 12, 13, 14, 15, 16, 21, 22, 31, 32, 42, 51, 52, 53)
barrios_base <- barrios_sf %>%
  filter(CODDISTBAR %in% codigos_base_centro) %>%
  mutate(
    tipo = rep("Observada", n()),
    valor_ideol = ideol_auto
  )

barrios_pred <- barrios_base %>%
  mutate(
    tipo = "Predicha",
    valor_ideol = pred_auto
  )

barrios_plot <- bind_rows(barrios_base, barrios_pred)

# 2. Leyenda común
rango_total <- range(barrios_plot$valor_ideol, na.rm = TRUE)
breaks_comunes <- seq(floor(rango_total[1] * 10)/10, ceiling(rango_total[2] * 10)/10, by = 0.1)

# 3. Mapa con facetas
tmap_mode("plot")

mapa_centro_auto <- tm_shape(barrios_plot) +
  tm_fill("valor_ideol", palette = "-RdBu", title = "Ideología (SAR – Autonómicas)",
          breaks = breaks_comunes, textNA = "") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("CODDISTBAR", size = 1) +
  tm_facets(by = "tipo", nrow = 1) +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.stack = "horizontal",
    legend.text.size = 0.9,
    legend.title.size = 1.2,
    title.size = 1.5,
    frame = FALSE
  )

# Mostrar
mapa_centro_auto
# ----------------------------
# 6. CASO CAMI DE VERA
# 1. Barrios a incluir: 142 (focus) + 135, 141, 153 (colindantes)
codigos_vera <- c(135, 141, 142, 153)
barrios_obs <- barrios_sf %>%
  filter(CODDISTBAR %in% codigos_vera) %>%
  mutate(
    tipo = "Observada",
    valor_ideol = ideol_auto
  )

barrios_pred <- barrios_obs %>%
  mutate(
    tipo = "Predicha",
    valor_ideol = pred_auto
  )

barrios_plot <- bind_rows(barrios_obs, barrios_pred)

# 2. Leyenda común
rango_vera <- range(barrios_plot$valor_ideol, na.rm = TRUE)
breaks_vera <- seq(floor(rango_vera[1] * 10)/10, ceiling(rango_vera[2] * 10)/10, by = 0.05)

# 3. Mapa comparativo
tmap_mode("plot")

mapa_vera <- tm_shape(barrios_plot) +
  tm_fill("valor_ideol", palette = "-RdBu", title = "Ideología (SAR – Autonómicas)",
          breaks = breaks_vera, textNA = "") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("CODDISTBAR", size = 1) +
  tm_facets(by = "tipo", nrow = 1) +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.stack = "horizontal",
    legend.text.size = 0.9,
    legend.title.size = 1.2,
    title.size = 1.5,
    frame = FALSE
  )

# 4. Mostrar
mapa_vera

# ----------------------------
# 6. Caso pobles del sud

# 1. Preparar datos
codigos_base_sud <- c(197, 198, 191, 95, 83, 82)
barrios_base <- barrios_sf %>%
  filter(CODDISTBAR %in% codigos_base_sud) %>%
  mutate(
    tipo = rep("Observada", n()),  # Inicializar columna
    valor_ideol = ideol_gene
  )

barrios_pred <- barrios_base %>%
  mutate(
    tipo = "Predicha",
    valor_ideol = pred_gene
  )

barrios_plot <- bind_rows(barrios_base, barrios_pred)

# 2. Leyenda común
rango_total <- range(barrios_plot$valor_ideol, na.rm = TRUE)
breaks_comunes <- seq(floor(rango_total[1] * 10)/10, ceiling(rango_total[2] * 10)/10, by = 0.1)

# 3. Mapa con facetas
tmap_mode("plot")

map_comparativo <- tm_shape(barrios_plot) +
  tm_fill("valor_ideol", palette = "-RdBu", title = "Ideología (SAR – Generales)", 
          breaks = breaks_comunes, textNA = "") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("CODDISTBAR", size = 1) +
  tm_facets(by = "tipo", nrow = 1) +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.stack = "horizontal",
    legend.text.size = 0.8,
    legend.title.size = 1,
    frame = FALSE
  )

map_comparativo

# 1. Preparar datos
codigos_base_sud <- c(197, 198, 191, 95, 83, 82)
barrios_base <- barrios_sf %>%
  filter(CODDISTBAR %in% codigos_base_sud) %>%
  mutate(
    tipo = rep("Observada", n()),  # Inicializar columna
    valor_ideol = ideol_muni
  )

barrios_pred <- barrios_base %>%
  mutate(
    tipo = "Predicha",
    valor_ideol = pred_muni
  )

barrios_plot <- bind_rows(barrios_base, barrios_pred)

# 2. Leyenda común
rango_total <- range(barrios_plot$valor_ideol, na.rm = TRUE)
breaks_comunes <- seq(floor(rango_total[1] * 10)/10, ceiling(rango_total[2] * 10)/10, by = 0.1)

# 3. Mapa con facetas
tmap_mode("plot")

map_comparativo <- tm_shape(barrios_plot) +
  tm_fill("valor_ideol", palette = "-RdBu", title = "Ideología (SAR – Generales)", 
          breaks = breaks_comunes, textNA = "") +
  tm_borders(col = "black", lwd = 1) +
  tm_text("CODDISTBAR", size = 1) +
  tm_facets(by = "tipo", nrow = 1) +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.stack = "horizontal",
    legend.text.size = 0.8,
    legend.title.size = 1,
    frame = FALSE
  )

map_comparativo
############################################################
# 24. MODELOS SARlag – Con variables socioeconómicas seleccionadas - NO UTILIZADO POR LA MALA CALIDAD DEL AJUSTE
############################################################

library(spatialreg)
library(sf)

vars <- c(
  "Masc", "Porc_men18", "Porc_nacionalidad_extra", "Porc_nacidos_ciudad",
  "Porc_nacida_fuera_UE", "Porc_18añosprimaria_menos", "Porc_25anos_bach_mas",
  "Rentamedia_hogar", "Gini_renta", "Porc_ingresos_salarios",
  "Porc_ingresos_pensiones", "Porc_otros_ingresos",
  "Porc_ingresos_ud_consumo_debajo_10k", "Ind_socioecon",
  "Ind_vulnerabilidad", "Porc_parados", "E_media", "Envej", "Ind_dep"
)

# 3) Ajuste especial para “Pob”
ajusta_pob <- function(Pob) {
  out <- numeric(88)
  out[1:73]  <- Pob[1:73]
  out[74]    <-  363    # copia manual de datos electorales “apilados”
  out[75]    <-   56
  out[76]    <-    0
  out[77:86] <- Pob[75:84]
  out[87]    <- 4969
  out[88]    <- 1668
  out
}

# 4) Ajustar cada variable y añadirla a barrios_sf
# 5) Ajusta cada variable y vuelca en tu barrios_sf
for (v in vars) {
  raw <- socio_vars[[v]]
  barrios_sf[[v]] <- ajusta(raw)
}

# 5) Ajustar pob
barrios_sf$Pob <- ajusta_pob(socio_vars$Pob)

# 1. Crear fórmula para el modelo
fórmula_vars <- as.formula(paste("ideol_muni ~", paste(vars, collapse = " + ")))

# 2. Crear matriz de pesos espaciales
nb <- poly2nb(barrios_sf, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# 3. Ajustar el modelo SARlag
sar_muni_vars <- lagsarlm(fórmula_vars, data = barrios_sf, listw = lw, zero.policy = TRUE)
summary(sar_muni_vars)

# 4. Comparar con el modelo OLS (si quieres)
ols_muni_vars <- lm(fórmula_vars, data = barrios_sf)
AIC(ols_muni_vars, sar_muni_vars)

# 5. Test de Moran sobre residuos
moran.test(residuals(sar_muni_vars), lw, zero.policy = TRUE)

