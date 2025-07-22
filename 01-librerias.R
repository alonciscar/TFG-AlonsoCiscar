# 01-librerias.R

# Instala los paquetes si no están instalados
paquetes <- c(
  "readr", "readxl", "dplyr", "tidyr", "tibble", "FactoMineR", "factoextra", "psych",
  "ggplot2", "ggrepel", "gplots", "sf", "RColorBrewer", "terra", "tmap", "patchwork",
  "classInt", "corrplot", "car", "MVN", "gridExtra", "lmtest", "viridis", "spdep",
  "spatialreg", "GGally"
)

instalados <- paquetes %in% rownames(installed.packages())
if (any(!instalados)) {
  install.packages(paquetes[!instalados])
}

# Carga de librerías
invisible(lapply(paquetes, library, character.only = TRUE))
