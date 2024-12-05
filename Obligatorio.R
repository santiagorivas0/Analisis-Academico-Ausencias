# Universidad ORT Uruguay - Obligatorio Alvarez / Alvarez / Rivas
# Facultad de Administración y Ciencias Sociales

# Borra todas las variables de la memoria de trabajo
rm(list = ls())

# Cargar el paquete necesario para leer archivos Excel
library(readxl)

# Establecer el directorio de trabajo
setwd("C:/Users/Usuario/Desktop/ORT/Segundo Semestre 2024/Principios de Estadística/Obligatorio")  # Define la carpeta de trabajo

# Cargar la base de datos
mydata <- read_excel("students-por_2024.xlsx")

# Visualizar la estructura de los datos
str(mydata)
head(mydata)

# ---------------------------------------------
# Análisis de la variable "guardian"
# Construcción de la distribución de frecuencias
# ---------------------------------------------

FrecAbs_guardian <- table(mydata$guardian)  # Frecuencia Absoluta
FrecAbs_guardian <- sort(FrecAbs_guardian, decreasing = TRUE)  # Ordenar de mayor a menor
FrecRel_guardian <- prop.table(FrecAbs_guardian)  # Frecuencia Relativa
FrecPorc_guardian <- round(FrecRel_guardian * 100, 2)  # Frecuencia Porcentual

# Combinar en una tabla
tabla_frecuencias_guardian <- cbind(
  Frecuencia_Absoluta = FrecAbs_guardian,
  Frecuencia_Relativa = FrecRel_guardian,
  Frecuencia_Porcentual = FrecPorc_guardian
)

# Ver la tabla resultante
print(tabla_frecuencias_guardian)

#Gráfico de barras ordenado de mayor a menor
barplot_heights <- barplot(FrecAbs_guardian, 
                           main = "Distribución de Tutores Principales", 
                           xlab = "Tutor", 
                           ylab = "Frecuencia Absoluta", 
                           col = "#A8D5BA", 
                           ylim = c(0, max(FrecAbs_guardian) + 20), 
                           font.lab = 2,  # Negrita en los ejes
                           cex.lab = 1.5,  # Tamaño de las etiquetas de los ejes
                           cex.axis = 1.3,  # Tamaño de los números en los ejes
                           cex.main = 2)  # Tamaño del título

# Agregar etiquetas dentro de las barras
text(x = barplot_heights, y = FrecAbs_guardian - 15,  # Ajusta para que queden dentro de la barra
     labels = FrecAbs_guardian, cex = 1.2, col = "black", font = 2)  # Tamaño más grande para los números


# ---------------------------------------------
# Análisis de las variables cuantitativas: absences y G3
# ---------------------------------------------

# Crear clases para absences
breaks_absences <- seq(0, 35, by = 5)  # Clases ajustadas
tabla_absences <- table(cut(mydata$absences, breaks = breaks_absences))  # Tabla de frecuencias

# Crear histograma de absences
hist(mydata$absences, 
     breaks = breaks_absences, 
     main = "Histograma de Ausencias", 
     xlab = "Número de Ausencias", 
     ylab = "Frecuencia", 
     col = "lightblue", 
     cex.main = 2,  # Tamaño del título
     cex.lab = 1.8,  # Tamaño de las etiquetas de los ejes
     cex.axis = 1.5,  # Tamaño de los números de los ejes
     las = 1)  # Rotar los números de los ejes para mejor lectura

# Crear clases para G3
breaks_G3 <- seq(0, 20, by = 5)  # Clases de 5 unidades
tabla_G3 <- table(cut(mydata$G3, breaks = breaks_G3))  # Tabla de frecuencias

# Crear histograma de G3
hist(mydata$G3, 
     breaks = breaks_G3, 
     main = "Histograma de Calificaciones Finales", 
     xlab = "Calificación Final", 
     ylab = "Frecuencia", 
     col = "brown", 
     cex.main = 2,  # Tamaño del título
     cex.lab = 1.8,  # Tamaño de las etiquetas de los ejes
     cex.axis = 1.5,  # Tamaño de los números de los ejes
     las = 1)  # Rotar los números de los ejes para mejor lectura

# ---------------------------------------------
# Cálculo de estadísticas descriptivas para absences y G3
# ---------------------------------------------

# Medidas de tendencia central y dispersión para absences
media_absences <- mean(mydata$absences, na.rm = TRUE)  #na.rm = TRUE ignora valores nulos
media_absences
mediana_absences <- median(mydata$absences, na.rm = TRUE)  #na.rm = TRUE ignora valores nulos
mediana_absences
sd_absences <- sd(mydata$absences, na.rm = TRUE)  #na.rm = TRUE ignora valores nulos
sd_absences

# Medidas de tendencia central y dispersión para G3
media_G3 <- mean(mydata$G3, na.rm = TRUE)  #na.rm = TRUE ignora valores nulos
media_G3
mediana_G3 <- median(mydata$G3, na.rm = TRUE)  #na.rm = TRUE ignora valores nulos
mediana_G3
sd_G3 <- sd(mydata$G3, na.rm = TRUE)  #na.rm = TRUE ignora valores nulos
sd_G3

# ---------------------------------------------
# Detección de observaciones atípicas para absences y G3
# ---------------------------------------------

# Calcular el valor z para los extremos de la variable absences
min_absences <- min(mydata$absences, na.rm = TRUE)  # Valor mínimo ignorando valores nulos
min_absences
max_absences <- max(mydata$absences, na.rm = TRUE)  # Valor máximo ignorando valores nulos
max_absences

z_min_absences <- (min_absences - media_absences) / sd_absences  # Cálculo del z para el mínimo
z_min_absences
z_max_absences <- (max_absences - media_absences) / sd_absences  # Cálculo del z para el máximo
z_max_absences

# Calcular el valor z para los extremos de la variable G3
min_G3 <- min(mydata$G3, na.rm = TRUE)  # Valor mínimo ignorando valores nulos
min_G3
max_G3 <- max(mydata$G3, na.rm = TRUE)  # Valor máximo ignorando valores nulos
max_G3

z_min_G3 <- (min_G3 - media_G3) / sd_G3  # Cálculo del z para el mínimo
z_min_G3
z_max_G3 <- (max_G3 - media_G3) / sd_G3  # Cálculo del z para el máximo
z_max_G3

# ---------------------------------------------
# Comparación de la dispersión de absences y G3 usando el Coeficiente de Variación
# ---------------------------------------------

# Cálculo del Coeficiente de Variación para absences
cv_absences <- sd_absences / media_absences  # CV = Desviación estándar / Media
cv_absences

# Cálculo del Coeficiente de Variación para G3
cv_G3 <- sd_G3 / media_G3  # CV = Desviación estándar / Media
cv_G3

# ---------------------------------------------
# Tabulación cruzada entre absences y guardian
# ---------------------------------------------

# Crear clases para la variable absences
breaks_absences <- seq(0, 35, by = 5)  # Clases de 5 unidades
absences_clases <- cut(mydata$absences, breaks = breaks_absences, include.lowest = TRUE)

# Creación de la tabla
tabla_cruzada <- table(absences_clases, mydata$guardian)
print(tabla_cruzada)

# ---------------------------------------------
# Interpretación de los resultados
# ---------------------------------------------

# Ver las proporciones por fila
proporciones <- prop.table(tabla_cruzada, margin = 1)
print(proporciones)

# ---------------------------------------------
# Análisis de asociación con Chi-Cuadrado e Índice de Cramer
# ---------------------------------------------

# Crear la tabla de contingencia
tabla_cruzada <- table(absences_clases, mydata$guardian)

# Calcular el test de Chi-Cuadrado
chi_test <- chisq.test(tabla_cruzada)

# Mostrar los resultados del test de Chi-Cuadrado
chi_test

# Calcular el Índice de Cramer
n_total <- sum(tabla_cruzada)  # Total de observaciones
k <- min(dim(tabla_cruzada)) - 1  # Número de categorías menos 1
cramer_v <- sqrt(chi_test$statistic / (n_total * k))

# Mostrar el Índice de Cramer
cramer_v

# ---------------------------------------------
# Relación entre las variables absences y G3
# ---------------------------------------------

# Crear el diagrama de dispersión
plot(mydata$absences, mydata$G3,
     main = "Relación entre Ausencias y Calificación Final",
     xlab = "Número de Ausencias",
     ylab = "Calificación Final",
     pch = 19, col = "blue",  # Puntos azules
     cex.main = 2,  # Tamaño del título más grande
     font.main = 2,  # Título en negrita
     cex.lab = 1.8,  # Tamaño de las etiquetas de los ejes
     font.lab = 2,  # Etiquetas de los ejes en negrita
     cex.axis = 1.5)  # Tamaño de los números en los ejes

# Calcular el coeficiente de correlación lineal
correlacion <- cor(mydata$absences, mydata$G3, use = "complete.obs")

# Mostrar el coeficiente de correlación
correlacion

# ---------------------------------------------
# BONUS: Relación entre absences y G3 según guardian (sin incluir NA)
# ---------------------------------------------

# Cargar la librería ggplot2
library(ggplot2)

# Filtrar los datos para excluir valores NA en la variable guardian
filtered_data <- na.omit(mydata[, c("absences", "G3", "guardian")])

# Crear el gráfico de dispersión faceteado por guardian con puntos más grandes
ggplot(filtered_data, aes(x = absences, y = G3)) +
  geom_point(color = "purple", size = 3, alpha = 0.7) +  # Puntos azules más grandes
  facet_wrap(~guardian, scales = "free") +            # Crear cuadrantes según guardian
  labs(
    title = "Relación entre Ausencias y Calificación Final según Tutor",
    x = "Número de Ausencias",
    y = "Calificación Final"
  ) +
  theme_minimal() +                                   # Tema limpio y minimalista
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),  # Título centrado y grande
    axis.title = element_text(size = 14, face = "bold"),              # Ejes en negrita
    axis.text = element_text(size = 12),                              # Texto de los ejes más visible
    strip.text = element_text(size = 14, face = "bold")               # Títulos de facetas en negrita
  )


