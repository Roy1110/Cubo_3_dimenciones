
# -------------------------------
library(plotly)
library(dplyr)

# Age, gender, employer status (in this case we use students)

# Llamamos la ruta de los archivos
mi_data_1 <- read.csv("/home/jimbotz/Desktop/Universidad/Sexto semestre/mineria de datos/p1/Cubo_3_dimenciones/mental_health_dataset.csv")
mi_data_2 <- read.csv("/home/jimbotz/Desktop/Universidad/Sexto semestre/mineria de datos/p1/Cubo_3_dimenciones/Student Mental health.csv")
mi_data_3 <- read.csv("/home/jimbotz/Desktop/Universidad/Sexto semestre/mineria de datos/p1/Cubo_3_dimenciones/student_depression_dataset.csv")

# para tus rutas rodri
# mi_data_1 <- read.csv("/home/jimbotz/Desktop/Universidad/Sexto semestre/mineria de datos/p1/Cubo_3_dimenciones/mental_health_dataset.csv")
# mi_data_2 <- read.csv("/home/jimbotz/Desktop/Universidad/Sexto semestre/mineria de datos/p1/Cubo_3_dimenciones/mental_health_dataset.csv")
# mi_data_3 <- read.csv("/home/jimbotz/Desktop/Universidad/Sexto semestre/mineria de datos/p1/Cubo_3_dimenciones/mental_health_dataset.csv")


# primer csv age, gender, employment_status (tenemos que seleecionar students), depression_score (tenemos que generalizar el score a un si o no) 0-30, mental_health_history
#head(mi_data_1)
# segundo  csv Age, Choose your gender, What is your course? (aca todos son estudiantes), Do you have Depression?, Did you seek any specialist for a treatment?     
#head(mi_data_2)
# tercer csv Age, Gender, Profession (tenemos que seleecionar students), Depression, Family History of Mental Illness (aca si bien no es igual a los otros campos puede servir)
#head(mi_data_3)


# Una vez que verificamos que si abrieron los archivos xd toca seleccionar los campos 
# Seleccionamos las 3 variables más simples que todos tienen pero mientras solo con 1 dataset
todas_combinaciones <- expand.grid(
  age = unique(mi_data$age),
  gender = unique(mi_data$gender),
  employment_status = unique(mi_data$employment_status)
)

# Contamos las frecuencia y unimos las combinaciones aunque deberiamos de hacerlo más selecto ig
cubo_data <- mi_data %>%
  select(age, gender, employment_status) %>%
  group_by(age, gender, employment_status) %>%
  summarise(Frecuencia = n(), .groups = 'drop') %>%
  right_join(todas_combinaciones, by = c("age", "gender", "employment_status")) %>%
  mutate(Frecuencia = ifelse(is.na(Frecuencia), 0, Frecuencia))

# Se sacan las dimensiones unicas para separarlas de manera facil aunque nuestros valores sean categoricos entonces cada texto unico es una dimension
edades_unicas <- sort(unique(cubo_data$age))
generos_unicos <- unique(cubo_data$gender)
empleos_unicos <- unique(cubo_data$employment_status)

# Se hace el mapeo en 3D
x_map <- setNames(0:(length(edades_unicas) - 1), edades_unicas)
y_map <- setNames(0:(length(generos_unicos) - 1), generos_unicos)
z_map <- setNames(0:(length(empleos_unicos) - 1), empleos_unicos)

# Asignacion de las coords
cubo_data <- cubo_data %>%
  mutate(
    x = x_map[as.character(age)], # altura desde 18 años hacia delante
    y = y_map[gender], # Diferentes posiciones de y para el genero 
    z = z_map[employment_status] # diferentes alturas para diferente tipo de gente en el aspecto de profesion
  )

# Paso 6: Crear la visualización 3D
fig <- plot_ly(
  cubo_data,
  x = ~x,
  y = ~y, 
  z = ~z,
  type = "scatter3d",
  mode = "markers",
  text = ~paste("Edad:", age, 
                "<br>Género:", gender, 
                "<br>Estado laboral:", employment_status, 
                "<br>Frecuencia:", Frecuencia),
  hoverinfo = "text",
  marker = list(
    size = ~Frecuencia/2 + 5,  # Tamaño proporcional a la frecuencia
    color = ~Frecuencia,
    colorscale = "Viridis",
    opacity = 0.8,
    showscale = TRUE,
    sizemode = "diameter"
  )
) %>%
  layout(
    scene = list(
      xaxis = list(
        title = "Edad",
        tickvals = 0:(length(edades_unicas) - 1),
        ticktext = edades_unicas
      ),
      yaxis = list(
        title = "Género",
        tickvals = 0:(length(generos_unicos) - 1),
        ticktext = generos_unicos
      ),
      zaxis = list(
        title = "Estado Laboral", 
        tickvals = 0:(length(empleos_unicos) - 1),
        ticktext = empleos_unicos
      )
    ),
    title = "Cubo MOLAP - Distribución por Edad, Género y Estado Laboral",
    margin = list(l = 0, r = 0, b = 0, t = 50)
  )

# Se muestra la fig
fig

