# ------------------------------- librerias a ocupar
library(dplyr)
library(plotly)

# Cargamos los datasets tenemos las dos rutas de donde obtenemos los archivos
# Llamamos la ruta de los archivos
#mi_data_1 <- read.csv("/home/jimbotz/Desktop/Universidad/Sexto semestre/mineria de datos/p1/Cubo_3_dimenciones/mental_health_dataset.csv")
#mi_data_2 <- read.csv("/home/jimbotz/Desktop/Universidad/Sexto semestre/mineria de datos/p1/Cubo_3_dimenciones/Student Mental health.csv")
#mi_data_3 <- read.csv("/home/jimbotz/Desktop/Universidad/Sexto semestre/mineria de datos/p1/Cubo_3_dimenciones/student_depression_dataset.csv")

mi_data_1 <- read.csv("/home/rodrigo/6to semestre/Mineria de Datos/cubo_3/mental_health_dataset.csv")
mi_data_2 <- read.csv("/home/rodrigo/6to semestre/Mineria de Datos/cubo_3/Student Mental health.csv")
mi_data_3 <- read.csv("/home/rodrigo/6to semestre/Mineria de Datos/cubo_3/student_depression_dataset.csv")


# Limpiamos los datos y los agrupamos en un dataset

# ---------------Dataset 1
# Se busco datasets que concordaran con informacion muy parecida 
mi_data_1 <- mi_data_1 %>%
  mutate(
    # Al revisar el dataset nos percatamos de que los niveles de deprecion iban en un rango de 0-30
    # por ello con el fin de agruparlos, impusimos que los valores mayores a 15 (considerando una media)
    # sean tomados como deprecion y los demas como "no"
    depression = ifelse(depression_score > 15, "Yes", "No"),
    # Y en esta parte fue mas para asegurar, aunque todos los registros se veaian bien definidos, mas valia tomar esa precaucion
    mental_health_history = ifelse(mental_health_history == "Yes", "Yes", "No")
  ) %>%
  rename(
    age = age,
    gender = gender,
    employment_status = employment_status
  ) %>%
  select(age, gender, employment_status, depression, mental_health_history)


# ------------Dataset 2
mi_data_2 <- mi_data_2 %>%
  mutate(
    # Este dataset fue el que impusimos mas cambios, ya que se definian los status por el nombre de la carrera, asi que mejor tomamos a todos como estudiantes
    employment_status = "student",  # todos son estudiantes
    depression = ifelse(Do.you.have.Depression. == "Yes", "Yes", "No"),
    # Aqui no fue como tal la misma de "mental_health_history", pero es una columna similar
    mental_health_history = ifelse(Did.you.seek.any.specialist.for.a.treatment. == "Yes", "Yes", "No")
  ) %>%
  rename(
    age = Age,
    gender = Choose.your.gender
  ) %>%
  select(age, gender, employment_status, depression, mental_health_history)


# -----------------Dataset 3
mi_data_3 <- mi_data_3 %>%
  mutate(
    # De manera similar a lo anterior, aqui como ya estaban definidos a los students solo lo estandarizamos para que cambie de Students->students
    employment_status = ifelse(Profession == "Student", "student", "other"),
    depression = ifelse(Depression == "Yes", "Yes", "No"),
    mental_health_history = ifelse(Family.History.of.Mental.Illness == "Yes", "Yes", "No")
  ) %>%
  rename(
    age = Age,
    gender = Gender
  ) %>%
  select(age, gender, employment_status, depression, mental_health_history)


# Ya que tenemos los datasets con informacion similar, vamos a agruparlo en uno solo
mi_data <- bind_rows(mi_data_1, mi_data_2, mi_data_3)

# Damos un vistazo al dataset resultante
head(mi_data)
str(mi_data)

# Ya que verificamos que se unifico en un dataset, seleccionamos las 3 variables de interes
# Construccion del cubo 3D

# Todas las combinaciones posibles
todas_combinaciones <- expand.grid(
  age = unique(mi_data$age),
  gender = unique(mi_data$gender),
  employment_status = unique(mi_data$employment_status)
)

# Contamos las frecuencias y unimos las combinaciones 
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

# Se hace el mapeo 3D
x_map <- setNames(0:(length(edades_unicas) - 1), edades_unicas)
y_map <- setNames(0:(length(generos_unicos) - 1), generos_unicos)
z_map <- setNames(0:(length(empleos_unicos) - 1), empleos_unicos)

# Asignacion de coord
cubo_data <- cubo_data %>%
  mutate(
    x = x_map[as.character(age)], # altura desde 18 años hacia delante
    y = y_map[gender], # Diferentes posiciones de y para el genero 
    z = z_map[employment_status] #diferentes alturas para diferente tipo de gente en el aspecto de profesion
  )

# Visualización 3D
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
    size = ~sqrt(Frecuencia) + 4,  # Fue tratado con "sqrt", debido a que las dimenciones de las esferas eran muy grandes y no se distinguian en el visualizacion
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

fig
