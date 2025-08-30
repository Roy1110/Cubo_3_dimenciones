# Instalar si no lo tienes
#install.packages("plotly")
#install.packages("dplyr")

library(plotly)
library(dplyr)

# Datos
anios <- c(2023, 2024)
productos <- c("Laptop", "Tablet", "Teléfono")
regiones <- c("Norte", "Sur")

ventas <- data.frame(
  Año = rep(anios, each = length(productos) * length(regiones)),
  Producto = rep(rep(productos, each = length(regiones)), times = length(anios)),
  Región = rep(regiones, times = length(productos) * length(anios)),
  Ventas = c(
    5000, 4000, 6000, 3500, 8000, 7000,
    5500, 4500, 6200, 3900, 8100, 7300
  )
)

# Asignar coordenadas para simular un cubo 3D
x_map <- setNames(0:(length(anios) - 1), anios)
y_map <- setNames(0:(length(productos) - 1), productos)
z_map <- setNames(0:(length(regiones) - 1), regiones)

ventas <- ventas %>%
  mutate(x = x_map[as.character(Año)], y = y_map[Producto], z = z_map[Región])

# Graficar
fig <- plot_ly(
  ventas,
  x = ~ x,
  y = ~ y,
  z = ~ z,
  type = "scatter3d",
  mode = "markers+text",
  text = ~ paste("Ventas:", Ventas),
  marker = list(
    size = 8,
    color = ~ Ventas,
    colorscale = "Viridis",
    opacity = 0.8
  )
) %>%
  layout(scene = list(
    xaxis = list(
      title = "Año",
      tickvals = 0:1,
      ticktext = anios
    ),
    yaxis = list(
      title = "Producto",
      tickvals = 0:2,
      ticktext = productos
    ),
    zaxis = list(
      title = "Región",
      tickvals = 0:1,
      ticktext = regiones
    )
  ),
  title = "Cubo OLAP(Ventas)")

fig


