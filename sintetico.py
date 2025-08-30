import pandas as pd
import random
from faker import Faker

fake = Faker()

# Lista de estados de México
estados_mexico = [
    "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Chiapas",
    "Chihuahua", "Ciudad de México", "Coahuila", "Colima", "Durango",
    "Estado de México", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
    "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca",
    "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa",
    "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
    "Yucatán", "Zacatecas"
]

# Lista de países
paises = [fake.country() for _ in range(200)]

# Generar datos sintéticos
n = 5000
data = []

for _ in range(n):
    anio = random.randint(2000, 2023)
    trimestre = random.randint(1, 4)
    # Fecha representativa del trimestre (ejemplo: primer día del último mes del trimestre)
    mes = trimestre * 3
    fecha = pd.to_datetime(f"{anio}-{mes}-01")
    millones = round(random.uniform(0, 10), 6)  # entre 0 y 10 millones
    
    fila = [
        random.choice(estados_mexico),
        random.choice(paises),
        anio,
        trimestre,
        fecha.strftime("%Y-%m-%d"),
        millones
    ]
    data.append(fila)

# Crear DataFrame
df = pd.DataFrame(data, columns=["entidad", "pais_de_origen", "anio", "trimestre", "fecha", "millones_de_dolares"])

df.to_csv("dato_sinteticos.csv", index=False)
