import numpy as np
import matplotlib.pyplot as plt

# Relación específica de calor para gases de combustión típicos
gamma = 1.22

# Relación de expansión de la tobera
eps = 40

# Discretización del eje longitudinal de la tobera
x = np.linspace(0, 1, 100)
area_ratio = 1 + (eps - 1) * x**2  # Simplificación cuadrática

# Calcular Mach a partir del área usando la ecuación isentrópica (forma implícita invertida)
def mach_from_area(A_Astar, gamma):
    def func(M):
        return ((1/M) * ((2/(gamma+1))*(1 + (gamma-1)/2 * M**2))**((gamma+1)/(2*(gamma-1))) - A_Astar)
    from scipy.optimize import fsolve
    return fsolve(func, 2.0)[0]  # Supersónico

mach_values = [mach_from_area(ar, gamma) for ar in area_ratio]

# Graficar perfil de Mach
plt.plot(x, mach_values, label="Mach")
plt.xlabel("Longitud normalizada de la tobera")
plt.ylabel("Número de Mach")
plt.title("Perfil de Mach a lo largo de la tobera")
plt.grid(True)
plt.legend()
plt.show()
