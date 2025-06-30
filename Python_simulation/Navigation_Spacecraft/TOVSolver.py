import numpy as np
from scipy.integrate import solve_ivp
import matplotlib.pyplot as plt

# Constantes naturales
G = 6.67430e-11  # m^3 kg^-1 s^-2
c = 3.0e8        # m/s

# Ecuación de estado simplificada (plasma relativista ideal)
def eos(n):
    p = K * n**gamma
    rho = p / (gamma - 1)
    return p, rho

# Derivadas de las ecuaciones TOV
def tov_equations(r, y):
    p, m = y
    if p <= 0:
        return [0, 0]
    p_val, rho = eos(n0)  # constante o puede ser una función n(r)
    dpdr = -G * (rho + p / c**2) * (m + 4 * np.pi * r**3 * p / c**2) / (r * (r - 2 * G * m / c**2))
    dmdr = 4 * np.pi * r**2 * rho
    return [dpdr, dmdr]

# Parámetros iniciales
K = 1e-7     # Constante EoS (ajustar)
gamma = 5/3  # índice adiabático
n0 = 1e17    # densidad inicial (ajustar)
p0, rho0 = eos(n0)
m0 = 0.0
r_span = (1e-5, 1e4)
y0 = [p0, m0]

# Solución TOV
sol = solve_ivp(tov_equations, r_span, y0, method='RK45', dense_output=True, max_step=10)

# Plot resultados
r = sol.t
p, m = sol.y
plt.plot(r, p, label='Presión p(r)')
plt.plot(r, m, label='Masa m(r)')
plt.xlabel('Radio (m)')
plt.ylabel('Magnitudes')
plt.legend()
plt.grid(True)
plt.title('Solución TOV para reactor relativista')
plt.show()
