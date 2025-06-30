import numpy as np
import matplotlib.pyplot as plt

# Simulación simplificada de la trayectoria de un haz de plasma
def plasma_jet_trajectory(initial_velocity, field_strength, angle_deg):
    angle_rad = np.radians(angle_deg)
    t = np.linspace(0, 0.01, 1000)
    acceleration = field_strength / 1e6  # simplificado
    x = initial_velocity * np.cos(angle_rad) * t
    y = initial_velocity * np.sin(angle_rad) * t + 0.5 * acceleration * t**2
    return x, y

x, y = plasma_jet_trajectory(3e5, 2e4, 30)
plt.plot(x, y)
plt.xlabel("Distance (m)")
plt.ylabel("Height (m)")
plt.title("Plasma Jet Trajectory (Simplified)")
plt.grid(True)
#plt.show()
