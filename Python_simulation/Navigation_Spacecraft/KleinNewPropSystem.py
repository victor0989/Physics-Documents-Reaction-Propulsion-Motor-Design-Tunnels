import numpy as np

# Constantes físicas
g0 = 9.80665  # gravedad estándar en m/s²
R = 8.314     # constante de los gases ideales
Cp = 1005     # capacidad calorífica a presión constante (J/kg·K) para aire

# Datos de entrada para propulsión
chamber_temp = 3200  # Temperatura de cámara en K (alta para carbono-carbono)
exit_temp = 1400     # Temperatura de salida en K
exit_velocity = 4500 # Velocidad de salida de gases en m/s
mass_flow_rate = 0.5 # kg/s

# 1. Impulso específico (Isp) en el vacío
def specific_impulse(v_exit):
    return v_exit / g0

# 2. Eficiencia térmica de ciclo ideal (simplificado)
def thermal_efficiency(T1, T3):
    return 1 - T3 / T1

# 3. Blade loading coefficient (psi) usando geometría de flujo
def blade_loading_coefficient(Ca, beta2_deg, beta3_deg, U):
    beta2 = np.radians(beta2_deg)
    beta3 = np.radians(beta3_deg)
    return 2 * (Ca / U) * (np.tan(beta2) + np.tan(beta3))

# 4. Simulación de materiales: carbon-carbon vs aleaciones fundidas
def material_temperature_limit(material: str):
    limits = {
        'carbon-carbon': 4000,   # K
        'titanium alloy': 2200,  # K
        'melted alloy-X': 2700,  # K
        'graphene hybrid': 4500  # K
    }
    return limits.get(material.lower(), "Unknown material")

# 5. Gravedad compactificada: modelo conceptual (Kaluza-Klein)
def kaluza_klein_radius(G5, G4):
    # r_c² = (G5 / G4) * (1 / 2π)
    return np.sqrt((G5 / G4) / (2 * np.pi))

# Resultados
print("=== PROPULSION SYSTEM SIMULATION ===")
Isp = specific_impulse(exit_velocity)
print(f"Specific Impulse (vacuum): {Isp:.2f} s")

eff = thermal_efficiency(chamber_temp, exit_temp)
print(f"Thermal Efficiency (Ideal): {eff * 100:.2f} %")

psi = blade_loading_coefficient(Ca=180, beta2_deg=45, beta3_deg=-40, U=600)
print(f"Blade Loading Coefficient ψ: {psi:.2f}")

# Material comparison
for mat in ['Carbon-Carbon', 'Titanium Alloy', 'Melted Alloy-X', 'Graphene Hybrid']:
    Tmax = material_temperature_limit(mat)
    print(f"Max Operating Temp for {mat}: {Tmax} K")

# Modelo compacto de curvatura extra dimensional
G5 = 6.67e-39  # gravedad en 5D
G4 = 6.67e-11  # gravedad en 4D
radius = kaluza_klein_radius(G5, G4)
print(f"Kaluza-Klein Compactification Radius: {radius:.3e} m")
