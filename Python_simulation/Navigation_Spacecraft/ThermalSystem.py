from components.thermal_system import ThermalControlSystem
from components.shielding import ShieldingMaterial
from components.blackhole_metrics import BlackHoleMetric

# === SYSTEMS ===
thermal_system = ThermalControlSystem()
print("Heat dissipation at 9.86 Rs:", thermal_system.heat_dissipation(9.86), "W")

# === SHIELDING MATERIAL ===
tps = ShieldingMaterial("C–C Composite", "Carbon–Carbon", "Radiation and thermal shielding")
print(tps.describe())

# === BLACK HOLE METRICS ===
metrics = [
    BlackHoleMetric("Frolov", "Spacetime near black holes with charge and rotation"),
    BlackHoleMetric("Schwarzschild", "Spherical, non-rotating black holes"),
    BlackHoleMetric("Kerr", "Rotating black holes")
]

for metric in metrics:
    print(metric.summary())
