class ThermalControlSystem:
    def __init__(self):
        self.oper_temp_range = (20, 150)
        self.survival_temp_range = ((10, 190), (-80, -130))

    def heat_dissipation(self, distance_rs):
        if distance_rs == 9.86:
            return 6400  # W
        return None

class ShieldingMaterial:
    def __init__(self, name, composition, purpose):
        self.name = name
        self.composition = composition
        self.purpose = purpose

    def describe(self):
        return f"{self.name}: {self.composition}, used for {self.purpose}"

class BlackHoleMetric:
    def __init__(self, type_name, description):
        self.type_name = type_name
        self.description = description

    def summary(self):
        return f"{self.type_name} describes: {self.description}"

metrics = [
    BlackHoleMetric("Frolov", "Spacetime near black holes with charge and rotation"),
    BlackHoleMetric("Schwarzschild", "Spherical, non-rotating black holes"),
    BlackHoleMetric("Kerr", "Rotating black holes")
]
