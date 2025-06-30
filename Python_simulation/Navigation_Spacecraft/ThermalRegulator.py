import random

class ThermalRegulator:
    def __init__(self):
        self.temperature = 300.0  # in Kelvin
        self.threshold = 340.0
        self.ai_enabled = True

    def simulate_heat_input(self, energy_input_joules):
        delta_temp = energy_input_joules / 5000  # ficticio
        self.temperature += delta_temp
        return self.regulate()

    def regulate(self):
        if self.temperature > self.threshold and self.ai_enabled:
            cooling_action = random.uniform(5.0, 15.0)
            self.temperature -= cooling_action
            return {
                "AI Response": "Cooling activated",
                "Cooling Applied": round(cooling_action, 2),
                "New Temp": round(self.temperature, 2)
            }
        return {
            "AI Response": "Stable",
            "Temp": round(self.temperature, 2)
        }
