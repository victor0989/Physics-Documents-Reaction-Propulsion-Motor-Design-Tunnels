class MagneticContainmentSystem:
    def __init__(self, coil_temp_k=20.0):
        self.coil_temp_k = coil_temp_k
        self.superconducting = self.coil_temp_k < 30  # simple condition
        self.field_strength_t = 4.5 if self.superconducting else 0.0  # Tesla

    def cool_down(self, delta_k):
        self.coil_temp_k -= delta_k
        self.superconducting = self.coil_temp_k < 30
        self.field_strength_t = 4.5 if self.superconducting else 0.0

        return {
            "Coil Temp (K)": round(self.coil_temp_k, 2),
            "Superconducting": self.superconducting,
            "Field Strength (T)": self.field_strength_t
        }
