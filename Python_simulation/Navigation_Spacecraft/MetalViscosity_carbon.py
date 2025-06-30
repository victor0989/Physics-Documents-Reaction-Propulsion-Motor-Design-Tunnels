import math


class MoltenMetal:
    def __init__(self, viscosity, viscosity_index, surface_tension, oxide_factor,
                 inclusion_factor, solidification_range, mold_conductivity,
                 mold_roughness, superheat, pouring_speed):
        """
        Initialize the molten metal properties and casting parameters.

        Parameters:
        - viscosity: Base viscosity of the molten metal.
        - viscosity_index: Sensitivity of viscosity to temperature.
        - surface_tension: Surface tension of molten metal (without oxides).
        - oxide_factor: Factor by which oxide film increases surface tension.
        - inclusion_factor: Factor representing the effect of inclusions on viscosity.
        - solidification_range: Solidification interval of the alloy (°C).
        - mold_conductivity: Thermal conductivity of the mold.
        - mold_roughness: Roughness factor of the mold surface.
        - superheat: Temperature above melting point (°C).
        - pouring_speed: Speed of pouring molten metal (m/s).
        """
        self.viscosity = viscosity
        self.viscosity_index = viscosity_index
        self.surface_tension = surface_tension * oxide_factor  # adjust for oxide film
        self.inclusion_factor = inclusion_factor
        self.solidification_range = solidification_range
        self.mold_conductivity = mold_conductivity
        self.mold_roughness = mold_roughness
        self.superheat = superheat
        self.pouring_speed = pouring_speed

    def calculate_fluidity(self):
        """
        Calculate a dimensionless fluidity index based on the given parameters.

        The higher the fluidity index, the better the molten metal flows into the mold.
        """

        # 1) Effect of viscosity and inclusions (inverse effect)
        effective_viscosity = self.viscosity * (1 + self.viscosity_index) * (1 + self.inclusion_factor)

        # 2) Effect of surface tension (higher reduces fluidity)
        tension_effect = 1 / self.surface_tension

        # 3) Effect of solidification range (fluidity inversely proportional)
        solidification_effect = 1 / self.solidification_range

        # 4) Effect of mold properties (higher conductivity and roughness reduce fluidity)
        mold_effect = 1 / (self.mold_conductivity * (1 + self.mold_roughness))

        # 5) Effect of superheat (increases fluidity)
        superheat_effect = 1 + (self.superheat / 100)  # scaled factor

        # 6) Effect of pouring speed (higher speed improves fluidity)
        pouring_effect = math.log(1 + self.pouring_speed)

        # Calculate fluidity index as product of all positive effects divided by viscosity
        fluidity_index = (tension_effect * solidification_effect * mold_effect *
                          superheat_effect * pouring_effect) / effective_viscosity

        return fluidity_index


# Example usage:
# Typical values (arbitrary units for demonstration):
metal = MoltenMetal(
    viscosity=1.0,  # base viscosity
    viscosity_index=0.2,  # viscosity sensitivity to temp
    surface_tension=0.8,  # base surface tension (N/m)
    oxide_factor=3.0,  # oxide film triples tension
    inclusion_factor=0.1,  # 10% increase in viscosity due to inclusions
    solidification_range=10.0,  # degrees Celsius
    mold_conductivity=1.5,  # W/m-K (higher reduces fluidity)
    mold_roughness=0.3,  # roughness factor (0 to 1)
    superheat=50,  # degrees Celsius above melting point
    pouring_speed=2.0  # m/s
)

fluidity = metal.calculate_fluidity()
print(f"Calculated fluidity index: {fluidity:.4f}")
