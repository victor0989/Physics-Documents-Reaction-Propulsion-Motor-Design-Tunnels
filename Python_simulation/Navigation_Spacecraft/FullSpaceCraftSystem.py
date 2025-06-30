class FullSpacecraftSystem:
    def __init__(self):
        self.core = SpacecraftCore()
        self.propulsion = PropulsionSystem()
        self.energy = EnergySystem()
        self.shielding = ShieldingSystem()
        self.control = ControlAndNavigation()

    def launch_sequence(self):
        print("\n--- LAUNCH SEQUENCE INITIATED ---")
        self.core.describe()
        self.energy.reactor_status()
        self.shielding.engage_shields()
        self.control.diagnostics()
        self.propulsion.ignite_hybrid_booster()
        self.propulsion.activate_plasma_drive(65)

# Simulación
ship = FullSpacecraftSystem()
ship.launch_sequence()
