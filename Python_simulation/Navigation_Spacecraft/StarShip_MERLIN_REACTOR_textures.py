import trimesh
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection

# Parámetros base
FUS_RIB_A_BEV_SCALE_X = 1.5
SENSOR_RAD = 0.25
SHIELD_MAJOR_RADIUS = 2.0
SHIELD_MINOR_RADIUS = 0.15

# Colores RGBA para representar materiales (simples)
COLOR_CFRP = (0.2, 0.2, 0.2, 0.8)  # Gris oscuro semitransparente para CFRP
COLOR_CERAMIC_LAYER = (0.7, 0.7, 0.7, 0.3)  # Gris claro translúcido para capa cerámica
COLOR_TITANIUM_LAYER = (0.5, 0.5, 0.6, 0.6)  # Azul grisáceo
COLOR_RADIATION_SHIELD = (0.9, 0.8, 0.3, 0.4)  # Amarillo translúcido para protección radiación
COLOR_VASIMR_MAGNET = (0.3, 0.6, 1.0, 0.7)  # Azul brillante para bobinas superconductoras
COLOR_ION_CHAMBER = (1.0, 0.5, 0.0, 0.6)  # Naranja translúcido
COLOR_MERLIN_ENGINE = (0.8, 0.3, 0.3, 0.7)  # Rojo tenue


# --------------------
# Creación de partes

def create_fuselage():
    """Fuselaje con capas para CFRP y recubrimientos"""
    length = 8
    radius = 1.0 * FUS_RIB_A_BEV_SCALE_X

    # Capa base CFRP
    cfrp = trimesh.creation.cylinder(radius=radius, height=length, sections=64)
    cfrp.apply_translation([0, 0, length / 2])
    cfrp.visual.vertex_colors = np.tile(np.array(COLOR_CFRP) * 255, (len(cfrp.vertices), 1))

    # Capa cerámica interna (un poco más pequeña)
    ceramic = trimesh.creation.cylinder(radius=radius * 0.9, height=length * 0.95, sections=64)
    ceramic.apply_translation([0, 0, length / 2])
    ceramic.visual.vertex_colors = np.tile(np.array(COLOR_CERAMIC_LAYER) * 255, (len(ceramic.vertices), 1))

    # Capa de titanio-berilio (más interna aún)
    titanium = trimesh.creation.cylinder(radius=radius * 0.8, height=length * 0.9, sections=64)
    titanium.apply_translation([0, 0, length / 2])
    titanium.visual.vertex_colors = np.tile(np.array(COLOR_TITANIUM_LAYER) * 255, (len(titanium.vertices), 1))

    # Combinar capas
    fuselage = trimesh.util.concatenate([cfrp, ceramic, titanium])
    return fuselage


def create_radiation_shield():
    """Capa de protección contra radiación (torus translúcido)"""
    shield = trimesh.creation.torus(SHIELD_MAJOR_RADIUS, SHIELD_MINOR_RADIUS)
    shield.apply_translation([0, 0, 4.0])
    shield.visual.vertex_colors = np.tile(np.array(COLOR_RADIATION_SHIELD) * 255, (len(shield.vertices), 1))
    return shield


def create_vasimr_engine():
    """Simulación del motor VASIMR con componentes pequeños"""
    parts = []
    base_length = 2.5

    # Cámara de ionización (cilindro naranja translúcido)
    ion_chamber = trimesh.creation.cylinder(radius=0.6, height=base_length * 0.6, sections=32)
    ion_chamber.apply_translation([0, 0, 1.5])
    ion_chamber.visual.vertex_colors = np.tile(np.array(COLOR_ION_CHAMBER) * 255, (len(ion_chamber.vertices), 1))
    parts.append(ion_chamber)

    # Bobinas superconductoras (anillos azules)
    for z in np.linspace(0.5, 2.0, 3):
        coil = trimesh.creation.torus(major_radius=0.65, minor_radius=0.1)
        coil.apply_translation([0, 0, z])
        coil.visual.vertex_colors = np.tile(np.array(COLOR_VASIMR_MAGNET) * 255, (len(coil.vertices), 1))
        parts.append(coil)

    # Boquilla electromagnética (cono)
    nozzle = trimesh.creation.cone(radius=0.5, height=0.7, sections=32)
    nozzle.apply_translation([0, 0, 0.4])
    nozzle.visual.vertex_colors = np.tile(np.array(COLOR_VASIMR_MAGNET) * 255, (len(nozzle.vertices), 1))
    parts.append(nozzle)

    engine = trimesh.util.concatenate(parts)
    engine.apply_translation([0, 0, 1])  # Posicionar en la nave
    return engine


def create_merlin_engine():
    """Motor Merlin D1 simplificado con geometría"""
    base = trimesh.creation.cylinder(radius=0.4, height=1.5)
    base.visual.vertex_colors = np.tile(np.array(COLOR_MERLIN_ENGINE) * 255, (len(base.vertices), 1))

    # Cámara de combustión (cono invertido)
    combustion_chamber = trimesh.creation.cone(radius=0.3, height=0.7)
    combustion_chamber.apply_translation([0, 0, 1.5])
    combustion_chamber.visual.vertex_colors = np.tile(np.array(COLOR_MERLIN_ENGINE) * 255,
                                                      (len(combustion_chamber.vertices), 1))

    engine = trimesh.util.concatenate([base, combustion_chamber])
    engine.apply_translation([-1.5, 0, 0.8])  # Ajustar a la popa de la nave
    return engine


def create_lidar_and_sensors():
    """Sensores LIDAR y térmicos distribuidos con pequeños esferas y cilindros"""
    sensors = []
    positions = [
        [1.5, 0, 6.5],
        [-1.5, 0, 6.5],
        [1.5, 0, 5.0],
        [-1.5, 0, 5.0],
        [0, 1.7, 6.0],
        [0, -1.7, 6.0],
    ]
    for pos in positions:
        sphere = trimesh.creation.icosphere(subdivisions=2, radius=0.15)
        sphere.apply_translation(pos)
        sphere.visual.vertex_colors = np.tile(np.array([0.1, 0.8, 0.2, 1.0]) * 255, (len(sphere.vertices), 1))  # verde
        sensors.append(sphere)

    # Añadimos sensores térmicos (cilindros)
    thermal_sensor_pos = [
        [0.8, 0.8, 7.0],
        [-0.8, -0.8, 7.0]
    ]
    for pos in thermal_sensor_pos:
        cyl = trimesh.creation.cylinder(radius=0.08, height=0.3)
        cyl.apply_translation(pos)
        cyl.visual.vertex_colors = np.tile(np.array([1.0, 0.4, 0.1, 1.0]) * 255, (len(cyl.vertices), 1))  # naranja
        sensors.append(cyl)

    return sensors


def combine_meshes(mesh_list):
    return trimesh.util.concatenate(mesh_list)


# --------------------
# PLOT 3D CON MATERIALES VISUALES

def plot_mesh(mesh):
    fig = plt.figure(figsize=(14, 14))
    ax = fig.add_subplot(111, projection='3d')

    faces = mesh.faces
    vertices = mesh.vertices

    mesh_collection = Poly3DCollection(vertices[faces], alpha=0.9)
    mesh_collection.set_facecolor((0.4, 0.55, 0.7, 0.9))  # Color base, pero cada mesh tiene color vertex_color
    mesh_collection.set_edgecolor((0.1, 0.1, 0.15, 0.8))
    ax.add_collection3d(mesh_collection)

    bounds = mesh.bounds
    x_min, y_min, z_min = bounds[0]
    x_max, y_max, z_max = bounds[1]
    ax.set_xlim(x_min - 1, x_max + 1)
    ax.set_ylim(y_min - 1, y_max + 1)
    ax.set_zlim(z_min - 1, z_max + 3)

    ax.set_axis_off()
    ax.view_init(elev=35, azim=45)

    plt.tight_layout()
    plt.savefig("advanced_starship_design.png", dpi=300)
    plt.show()


# --------------------
# MAIN

def main():
    fuselage = create_fuselage()
    shield = create_radiation_shield()
    vasimr = create_vasimr_engine()
    merlin = create_merlin_engine()
    sensors = create_lidar_and_sensors()

    all_parts = [fuselage, shield, vasimr, merlin] + sensors
    ship = combine_meshes(all_parts)

    plot_mesh(ship)


if __name__ == "__main__":
    main()

