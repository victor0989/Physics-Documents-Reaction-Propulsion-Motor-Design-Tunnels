import trimesh
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection

# Base ship parameters
FUS_RIB_A_BEV_SCALE_X = 1.5
REACTOR_THICK = 1.0
SENSOR_RAD = 0.25
SHIELD_MAJOR_RADIUS = 2.0
SHIELD_MINOR_RADIUS = 0.15

# --------------------
# PART CREATION FUNCTIONS

def create_fuselage():
    """CFRP/graphene/titanium composite fuselage (long cylinder)"""
    length = 8
    radius = 1.0 * FUS_RIB_A_BEV_SCALE_X
    fuselage = trimesh.creation.cylinder(radius=radius, height=length, sections=64)
    fuselage.apply_translation([0, 0, length/2])
    return fuselage

def create_reactor():
    """VASIMR rear plasma motor (small cylinder)"""
    reactor = trimesh.creation.cylinder(radius=0.8, height=2.0, sections=32)
    reactor.apply_translation([0, 0, 1.0])
    return reactor

def create_shield():
    """Heat/radiation shield (torus)"""
    shield = trimesh.creation.torus(SHIELD_MAJOR_RADIUS, SHIELD_MINOR_RADIUS)
    shield.apply_translation([0, 0, 4.0])
    return shield

def create_cabin():
    """FPGA electronic cabinet (cube)"""
    cabin = trimesh.creation.box(extents=[1.5, 1.5, 1.0])
    cabin.apply_translation([0, 0, 7.5])
    return cabin

def create_thermal_system():
    """Active thermal system (heat pipes simulated as small cylinders)"""
    pipes = []
    for angle in np.linspace(0, 2*np.pi, 6, endpoint=False):
        x = 2.0 * np.cos(angle)
        y = 2.0 * np.sin(angle)
        pipe = trimesh.creation.cylinder(radius=0.05, height=2.5)
        pipe.apply_translation([x, y, 5.0])
        pipes.append(pipe)
    return pipes

def create_lidar_sensors():
    """LIDAR sensors (small spheres)"""
    positions = [
        [1.5, 0, 6.5],
        [-1.5, 0, 6.5],
        [1.5, 0, 5.0],
        [-1.5, 0, 5.0],
    ]
    sensors = [trimesh.creation.icosphere(subdivisions=3, radius=SENSOR_RAD) for _ in positions]
    for sensor, pos in zip(sensors, positions):
        sensor.apply_translation(pos)
    return sensors

def create_auxiliary_jets():
    """Atmospheric auxiliary engines (small cones)"""
    jets = []
    for pos in [[2.5, 0, 3], [-2.5, 0, 3]]:
        jet = trimesh.creation.cone(radius=0.3, height=0.7)
        jet.apply_translation(pos)
        jets.append(jet)
    return jets

def create_solar_panels():
    """Deployable solar panels (rectangular planes)"""
    panels = []
    size = [3.0, 0.1, 1.5]
    positions = [[0, 3.0, 6], [0, -3.0, 6]]
    for pos in positions:
        panel = trimesh.creation.box(extents=size)
        panel.apply_translation(pos)
        panels.append(panel)
    return panels

def create_starwars_fighter():
    """Star Wars style fighter with wings and cannons"""

    parts = []

    # Fuselage: tapered cylinder
    fuselage = trimesh.creation.cylinder(radius=0.3, height=3.5, sections=32)
    fuselage.apply_translation([0, 0, 1.75])
    parts.append(fuselage)

    # Nose sphere
    nose = trimesh.creation.icosphere(subdivisions=2, radius=0.35)
    nose.apply_translation([0, 0, 3.3])
    parts.append(nose)

    # Wings (X-shaped)
    wing1 = trimesh.creation.box(extents=[1.2, 0.05, 0.4])
    wing1.apply_translation([0, 0.8, 1.5])
    rotation_mat1 = trimesh.transformations.rotation_matrix(np.radians(35), [1, 0, 0])
    wing1.apply_transform(rotation_mat1)
    parts.append(wing1)

    wing2 = wing1.copy()
    wing2.apply_translation([0, -1.6, 0])
    parts.append(wing2)

    wing3 = trimesh.creation.box(extents=[1.2, 0.05, 0.4])
    wing3.apply_translation([0, 0.8, 1.5])
    rotation_mat2 = trimesh.transformations.rotation_matrix(np.radians(-35), [1, 0, 0])
    wing3.apply_transform(rotation_mat2)
    parts.append(wing3)

    wing4 = wing3.copy()
    wing4.apply_translation([0, -1.6, 0])
    parts.append(wing4)

    # Rear engine pods
    engine_pod_left = trimesh.creation.cylinder(radius=0.15, height=0.6)
    engine_pod_left.apply_translation([-0.4, 0.75, 0.7])
    parts.append(engine_pod_left)

    engine_pod_right = engine_pod_left.copy()
    engine_pod_right.apply_translation([0, -1.5, 0])
    parts.append(engine_pod_right)

    # Front cannons
    cannon_left = trimesh.creation.cylinder(radius=0.05, height=0.7)
    cannon_left.apply_translation([0.3, 0.3, 3.2])
    parts.append(cannon_left)

    cannon_right = cannon_left.copy()
    cannon_right.apply_translation([0, -0.6, 0])
    parts.append(cannon_right)

    # Combine fighter parts
    fighter = trimesh.util.concatenate(parts)

    # Position fighter near mothership
    fighter.apply_translation([5, 3, 1])

    return fighter

def combine_meshes(mesh_list):
    return trimesh.util.concatenate(mesh_list)

# --------------------
# 3D PLOT

def plot_mesh(mesh):
    fig = plt.figure(figsize=(14,14))
    ax = fig.add_subplot(111, projection='3d')

    faces = mesh.faces
    vertices = mesh.vertices

    mesh_collection = Poly3DCollection(vertices[faces], alpha=0.9)
    mesh_collection.set_facecolor((0.4, 0.55, 0.7, 0.9))  # translucent gray-blue
    mesh_collection.set_edgecolor((0.1, 0.1, 0.15, 0.8))  # dark edges
    ax.add_collection3d(mesh_collection)

    bounds = mesh.bounds
    x_min, y_min, z_min = bounds[0]
    x_max, y_max, z_max = bounds[1]
    ax.set_xlim(x_min - 1, x_max + 6)  # espacio para el fighter separado
    ax.set_ylim(y_min - 1, y_max + 6)
    ax.set_zlim(z_min - 1, z_max + 3)

    ax.set_axis_off()
    ax.view_init(elev=35, azim=45)

    plt.tight_layout()
    plt.savefig("nave_3d_full_system_with_fighter.png", dpi=300)
    plt.show()

# --------------------
# MAIN

def main():
    fuselage = create_fuselage()
    reactor = create_reactor()
    shield = create_shield()
    cabin = create_cabin()
    thermal_system = create_thermal_system()
    sensors = create_lidar_sensors()
    jets = create_auxiliary_jets()
    panels = create_solar_panels()
    fighter = create_starwars_fighter()

    all_parts = [fuselage, reactor, shield, cabin] + thermal_system + sensors + jets + panels + [fighter]
    ship = combine_meshes(all_parts)

    plot_mesh(ship)

if __name__ == "__main__":
    main()

