import trimesh
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection


def combine_meshes(mesh_list):
    # Solo combina las geometrías en una escena visual
    combined = trimesh.util.concatenate(mesh_list)
    return combined


def create_mining_robot():
    parts = []

    # ----- Cuerpo principal -----
    body = trimesh.creation.box(extents=[1.2, 1.0, 1.0])
    body.apply_translation([0, 0, 0.5])
    parts.append(body)

    # ----- Taladro frontal -----
    shaft = trimesh.creation.cylinder(radius=0.08, height=0.7)
    shaft.apply_translation([0, 0.6, 0.4])
    tip = trimesh.creation.cone(radius=0.1, height=0.25)
    tip.apply_translation([0, 0.95, 0.4])
    parts.extend([shaft, tip])

    # ----- Brazo robótico articulado (3 segmentos) -----
    for i in range(3):
        segment = trimesh.creation.cylinder(radius=0.06, height=0.3)
        angle = np.radians(i * 12)
        segment.apply_translation([-0.65, -0.3 + i * 0.3, 0.9])
        segment.apply_transform(trimesh.transformations.rotation_matrix(angle, [0, 0, 1]))
        parts.append(segment)

    # ----- Compartimento de carga trasero -----
    cargo = trimesh.creation.box(extents=[0.8, 0.5, 0.4])
    cargo.apply_translation([0, -0.85, 0.25])
    parts.append(cargo)

    # ----- Panel solar -----
    solar = trimesh.creation.box(extents=[1.4, 0.1, 0.4])
    solar.apply_translation([0, 0, 1.2])
    parts.append(solar)

    # ----- 4 ruedas tipo rover -----
    for x, y in [(-0.5, 0.6), (0.5, 0.6), (-0.5, -0.6), (0.5, -0.6)]:
        wheel = trimesh.creation.cylinder(radius=0.2, height=0.1)
        wheel.apply_translation([x, y, 0.2])
        wheel.apply_transform(trimesh.transformations.rotation_matrix(np.pi / 2, [0, 1, 0]))
        parts.append(wheel)

    return combine_meshes(parts)


def plot_mesh(mesh, filename="Mining_Robot.png"):
    fig = plt.figure(figsize=(10, 10))
    ax = fig.add_subplot(111, projection='3d')

    faces = mesh.faces
    vertices = mesh.vertices
    collection = Poly3DCollection(vertices[faces], alpha=0.95)
    collection.set_facecolor((0.3, 0.5, 0.7, 1))
    collection.set_edgecolor((0.1, 0.1, 0.1, 0.3))
    ax.add_collection3d(collection)

    bounds = mesh.bounds
    ax.set_xlim(bounds[0][0] - 1, bounds[1][0] + 1)
    ax.set_ylim(bounds[0][1] - 1, bounds[1][1] + 1)
    ax.set_zlim(bounds[0][2] - 1, bounds[1][2] + 1)
    ax.set_axis_off()
    ax.view_init(elev=20, azim=30)

    plt.tight_layout()
    output_path = "/mnt/c/Users/PC/PycharmProjects/pythonProject1/Mining_Robot.png"
    plt.savefig(output_path, dpi=300)
    plt.show()


if __name__ == "__main__":
    robot = create_mining_robot()
    plot_mesh(robot)
