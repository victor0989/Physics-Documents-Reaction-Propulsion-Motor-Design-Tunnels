import trimesh
import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d.art3d import Poly3DCollection
from pycalculix import *

# ---------- TRIMESH COMPONENTES 3D VISUALES ----------

def create_thruster_body():
    body = trimesh.creation.cylinder(radius=0.6, height=3.0)
    body.apply_translation([0, 0, 1.5])
    return body

def create_nozzle():
    nozzle = trimesh.creation.cone(radius=0.4, height=1.0)
    nozzle.apply_translation([0, 0, 0.5])
    return nozzle

def create_flame():
    flame = trimesh.creation.cone(radius=0.3, height=1.8)
    flame.apply_translation([0, 0, -0.6])
    return flame

def create_test_chamber():
    walls = trimesh.creation.box(extents=[8, 5, 4])
    walls.apply_translation([0, 0, 2])
    return walls

def create_telemetry_monitors():
    monitors = []
    for i, y in enumerate([-1.5, 0, 1.5]):
        mon = trimesh.creation.box(extents=[0.6, 0.1, 0.4])
        mon.apply_translation([3.5, y, 1.4])
        monitors.append(mon)
    return monitors

def create_pressure_sensors():
    sensors = []
    for y in [-0.6, 0.6]:
        s = trimesh.creation.icosphere(radius=0.1)
        s.apply_translation([-0.8, y, 2.2])
        sensors.append(s)
    return sensors

def combine_meshes(meshes):
    return trimesh.util.concatenate(meshes)

# ---------- VISUALIZACIÓN PNG TRIMESH ----------

def plot_mesh(mesh, filename="TestChamber_MLE_Thruster.png"):
    fig = plt.figure(figsize=(14, 14))
    ax = fig.add_subplot(111, projection='3d')
    faces = mesh.faces
    vertices = mesh.vertices
    collection = Poly3DCollection(vertices[faces], alpha=0.92)
    collection.set_facecolor((0.4, 0.6, 0.8, 0.9))
    collection.set_edgecolor((0.1, 0.1, 0.1, 0.1))
    ax.add_collection3d(collection)

    bounds = mesh.bounds
    ax.set_xlim(bounds[0][0] - 1, bounds[1][0] + 1)
    ax.set_ylim(bounds[0][1] - 1, bounds[1][1] + 1)
    ax.set_zlim(bounds[0][2] - 1, bounds[1][2] + 1)
    ax.set_axis_off()
    ax.view_init(elev=20, azim=45)

    plt.tight_layout()
    output_path = "/mnt/c/Users/PC/PycharmProjects/pythonProject1/TestChamber_MLE_Thruster.png"
    plt.savefig(output_path, dpi=300)
    print(f"✅ Imagen guardada en: {output_path}")
    plt.show()

# ---------- SIMULACIÓN STRUCTURAL CON PYCALCULIX ----------

def run_structural_simulation():
    print("🚀 Iniciando simulación estructural con pycalculix...")

    proj = Project('thruster_pressure')
    part = proj.new_part('MLE_thruster')

    # Corte 2D del cilindro vertical
    part.sketch.rectangle(0, 0, 0.6, 3.0)
    part.set_material('aluminum')  # puedes cambiar a titanium si quieres

    part.mesh.set_global_seed(0.05)
    part.mesh.generate()

    # Condiciones de contorno
    part.bc.fix_edge('left')
    part.bc.fix_edge('bottom')

    # Presión interna de 3 MPa
    part.load.pressure('right', -3e6)

    proj.create_job()
    proj.run()

    # Guardar imagen del esfuerzo
    out_path = "/mnt/c/Users/PC/PycharmProjects/pythonProject1/thruster_stress_vonmises.png"
    part.post.plot_nodal_result('s_vonmises', filename=out_path)
    print(f"✅ Simulación estructural guardada en: {out_path}")

# ---------- EJECUCIÓN UNIFICADA ----------

def main():
    # Parte 1: Visualización 3D del entorno de prueba
    components = [
        create_test_chamber(),
        create_thruster_body(),
        create_nozzle(),
        create_flame(),
        *create_telemetry_monitors(),
        *create_pressure_sensors()
    ]
    model = combine_meshes(components)
    plot_mesh(model)

    # Parte 2: Simulación estructural FEM (presión interna en 2D)
    run_structural_simulation()

if __name__ == "__main__":
    main()
