# -*- coding: utf-8 -*-
"""
Integrated Simulation Framework:
CTC Nodes, Black Hole Tensors, Reaction Engine Components, and Quantum Circuit Dynamics
"""

import numpy as np
import matplotlib.pyplot as plt
from scipy.integrate import odeint
from qiskit import QuantumCircuit, Aer, transpile, execute
from qiskit.visualization import plot_histogram

# ----------------------------
# 1. CTC Node Simulation
# ----------------------------

def ctc_node_simulation(time, state, flip_threshold=0.2):
    """
    Simple differential model for CTC node behavior.
    state: represents the entropic arrow direction (+1 or -1)
    time: independent time variable
    flip_probability: random chance to flip state when entropy is low
    """
    # In a full model, dS/dt would be computed from a thermodynamic model.
    # Here we simulate with random fluctuations.
    dstate_dt = 0
    if np.random.rand() < flip_threshold:
        dstate_dt = -state  # Flip the state (simulate inversion)
    return dstate_dt

# Time array for simulation
t = np.linspace(0, 50, 500)
# Initial state (+1: forward in time)
state0 = 1

# Integrate the differential equation
state = odeint(ctc_node_simulation, state0, t).flatten()

# Plot the entropic arrow state over time
plt.figure(figsize=(8, 4))
plt.plot(t, state, label='CTC Node State (Entropic Arrow)')
plt.xlabel('Time')
plt.ylabel('State (+1: forward, -1: reversed)')
plt.title('CTC Node Simulation with Entropy Inversion')
plt.legend()
plt.grid(True)
plt.show()

# ----------------------------
# 2. Black Hole Tensor Influence on Reaction Engine
# ----------------------------

def engine_dynamics(y, t, curvature_factor):
    """
    Simplified engine dynamic simulation.
    y: engine state vector (e.g., representing temperature, pressure)
    t: time
    curvature_factor: factor from black hole tensor simulation (e.g., from Riemann tensor)
    """
    # For demonstration, assume a two-dimensional state vector
    # y[0]: temperature-like parameter, y[1]: pressure-like parameter
    T, P = y
    # Example dynamics: dT/dt = -a*T + curvature_factor * P, dP/dt = -b*P + curvature_factor * T
    a, b = 0.1, 0.1
    dT_dt = -a * T + curvature_factor * P
    dP_dt = -b * P + curvature_factor * T
    return [dT_dt, dP_dt]

# Simulated black hole tensor “curvature factor”
curvature_factor = 0.5  # In a real model, this would be computed from tensor components

# Initial engine state
y0 = [300, 100]  # e.g., temperature in Kelvin, pressure in arbitrary units
t_engine = np.linspace(0, 100, 1000)
engine_state = odeint(engine_dynamics, y0, t_engine, args=(curvature_factor,))

plt.figure(figsize=(8, 4))
plt.plot(t_engine, engine_state[:, 0], label='Temperature')
plt.plot(t_engine, engine_state[:, 1], label='Pressure')
plt.xlabel('Time')
plt.ylabel('Engine State')
plt.title('Reaction Engine Component Dynamics under Curvature Influence')
plt.legend()
plt.grid(True)
plt.show()

# ----------------------------
# 3. Quantum Circuit Simulation (Quantum Curvature Model)
# ----------------------------

def build_quantum_circuit(num_qubits=5, curvature_factor=np.pi/4):
    """
    Build a quantum circuit that simulates black hole curvature effects.
    Each qubit is rotated by an angle related to the curvature_factor,
    and entanglement is introduced with CNOT gates.
    """
    qc = QuantumCircuit(num_qubits)
    
    # Apply Hadamard gates to create superposition
    for q in range(num_qubits):
        qc.h(q)
    
    # Introduce entanglement with CNOT gates
    for q in range(num_qubits - 1):
        qc.cx(q, q+1)
    
    # Apply rotation gates to simulate curvature (RZ gate with curvature_factor)
    for q in range(num_qubits):
        qc.rz(curvature_factor, q)
    
    # Measure all qubits
    qc.measure_all()
    return qc

# Build the circuit
qc = build_quantum_circuit()
print(qc.draw(output='text'))

# Simulate the circuit
simulator = Aer.get_backend('qasm_simulator')
compiled_qc = transpile(qc, simulator)
job = execute(compiled_qc, simulator, shots=1024)
result = job.result()
counts = result.get_counts(qc)

# Plot the histogram of outcomes
plot_histogram(counts).show()

# ----------------------------
# End of Simulation Framework
# ----------------------------

if __name__ == '__main__':
    print("Simulation complete. Check plots and circuit output for details.")
