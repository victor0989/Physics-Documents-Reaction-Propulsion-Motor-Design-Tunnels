import networkx as nx
import matplotlib.pyplot as plt

# Create a directed graph
G = nx.DiGraph()

# Add nodes (components)
G.add_node("Sensor")
G.add_node("Amplifier")
G.add_node("Processor")
G.add_node("Output")

# Connections between components
G.add_edge("Sensor", "Amplifier")
G.add_edge("Amplifier", "Processor")
G.add_edge("Processor", "Output")

# Draw the diagram
plt.figure(figsize=(8, 5))
nx.draw(G, with_labels=True, node_size=2000, node_color="lightblue", font_size=12, arrows=True)
plt.title("Simplified technical diagram")
plt.show()