import numpy as np

def christoffel_symbols(metric, inv_metric, dim):
    """Compute Christoffel symbols of the second kind."""
    gamma = np.zeros((dim, dim, dim))
    for i in range(dim):
        for j in range(dim):
            for k in range(dim):
                gamma[i, j, k] = 0.5 * sum(
                    inv_metric[i, m] * (np.gradient(metric[m, j], axis=k) +
                                         np.gradient(metric[m, k], axis=j) -
                                         np.gradient(metric[j, k], axis=m))
                    for m in range(dim)
                )
    return gamma

def riemann_tensor(gamma, dim):
    """Compute the Riemann curvature tensor."""
    riemann = np.zeros((dim, dim, dim, dim))
    for i in range(dim):
        for j in range(dim):
            for k in range(dim):
                for l in range(dim):
                    riemann[i, j, k, l] = (np.gradient(gamma[i, j, l], axis=k) -
                                           np.gradient(gamma[i, j, k], axis=l) +
                                           sum(gamma[m, j, l] * gamma[i, m, k] -
                                               gamma[m, j, k] * gamma[i, m, l]
                                               for m in range(dim)))
    return riemann

# Define a simple Schwarzschild-like metric
dim = 4
metric = np.eye(dim)  # Placeholder metric (identity matrix for simplicity)
inv_metric = np.linalg.inv(metric)

# Compute Christoffel symbols and Riemann tensor
gamma = christoffel_symbols(metric, inv_metric, dim)
riemann = riemann_tensor(gamma, dim)

# Save results to a text file
with open("riemann_tensor_output.txt", "w") as f:
    f.write("Riemann Tensor Components:\n")
    f.write(str(riemann))

print("Riemann tensor calculation complete. Output saved to riemann_tensor_output.txt")
