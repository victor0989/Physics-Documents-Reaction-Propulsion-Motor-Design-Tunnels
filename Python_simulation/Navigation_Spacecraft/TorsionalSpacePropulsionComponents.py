GOAL: Reduce ∇·T = 0 to usable form.

STRESS-ENERGY TENSOR for perfect fluid (curved or flat spacetime):
  T = (ρ + p) u ⊗ u + p g        (Eq. 22.9)

USEFUL IDENTITIES:
  ∇g = 0    (metric compatibility)
  (∇p)·g = ∇p
  u·∇u = ½ ∇(u²) = 0   (since u² = -1)

DIVERGENCE OF T:
  ∇·T = ∇[(ρ + p) u ⊗ u] + ∇(p g)
       = (∇ρ + ∇p)(u·u) + (ρ + p)(∇·u) + ∇p
       = u(∇ρ) + (ρ + p) ∇·u + ∇p

PROJECT ALONG 4-VELOCITY u:
  u·(∇·T) = -∇ρ - (ρ + p) ∇·u         (Eq. 22.11a)

Combine with baryon conservation:
  ∇·(n u) = 0  ⇒  dn/dτ = -n ∇·u

⇒  dρ/dτ = (ρ + p)/n · dn/dτ         (Eq. 22.11a)
⇒  ds/dτ = 0                         (Eq. 22.11b)
    → Local adiabaticity (no heat flow)

REMAINING: Project orthogonal to u using:
  P = g + u ⊗ u                      (Eq. 22.12)

PROJECTED DYNAMICS ("Euler Eq."):
  (ρ + p) ∇_u u = - P·∇p             (Eq. 22.13)

This yields:
  mass density × 4-acceleration = - pressure gradient

──────────────────────────────────────────────
Box 22.2 — SUMMARY: PERFECT FLUID IN CURVED SPACETIME

A. STATE VARIABLES (10 total):
  Thermodynamic (6):
    n  = baryon density
    ρ  = total energy density
    p  = pressure
    T  = temperature
    s  = entropy per baryon
    μ  = chemical potential = (ρ + p)/n
  Kinematic (4):
    u^α = 4-velocity (normalized: u·u = -1)

B. GOVERNING EQUATIONS (10 total):
  (1) Equation of state:     p = p(n, s)
  (2) Temperature function:  T = T(n, s)
  (3) Compatibility constraint (Maxwell relation)
  (4) Chemical potential:    μ = (ρ + p)/n
  (5) Baryon conservation:   ∇·(n u) = 0
  (6) Adiabaticity:          ds/dτ = 0
  (7) First law:             dρ = (ρ + p)/n dn + nT ds
  (8) Euler equation:        (ρ + p) ∇_u u = - P·∇p
  (9) ∇·T = 0 (local energy-momentum conservation)
  (10) Normalization:        u·u = -1

This motivates the definition
Riemann (..., C, A, B) = [VA' Vs]C.
[empty slot for inserting a one-form]

B. Failure of this Definition
(2)
1. Definition acceptable only if Riemann (..., C, A, B) is a linear machine, independent of how A, B, C vary from point to point.
2. Check, in part: change variations of C, but not C itself, at event <;10:
   CNEW(W) = f(W) COLD(W),
   [arbitrary function except f(W0) = 1]
3. Does this change [VA' Vs]C? Yes! Exercise 11.1 shows

C. Modified Definition of Riemann:
1. The term causing trouble, COLD V[A,S], can be disposed of by subtracting a "correction term" resembling it from Riemann—i.e., by redefining
   Riemann (..., C, A, B) ≡ ℛ(A, B)C,
   ℛ(A, B) ≡ [VA' Vs] - V[A,S]
(3)
(4)
2. The above calculation then gives a result independent of the "modifying function" f.

D. Is Modified Definition Compatible with Equation for Tidal Gravitational Forces?
1. One would like to write Vu Vun + Riemann (..., u, n, u) = 0.
2. This works just as well for modified definition of Riemann as for original definition, because

§11.3. TIDAL FORCES AND RIEMANN TENSOR

ℛ(n, u) = [Vn, Vu] - V[n, u] = [Vn, Vu],
≠ 0 because n = (∂/∂η) and u = (∂/∂λ) commute

Geodesic deviation and tidal forces cannot tell the difference between ℛ(n, u)
and [Vn, Vu], nor consequently between old and new definitions of Riemann.

E. Is Modified Definition Acceptable?
I.e., is Riemann (..., C, A, B) = ℛ(A, B)C a linear machine with output independent of how A, B, C vary near point of evaluation? YES! (See exercise 11.2.)

Take stock, first, of what one knows already about the Riemann curvature tensor.
(1) Riemann is a tensor; despite the appearance of V in its definition (11.9), no derivatives actually act on the input vectors A, B, and C.
(2) Riemann is a 4-tensor; its first slot accepts a 1-form; the others, vectors.
(3) Riemann is determined entirely by V, or equivalently by the geodesics of spacetime, or equivalently by spacetime's parallel transport law; nothing but V and the input vectors and 1-form are required to fix Riemann's output.
(4) Riemann produces the tidal gravitational forces that pry geodesics (test-particle trajectories) apart or push them together; i.e., it characterizes the "curvature of spacetime":
   Vu Vun + Riemann (..., u, n, u) = 0.   (11.10)

(This "equation of geodesic deviation" follows from equations 11.6, 11.8, and 11.9, and the relation [n, u] = 0.)

All these facets of Riemann are pictorial (e.g., geodesic deviation; see Boxes 11.2 and 11.3) or abstract (e.g., equations 11.8 and 11.9 for Riemann in terms of V).
Riemann's component facet (11.11) is related to the component facet of V by the following equation, valid in any coordinate basis {eα} = {∂/∂xα}:
(11.12)

(See exercise 11.3 for derivation, and exercise 11.4 for the extension to noncoordinate bases.)
These components of Riemann, with no sign of any derivative operator anywhere, may leave one with a better feeling in one's stomach than the definition (11
