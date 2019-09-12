# Parameters to be read for file "Mutualism.R"

# Run (0:All, 1:First, 2:Second, 3:Sensitivity)
run = 3 
title = "Sensitivity analysis - PUGR (Wm)"
# Equations
params=c(
  c1=0.1,
  b1=0.02,
  a1=0.1,
  c2=0.2,
  b2=0.15,
  a2=0.4
)

# Time steps
hs=0.05
m=1000

# Initial condition
x0.First = c(1.,1.)
x0.Second= c(10,1)
x0.Sens  = c(1,1,0,0,0,0,0,0,0,0,0,0,0,0)

# Error in parameters:
err.par = 0.001
err.a1  = 0.001

