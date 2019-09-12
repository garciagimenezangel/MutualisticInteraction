# Parameters to be read for file "Mutualism.R"

# Run (0:All, 1:First, 2:Second, 3:Sensitivity)
run = 1 
title = "PUGR - Strong mutualism"
# Equations
params=c(
c1=0.1,
b2=0.2,
a1=0.1,
c2=0.2,
b2=1.5,
a2=0.4
)

# Time steps
hs=0.0024
m=1000

# Initial condition
x0.First = c(1.,1.)
x0.Second= c(10,1)
x0.Sens  = c(10,1,0,0)
