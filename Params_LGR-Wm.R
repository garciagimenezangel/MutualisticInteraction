# Parameters to be read for file "Mutualism.R"

# Run (0:All, 1:First, 2:Second, 3:Sensitivity)
run = 2 
title = "Limited per capita growth rate - Weak mutualism"
# Equations
params=c(
c1=0.1,
m1=0.2,
d1=0.1,
a1=0.1,
c2=0.1,
m2=0.2,
d2=0.5,
a2=0.4
)

# Time steps
hs=0.05
m=1000

# Initial condition
x0.First = c(1,1)
x0.Second= c(1,1)
x0.Sens  = c(1,1,0,0)
