#!/usr/bin/env python

# Calculate displacements and strains for line loading in an elastic half-space
# of a isotropic, homogeneous body undergoing small strain.
# See
# Johnson, K.L. Contact Mechanics.  2.2 Line Loading of an Elastic Half-Space:
# Concentrated Normal Force. Cambridge University Press.  1985.
# Fearing, Ronald S and Hollerbach, John M.  Basic Solid Mechanics for Tactile
# Sensing.  A.I. Memo 771 Massachusetts Institute of Technology Artificial
# Intelligence Laboratory.  March, 1984.

import numpy as np

# 3 / 2 P / E / pi
PE_term = 2.0 * 1 / 5 / np.pi
# Poisson's ratio
nu = 0.495

start = -200
stop = 201
side_length = stop - start

x = np.arange(side_length)
x = np.tile(x, (side_length, 1))
x = x.transpose()
y = np.arange(start, stop)
y = np.tile(y, (side_length, 1))
# y = y.transpose()

r = np.sqrt(x * x + y * y)
PEr_term = PE_term / r**4
# remove singularity
PEr_term[0, 0 - start] = 0.0

exx = PEr_term * (nu * (1.0 + nu) * x * y**2 - (1.0 - nu**2) * x**3)
eyy = PEr_term * (nu * (1.0 + nu) * x**3 - (1.0 - nu**2) * x * y**2)
exy = PEr_term * x**2 * y * (nu + 1)

C1 = 1.0
# incorrect ??
ux = (
    PE_term
    / -2.0
    * ((nu + 1) * y**2 / (x**2 + y**2) + (1 + nu**2) * np.log(x**2 + y**2))
    + C1
)
# ux = PE_term / 2. * ( ( 1 - nu**2 ) * (np.log( x**2 + y**2 )*y**2 -x**2) + nu * ( 1 + nu ) *
# np.log( x**2 + y**2 ) ) + C1
# ux = PE_term* ((1 + nu) * (-3*(nu - 1)*x**2 + (1-3*nu)*y**2))/(12*(x**2+y**2)**3)+ C1
# ux = PE_term*(nu + 1)* ((1.-3.*nu)*y**2 - 3*(nu - 1.)*x**2) / (12*(x**2 + y**2)**3) + C1
# ux = PE_term *(np.log(x**2 + y**2)*y**2 + x**2 * (nu - 1))*(nu+1)/2 + C1
# ux = PE_term*-1*(nu + 1)* ((3.*nu-1)*y**2 + 3*(nu - 1.)*x**2) / (12*(x**2 + y**2)**3) + C1
ux = np.nan_to_num(ux)
C2 = 0.0
uy = (
    PE_term
    * (
        (2 * nu**2 + nu - 1) * 0.5 * np.arctan(y / x)
        + (nu + 1) * x * y / 2 / (x**2 + y**2)
    )
    + C2
)
uy = np.nan_to_num(uy)


# write the displacement to a vtk file
with open("LineLoadDisplacement.vtk", "w") as f:
    f.write("# vtk DataFile Version 2.0\n")
    f.write("Displacements for a line load.\n")
    f.write("ASCII\n")
    f.write("DATASET STRUCTURED_POINTS\n")
    f.write("DIMENSIONS " + str(ux.shape[0]) + " " + str(ux.shape[1]) + " 1\n")
    f.write("ORIGIN 0.0 " + str(-1 * start) + " 0.0\n")
    f.write("SPACING 1.0 1.0 1.0\n")
    f.write("\nPOINT_DATA " + str(ux.size) + "\n")
    f.write("VECTORS displacement double\n")
    for i in range(ux.shape[0]):
        for j in range(ux.shape[1]):
            f.write("{0:.20g} {1:.20g} 0.0\n".format(ux[j, i], uy[j, i]))

for i in range(3):
    with open("../Baseline/LineLoadStrainComponent" + str(i) + ".mhd", "w") as f:
        f.write("ObjectType = Image\n")
        f.write("NDims = 2\n")
        f.write("BinaryData = True\n")
        f.write("BinaryDataByteOrderMSB = False\n")
        f.write("CompressedData = False\n")
        f.write("TransformMatrix = 1 0 0 1\n")
        f.write("Offset = 0 0\n")
        f.write("ElementSpacing = 1.0 1.0\n")
        f.write("DimSize = " + str(ux.shape[0]) + " " + str(ux.shape[1]) + "\n")
        f.write("ElementType = MET_DOUBLE\n")
        f.write("ElementDataFile = LineLoadStrainComponent" + str(i) + ".raw\n")
    with open("../Baseline/LineLoadStrainComponent" + str(i) + ".raw", "wb") as f:
        if i == 0:
            exx.tofile(f)
        elif i == 1:
            exy.tofile(f)
        elif i == 2:
            eyy.tofile(f)

with open("../Baseline/LineLoadStrain.vtk", "w") as f:
    f.write("# vtk DataFile Version 2.0\n")
    f.write("Strains for a line load.\n")
    f.write("ASCII\n")
    f.write("DATASET STRUCTURED_POINTS\n")
    f.write("DIMENSIONS " + str(ux.shape[0]) + " " + str(ux.shape[1]) + " 1\n")
    f.write("ORIGIN 0.0 " + str(-1 * start) + " 0.0\n")
    f.write("SPACING 1.0 1.0 1.0\n")
    f.write("\nPOINT_DATA " + str(ux.size) + "\n")
    f.write("TENSORS strain double\n")
    for i in range(ux.shape[0]):
        for j in range(ux.shape[1]):
            f.write(str(exx[j, i]) + " " + str(exy[j, i]) + " 0.0\n")
            f.write(str(exy[j, i]) + " " + str(eyy[j, i]) + " 0.0\n")
            f.write("0.0 0.0 0.0\n\n")
