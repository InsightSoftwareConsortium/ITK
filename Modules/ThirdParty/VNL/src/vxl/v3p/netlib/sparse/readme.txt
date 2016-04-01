#                         Sparse1.3
#              A Sparse Linear Equa#                         Sparse1.3
#              A Sparse Linear Equation Solver
#
#                     Kenneth S. Kundert
#              Alberto Sangiovanni-Vincentelli
#            University of California, Berkeley
#
#
#      Sparse1.3 is a flexible package of subroutines  written
# in  C used to quickly and accurately solve large sparse sys-
# tems of linear equations.  The package  is  able  to  handle
# arbitrary real and complex square matrix equations.  Besides
# being able to solve linear  systems,  it  is  also  able  to
# quickly  solve  transposed  systems,  find determinants, and
# estimate errors due to ill-conditioning  in  the  system  of
# equations  and instability in the computations.  Sparse also
# provides a test program that is able read  matrix  equations
# from  a file, solve them, and print useful information about
# the equation and its solution.
#
#      Sparse1.3 is generally as fast  or  faster  than  other
# popular sparse matrix packages when solving many matrices of
# similar structure.  Sparse does not require or  assume  sym-
# metry  and  is  able  to perform numerical pivoting to avoid
# unnecessary error in  the  solution.   It  handles  its  own
# memory allocation, which allows the user to forgo the hassle
# of providing adequate memory.  It also has a natural, flexi-
# ble, and efficient interface to the calling program.
#
#      Sparse was originally written for use in circuit  simu-
# lators  and  is  particularly  apt  at  handling  node-  and
# modified-node admittance matrices.  The  systems  of  linear
# generated  in  a  circuit  simulator stem from solving large
# systems of nonlinear equations  using  Newton's  method  and
# integrating  large  stiff  systems  of ordinary differential
# equations.  However, Sparse is also suitable for other uses,
# one  in  particular  is  solving  the  very large systems of
# linear equations resulting from the  numerical  solution  of
# partial differential equations.
#
#tion Solver
#
#                     Kenneth S. Kundert
#              Alberto Sangiovanni-Vincentelli
#            University of California, Berkeley
#
#
#      Sparse1.3 is a flexible package of subroutines  written
# in  C used to quickly and accurately solve large sparse sys-
# tems of linear equations.  The package  is  able  to  handle
# arbitrary real and complex square matrix equations.  Besides
# being able to solve linear  systems,  it  is  also  able  to
# quickly  solve  transposed  systems,  find determinants, and
# estimate errors due to ill-conditioning  in  the  system  of
# equations  and instability in the computations.  Sparse also
# provides a test program that is able read  matrix  equations
# from  a file, solve them, and print useful information about
# the equation and its solution.
#
#      Sparse1.3 is generally as fast  or  faster  than  other
# popular sparse matrix packages when solving many matrices of
# similar structure.  Sparse does not require or  assume  sym-
# metry  and  is  able  to perform numerical pivoting to avoid
# unnecessary error in  the  solution.   It  handles  its  own
# memory allocation, which allows the user to forgo the hassle
# of providing adequate memory.  It also has a natural, flexi-
# ble, and efficient interface to the calling program.
#
#      Sparse was originally written for use in circuit  simu-
# lators  and  is  particularly  apt  at  handling  node-  and
# modified-node admittance matrices.  The  systems  of  linear
# generated  in  a  circuit  simulator stem from solving large
# systems of nonlinear equations  using  Newton's  method  and
# integrating  large  stiff  systems  of ordinary differential
# equations.  However, Sparse is also suitable for other uses,
# one  in  particular  is  solving  the  very large systems of
# linear equations resulting from the  numerical  solution  of
# partial differential equations.
#
#
