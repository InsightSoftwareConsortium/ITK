#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="Eigen3"
readonly ownership="Eigen Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/Eigen3/src/itkeigen"
readonly repo="https://github.com/InsightSoftwareConsortium/eigen"
readonly tag="for/itk-20250707-master-6854da2e"
readonly paths="
Eigen/Cholesky
Eigen/CholmodSupport
Eigen/Core
Eigen/Dense
Eigen/Eigen
Eigen/Eigenvalues
Eigen/Geometry
Eigen/Householder
Eigen/IterativeLinearSolvers
Eigen/Jacobi
Eigen/LU
Eigen/MetisSupport
Eigen/OrderingMethods
Eigen/PaStiXSupport
Eigen/PardisoSupport
Eigen/QR
Eigen/QtAlignedMalloc
Eigen/SPQRSupport
Eigen/SVD
Eigen/Sparse
Eigen/SparseCholesky
Eigen/SparseCore
Eigen/SparseLU
Eigen/SparseQR
Eigen/StdDeque
Eigen/StdList
Eigen/StdVector
Eigen/SuperLUSupport
Eigen/UmfPackSupport
Eigen/src

COPYING.BSD
COPYING.MINPACK
COPYING.MPL2
COPYING.README
README.md
README.kitware.md

CMakeLists.txt
cmake/FindStandardMathLibrary.cmake
cmake/Eigen3Config.cmake.in

.gitattributes
"

extract_source () {
    git_archive
    pushd "$extractdir"
    popd
}

source "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/update-third-party.bash"
