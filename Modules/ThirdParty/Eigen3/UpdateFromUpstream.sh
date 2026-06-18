#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="Eigen3"
readonly ownership="Eigen Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/Eigen3/src/itkeigen"
readonly exact_tree_match=false
readonly repo="https://github.com/InsightSoftwareConsortium/eigen"
readonly tag="for/itk-eigen-5.0.1-bc3b39870"
readonly paths="
Eigen/AccelerateSupport
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
Eigen/KLUSupport
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
Eigen/ThreadPool
Eigen/UmfPackSupport
Eigen/Version
Eigen/src
unsupported/Eigen/MatrixFunctions
unsupported/Eigen/src/MatrixFunctions

unsupported/Eigen/NonLinearOptimization
unsupported/Eigen/LevenbergMarquardt
unsupported/Eigen/NumericalDiff
unsupported/Eigen/src/NonLinearOptimization
unsupported/Eigen/src/LevenbergMarquardt
unsupported/Eigen/src/NumericalDiff

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
