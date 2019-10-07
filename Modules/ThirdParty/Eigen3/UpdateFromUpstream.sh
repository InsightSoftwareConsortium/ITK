#!/usr/bin/env bash

set -e
set -x
shopt -s dotglob

readonly name="Eigen3"
readonly ownership="Eigen Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/Eigen3/src/itkeigen"
# readonly repo="https://github.com/eigenteam/eigen-git-mirror"
# readonly repo="https://gitlab.kitware.com/third-party/eigen.git"
readonly repo="https://gitlab.kitware.com/phcerdan/eigen.git"
readonly tag="for/itk-eigen-3.3"
readonly paths="
Eigen/Cholesky
Eigen/CholmodSupport
Eigen/CMakeLists.txt
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
Eigen/PardisoSupport
Eigen/PaStiXSupport
Eigen/QR
Eigen/QtAlignedMalloc
Eigen/Sparse
Eigen/SparseCholesky
Eigen/SparseCore
Eigen/SparseLU
Eigen/SparseQR
Eigen/SPQRSupport
Eigen/StdDeque
Eigen/StdList
Eigen/StdVector
Eigen/SuperLUSupport
Eigen/SVD
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
