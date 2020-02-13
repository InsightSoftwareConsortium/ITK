#!/usr/bin/env bash

thirdparty_module_name="Eigen3"

# readonly repo="https://gitlab.kitware.com/third-party/eigen.git"
upstream_git_url="https://gitlab.kitware.com/phcerdan/eigen.git"
upstream_git_branch="for/itk-3.3-find_package"

snapshot_author_name="Eigen Upstream"
snapshot_author_email="kwrobot@kitware.com"

snapshot_redact_cmd=''
snapshot_relative_path="src/itkeigen"

snapshot_paths="
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

source "${BASH_SOURCE%/*}/../../../Utilities/Maintenance/UpdateThirdPartyFromUpstream.sh"
update_from_upstream
