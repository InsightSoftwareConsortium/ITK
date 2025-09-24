#!/usr/bin/env bash
#
# Process for updating eigen
#
cat > /dev/null << EOF
TMP_DIR=${HOME}/tmp
mkdir -p ${TMP_DIR}
cd ${TMP_DIR}
rm -rf ${TMP_DIR}/eigen
git clone git@gitlab.com:libeigen/eigen.git
cd eigen
git remote add isceigen git@github.com:InsightSoftwareConsortium/eigen.git
git fetch isceigen
previous_isceigen_branch="for/itk-20250707-master-6854da2e"
eigen_upstream_newreferenceupstream="origin/3.4.1"

eigen_master_short_hash=$(git rev-parse --short ${eigen_upstream_newreferenceupstream})
today=$(date +"%Y%m%d")
new_isceigen_branch="for/itk-${today}-master-${eigen_master_short_hash}"
git checkout isceigen/${previous_isceigen_branch} -b ${new_isceigen_branch}
git rebase ${eigen_upstream_newreferenceupstream}
git push isceigen ${new_isceigen_branch}:${new_isceigen_branch}
echo "Change 'tag' line in Modules/ThirdParty/Eigen3/UpdateFromUpstream.sh to contain ${new_isceigen_branch}"
EOF


set -e
set -x
shopt -s dotglob

readonly name="Eigen3"
readonly ownership="Eigen Upstream <kwrobot@kitware.com>"
readonly subtree="Modules/ThirdParty/Eigen3/src/itkeigen"
readonly repo="https://github.com/InsightSoftwareConsortium/eigen"
readonly tag="for/itk-20250924-master-b66188b5d"  # Corresponds to eigen 3.4.1 tag
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
