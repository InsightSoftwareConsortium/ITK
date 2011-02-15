#!/bin/sh
if [ $# -lt 2 ]
then
    echo 'BuildKWStyle.sh Usage: BuildKWStyle.sh [build-directory] [install-prefix]'
    exit 1
fi

scriptdir=`( cd $(dirname $0)  ; pwd )`
# echo "scriptdir=${scriptdir}"

build_dir=$1
shift
install_prefix=$1
shift

echo "Building in ${build_dir},installing in ${install_prefix}"

mkdir -p ${build_dir}
cd ${build_dir}
if [ $? -ne 0 ]
then
echo "Failed to access build directory ${build_dir}"
exit 1
fi

cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX:PATH="${install_prefix}" "${scriptdir}"
make
