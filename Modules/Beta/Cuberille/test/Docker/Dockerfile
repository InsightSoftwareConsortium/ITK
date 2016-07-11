FROM debian:8
MAINTAINER Insight Software Consortium <community@itk.org>

RUN apt-get update && apt-get install -y \
  build-essential \
  curl \
  cmake \
  git \
  libexpat1-dev \
  libhdf5-dev \
  libjpeg-dev \
  libpng12-dev \
  libpython3-dev \
  libtiff5-dev \
  ninja-build \
  python3 \
  swig3.0 \
  wget \
  vim \
  zlib1g-dev

RUN mkdir -p /usr/src/ITKCuberille-build
WORKDIR /usr/src

# master 2016-07-06
ENV ITK_GIT_TAG 71a779150ee5797e6ca4bef01906d5c12a18188b
RUN git clone git://itk.org/ITK.git && \
  cd ITK && \
  git checkout ${ITK_GIT_TAG} && \
  cd ../ && \
  mkdir ITK-build && \
  cd ITK-build && \
  cmake \
    -G Ninja \
    -DCMAKE_INSTALL_PREFIX:PATH=/usr \
    -DBUILD_EXAMPLES:BOOL=OFF \
    -DBUILD_TESTING:BOOL=OFF \
    -DBUILD_SHARED_LIBS:BOOL=ON \
    -DITK_WRAP_PYTHON:BOOL=OFF \
    -DCMAKE_POSITION_INDEPENDENT_CODE:BOOL=ON \
    -DITK_LEGACY_REMOVE:BOOL=ON \
    -DITK_BUILD_DEFAULT_MODULES:BOOL=OFF \
    -DITK_USE_SYSTEM_LIBRARIES:BOOL=ON \
    -DSWIG_EXECUTABLE:FILEPATH=/usr/bin/swig3.0 \
    -DPYTHON_EXECUTABLE:FILEPATH=/usr/bin/python3 \
    -DModule_ITKImageFunction:BOOL=ON \
    -DModule_ITKImageGradient:BOOL=ON \
    -DModule_ITKMesh:BOOL=ON \
    -DModule_ITKTestKernel:BOOL=ON \
    -DModule_ITKQuadEdgeMesh:BOOL=ON \
    -DModule_ITKQuadEdgeMeshFiltering:BOOL=ON \
    -DModule_ITKThresholding:BOOL=ON \
    -DModule_ITKIOImageBase:BOOL=ON \
    ../ITK && \
  ninja && \
  find . -name '*.o' -delete

