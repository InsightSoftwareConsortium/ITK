This directory contains a subset of the OpenJPEG project (https://github.com/uclouvain/openjpeg/)

It was retrieved on Wed Mar 20 16:01:08 2019 +0100
URL:
https://github.com/uclouvain/openjpeg/archive/v2.3.0.tar.gz
This is the 2.3.0 Release

Project Description
OpenJPEG is an open-source JPEG 2000 codec written in C language. It has been
developed in order to promote the use of JPEG 2000, a still-image compression
standard from the Joint Photographic Experts Group (JPEG). Since April 2015, it
is officially recognized by ISO/IEC and ITU-T as a JPEG 2000 Reference
Software.

We only include enough of distribution to build the openjp2 library.


Modifications
-------------

- only include the cmake & src/lib/openjp2 add_subdirectory (+toplevel CMakeLists.txt)
- modify toplevel cmakelists.txt to fix compilations issues about missing directories
- add a symbols mangling mechanism
