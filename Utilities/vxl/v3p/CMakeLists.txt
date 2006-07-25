# v3p/CMakeLists.txt

PROJECT( vthreep )

# The borland compiler produces lots of warnings like
# Warning W8004 C:\cygwin\tmp\vxl_nightly_borland\vxl_src\v3p\tiff\tif_fax3.c 256:
#      'a0' is assigned a value that is never used in function Fax3Decode1D
# There is no point in us fixing them locally. IMS
IF (BORLAND)
  SET (CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -w-8004")
  SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -w-8004")
ENDIF (BORLAND)

SUBDIRS(netlib)
