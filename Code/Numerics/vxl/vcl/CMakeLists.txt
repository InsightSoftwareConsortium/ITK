LIBRARY( vcl )

SOURCE_FILES(
vcl_cmath
vcl_cassert
vcl_cstdlib
)

WIN32_DEFINES( -DBUILDING_VCL_DLL )
WIN32_INCLUDE_DIRECTORIES( config.win32-vc60 )

AUX_SOURCE_DIRECTORY( Templates )


SUBDIRS( tests )

