# vxl/CMakeLists.txt

PROJECT( vxlcore )

SET(global_sources
 vxl_copyright.h
 vxl_config.h.in
)

# common test executable
SUBDIRS(testlib)

# POSIX layer
SUBDIRS(vpl)

# stream I/O
SUBDIRS(vsl)

# utilities
SUBDIRS(vul)

# "basics" (templates)
SUBDIRS(vbl)

# numerics
SUBDIRS(vnl)

# geometry (including computational geometry)
SUBDIRS(vgl)

# image
SUBDIRS(vil)

# Old image library
SUBDIRS(vil1)

# video
SUBDIRS(vidl)

# video (old implementation)
SUBDIRS(vidl_vil1)

# coordinate systems
SUBDIRS(vcsl)

# multi-platform GUI toolkit
SUBDIRS(vgui)

IF(BUILD_EXAMPLES)
  SUBDIRS(examples)
ENDIF(BUILD_EXAMPLES)

IF( BUILD_TESTING )
  SUBDIRS( tests )
ENDIF( BUILD_TESTING )
