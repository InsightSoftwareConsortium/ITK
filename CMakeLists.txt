
SUBDIRS = \
Code \
Testing 

INCLUDE_DIRECTORIES = \
${CMAKE_CONFIG_DIR} \
${CMAKE_CONFIG_DIR}/Code/Insight3DParty/vxl \
${srcdir}/Code/Common \
${srcdir}/Code/Insight3DParty/vxl 

LINK_DIRECTORIES = \
${CMAKE_CONFIG_DIR}/Code/Common \
${CMAKE_CONFIG_DIR}/Code/Insight3DParty/vxl 

LINK_LIBRARIES = \
ITKCommon \
ITKNumerics

WIN32_LIBRARIES = 

UNIX_LIBRARIES = \
$(DL_LIBS) \
-lm

ME=ITK
