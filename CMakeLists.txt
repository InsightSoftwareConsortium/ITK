PROJECT=ITK

SUBDIRS = \
Code \
Testing 

INCLUDE_DIRECTORIES = \
${CMAKE_BINARY_DIR} \
${CMAKE_BINARY_DIR}/Code/Insight3DParty/vxl \
${CMAKE_SOURCE_ROOT}/Code/Common \
${CMAKE_SOURCE_ROOT}/Code/Insight3DParty/vxl 

LINK_DIRECTORIES = \
${CMAKE_BINARY_DIR}/Code/Common \
${CMAKE_BINARY_DIR}/Code/Insight3DParty/vxl 

LINK_LIBRARIES = \
ITKCommon \
ITKNumerics

WIN32_LIBRARIES = 

UNIX_LIBRARIES = \
$(DL_LIBS) \
-lm

BEGIN MAKE VERBATIM

Dashboard:
	$(MAKE) -C Testing/HTML/TestingResults/Dashboard Dashboard

DashboardBuild:
	$(TCLSH) Testing/Utilities/Build.tcl

DashboardTest:
	$(TCLSH) Testing/Utilities/Test.tcl

DashboardSubmission:
	$(TCLSH) Testing/Utilities/Submit.tcl

END MAKE VERBATIM

