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

NightlyDashboardStart:
	$(TCLSHCOMMAND) Testing/Utilities/DashboardManager.tcl Nightly Start

NightlyDashboardBuild:
	$(TCLSHCOMMAND) Testing/Utilities/DashboardManager.tcl Nightly Build

NightlyDashboardTest:
	$(TCLSHCOMMAND) Testing/Utilities/DashboardManager.tcl Nightly Test

NightlyDashboardCoverage:
	$(TCLSHCOMMAND) Testing/Utilities/DashboardManager.tcl Nightly Coverage

END MAKE VERBATIM

