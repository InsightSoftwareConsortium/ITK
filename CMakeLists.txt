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

# NightlyStart:
#	$(TCLSH) Testing/Utilities/DashboardManager.tcl Nightly Start

NightlyBuild:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Nightly Start Build

NightlyTest:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Nightly Test

NightlyCoverage:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Nightly Coverage

NightlyDashboardStart:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Nightly DashboardStart

NightlyDashboardEnd:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Nightly DashboardEnd


END MAKE VERBATIM

