PROJECT=ITK

SUBDIRS = \
Code \
Testing 

INCLUDE_DIRECTORIES = \
${CMAKE_BINARY_DIR} \
${CMAKE_BINARY_DIR}/Code/Insight3DParty/vxl \
${CMAKE_SOURCE_DIR}/Code/Common \
${CMAKE_SOURCE_DIR}/Code/Insight3DParty/vxl 

LINK_DIRECTORIES = \
${CMAKE_BINARY_DIR}/Code/Common \
${CMAKE_BINARY_DIR}/Code/Insight3DParty/vxl 

LINK_LIBRARIES = \
ITKCommon \
ITKNumerics

WIN32_LIBRARIES = 

UNIX_LIBRARIES = \
${THREAD_LIBS} \
${DL_LIBS} \
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

NightlySubmit:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Nightly Submit

NightlyDashboardStart:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Nightly DashboardStart

NightlyDashboardEnd:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Nightly DashboardEnd


ExperimentalBuild:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Experimental Start Build

ExperimentalTest:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Experimental Test

ExperimentalCoverage:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Experimental Coverage

ExperimentalSubmit:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Experimental Submit

Experimental:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Experimental Start Build Test Submit

END MAKE VERBATIM

