PROJECT(ITK)

SUBDIRS(Code Testing Examples)

INCLUDE_DIRECTORIES(
${CMAKE_BINARY_DIR} 
${CMAKE_BINARY_DIR}/Code/Insight3DParty/vxl 
${CMAKE_SOURCE_DIR}/Code/Common 
${CMAKE_SOURCE_DIR}/Code/Insight3DParty/vxl 
)

LINK_DIRECTORIES(
${CMAKE_BINARY_DIR}/Code/Common 
${CMAKE_BINARY_DIR}/Code/Insight3DParty/vxl )

LINK_LIBRARIES (
ITKCommon 
ITKNumerics
)

UNIX_LIBRARIES(
${THREAD_LIBS} 
${DL_LIBS} 
-lm
)


# Add commands to generate configure file when no
# autoconf is around

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/itkConfigure.h.in 
${CMAKE_BINARY_DIR}/itkConfigure.h)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Code/Insight3DParty/vxl/vcl/vcl_config-vc60.h
${CMAKE_BINARY_DIR}/Code/Insight3DParty/vxl/vcl/vcl_config.h)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Testing/Utilities/Build.tcl.in
${CMAKE_BINARY_DIR}/Testing/Utilities/Build.tcl)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Testing/Utilities/Utility.conf.in
${CMAKE_BINARY_DIR}/Testing/Utilities/Utility.conf)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Testing/Utilities/Utility.tcl.in
${CMAKE_BINARY_DIR}/Testing/Utilities/Utility.tcl)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Testing/Utilities/DashboardManager.tcl.in
${CMAKE_BINARY_DIR}/Testing/Utilities/DashboardManager.tcl)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Testing/Utilities/Test.tcl.in
${CMAKE_BINARY_DIR}/Testing/Utilities/Test.tcl)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Testing/Utilities/Submit.tcl.in
${CMAKE_BINARY_DIR}/Testing/Utilities/Submit.tcl)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Testing/Utilities/ftp.tcl.in
${CMAKE_BINARY_DIR}/Testing/Utilities/ftp.tcl)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Testing/Utilities/Doxygen.tcl.in
${CMAKE_BINARY_DIR}/Testing/Utilities/Doxygen.tcl)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Testing/Utilities/WindowsUtility.conf.in
${CMAKE_BINARY_DIR}/Testing/Utilities/WindowsUtility.conf)


BEGIN MAKE VERBATIM

# NightlyStart:
#	$(TCLSH) Testing/Utilities/DashboardManager.tcl Nightly Start

Nightly:
	$(TCLSH) Testing/Utilities/DashboardManager.tcl Nightly Start Build Test Submit

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

