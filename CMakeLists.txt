PROJECT(ITK)

SUBDIRS(Utilities Code Testing Examples)

INCLUDE_DIRECTORIES(
${CMAKE_BINARY_DIR} 
${CMAKE_SOURCE_DIR}/Code/Numerics
${CMAKE_SOURCE_DIR}/Code/Common 
${CMAKE_BINARY_DIR}/Code/Numerics/vxl 
${CMAKE_SOURCE_DIR}/Code/Numerics/vxl 
${CMAKE_BINARY_DIR}/Code/Numerics/vxl/vcl 
${CMAKE_SOURCE_DIR}/Code/Numerics/vxl/vcl 
)

UNIX_LIBRARIES(
${THREAD_LIBS} 
${DL_LIBS} 
-lm
)

LINK_DIRECTORIES(
${CMAKE_BINARY_DIR}/Code/Common 
${CMAKE_BINARY_DIR}/Code/Numerics/vxl )


# Add commands to generate configure file when no
# autoconf is around

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/itkConfigure.h.in 
${CMAKE_BINARY_DIR}/itkConfigure.h)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vxl_config.h
${CMAKE_BINARY_DIR}/Code/Numerics/vxl/vxl_config.h)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vcl_config_compiler.h
${CMAKE_BINARY_DIR}/Code/Numerics/vxl/vcl/vcl_config_compiler.h)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vcl_config_headers.h
${CMAKE_BINARY_DIR}/Code/Numerics/vxl/vcl/vcl_config_headers.h)

CONFIGURE_FILE_NOAUTOCONF(
${CMAKE_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vcl_config_manual.h
${CMAKE_BINARY_DIR}/Code/Numerics/vxl/vcl/vcl_config_manual.h)

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

