PROJECT(ITK)
INCLUDE_REGULAR_EXPRESSION("^(itk|vtk|vnl|vcl|f2c).*$")
SOURCE_GROUP("XML Files" "\.xml$")

SUBDIRS(Utilities Code Testing Examples)

INCLUDE_DIRECTORIES(
${ITK_BINARY_DIR} 
${ITK_SOURCE_DIR}/Code/Numerics
${ITK_SOURCE_DIR}/Code/Common 
${ITK_BINARY_DIR}/Code/Numerics/vxl 
${ITK_SOURCE_DIR}/Code/Numerics/vxl 
${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl 
${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl 
)

IF(UNIX)
  LINK_LIBRARIES(
  ${THREAD_LIBS} 
  ${DL_LIBS} 
  )
ENDIF(UNIX)

LINK_DIRECTORIES(
${ITK_BINARY_DIR}/Code/Common 
${ITK_BINARY_DIR}/Code/Numerics/vxl )


# look for the make program
IF(NOT UNIX) 
  FIND_PROGRAM(MAKEPROGRAM msdev )
ENDIF(NOT UNIX)
FIND_PROGRAM(MAKEPROGRAM gmake )
FIND_PROGRAM(MAKEPROGRAM make )

FIND_PROGRAM(CVSCOMMAND cvs )

FIND_PROGRAM(GREPCOMMAND grep )

FIND_PROGRAM(COMPRESSIONCOMMAND gzip)
FIND_PROGRAM(COMPRESSIONCOMMAND compress)
FIND_PROGRAM(COMPRESSIONCOMMAND zip )

FIND_PROGRAM(GUNZIPCOMMAND gunzip )
IF (UNIX)
  FIND_PROGRAM(TCLSHCOMMAND cygtclsh80 )
ENDIF(UNIX)
FIND_PROGRAM(TCLSHCOMMAND tclsh )
FIND_PROGRAM(TCLSHCOMMAND tclsh82)
FIND_PROGRAM(TCLSHCOMMAND tclsh8.2)
FIND_PROGRAM(TCLSHCOMMAND tclsh80 )
FIND_PROGRAM(TCLSHCOMMAND tclsh8.2 )
FIND_PROGRAM(HOSTNAME hostname /usr/bsd /usr/sbin /usr/bin /bin /sbin)
FIND_PROGRAM(NSLOOKUP nslookup /usr/bin /usr/sbin /usr/local/bin)

# set the site name
SITE_NAME(SITE)
# set the build name
BUILD_NAME(BUILDNAME)
# set the build command
BUILD_COMMAND(MAKECOMMAND ${MAKEPROGRAM} )
# Add commands to generate configure file when no
# autoconf is around

CONFIGURE_FILE_NOAUTOCONF(
${ITK_SOURCE_DIR}/itkConfigure.h.in 
${ITK_BINARY_DIR}/itkConfigure.h)

CONFIGURE_FILE_NOAUTOCONF(
${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vxl_config.h
${ITK_BINARY_DIR}/Code/Numerics/vxl/vxl_config.h)

CONFIGURE_FILE_NOAUTOCONF(
${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vcl_config_compiler.h
${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl/vcl_config_compiler.h)

CONFIGURE_FILE_NOAUTOCONF(
${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vcl_config_headers.h
${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl/vcl_config_headers.h)

CONFIGURE_FILE_NOAUTOCONF(
${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/config.win32-vc60/vcl_config_manual.h
${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl/vcl_config_manual.h)


CONFIGURE_FILE(
${ITK_SOURCE_DIR}/Testing/Utilities/Build.tcl.in
${ITK_BINARY_DIR}/Testing/Utilities/Build.tcl)

CONFIGURE_FILE(
${ITK_SOURCE_DIR}/Testing/Utilities/Utility.conf.in
${ITK_BINARY_DIR}/Testing/Utilities/Utility.conf)

CONFIGURE_FILE(
${ITK_SOURCE_DIR}/Testing/Utilities/Utility.tcl.in
${ITK_BINARY_DIR}/Testing/Utilities/Utility.tcl)

CONFIGURE_FILE(
${ITK_SOURCE_DIR}/Testing/Utilities/DashboardManager.tcl.in
${ITK_BINARY_DIR}/Testing/Utilities/DashboardManager.tcl)

CONFIGURE_FILE(
${ITK_SOURCE_DIR}/Testing/Utilities/Test.tcl.in
${ITK_BINARY_DIR}/Testing/Utilities/Test.tcl)

CONFIGURE_FILE(
${ITK_SOURCE_DIR}/Testing/Utilities/Submit.tcl.in
${ITK_BINARY_DIR}/Testing/Utilities/Submit.tcl)

CONFIGURE_FILE(
${ITK_SOURCE_DIR}/Testing/Utilities/ftp.tcl.in
${ITK_BINARY_DIR}/Testing/Utilities/ftp.tcl)

CONFIGURE_FILE(
${ITK_SOURCE_DIR}/Testing/Utilities/Doxygen.tcl.in
${ITK_BINARY_DIR}/Testing/Utilities/Doxygen.tcl)


ADD_CUSTOM_TARGET(Nightly 
"${TCLSHCOMMAND} Testing/Utilities/DashboardManager.tcl Nightly Start")
ADD_CUSTOM_TARGET(NightlyBuild 
"${TCLSHCOMMAND} Testing/Utilities/DashboardManager.tcl Nightly Start Build")
ADD_CUSTOM_TARGET(NightlyTest 
"${TCLSHCOMMAND} Testing/Utilities/DashboardManager.tcl Nightly Test")
ADD_CUSTOM_TARGET(NightlyCoverage 
"${TCLSHCOMMAND} Testing/Utilities/DashboardManager.tcl Nightly Coverage")
ADD_CUSTOM_TARGET(NightlySubmit 
"${TCLSHCOMMAND} Testing/Utilities/DashboardManager.tcl Nightly Submit")
ADD_CUSTOM_TARGET(NightlyDashboardStart 
"${TCLSHCOMMAND} Testing/Utilities/DashboardManager.tcl Nightly DashboardStart")
ADD_CUSTOM_TARGET(NightlyDashboardEnd 
"${TCLSHCOMMAND} Testing/Utilities/DashboardManager.tcl Nightly DashboardEnd")
ADD_CUSTOM_TARGET(ExperimentalBuild 
"${TCLSHCOMMAND} Testing/Utilities/DashboardManager.tcl Experimental Start Build")
ADD_CUSTOM_TARGET(ExperimentalTest 
"${TCLSHCOMMAND} Testing/Utilities/DashboardManager.tcl Experimental Test")
ADD_CUSTOM_TARGET(ExperimentalCoverage 
"${TCLSHCOMMAND} Testing/Utilities/DashboardManager.tcl Experimental Coverage")
ADD_CUSTOM_TARGET(ExperimentalSubmit 
"${TCLSHCOMMAND} Testing/Utilities/DashboardManager.tcl Experimental Submit")
ADD_CUSTOM_TARGET(Experimental 
"${TCLSHCOMMAND} Testing/Utilities/DashboardManager.tcl Experimental Start Build Test Submit")


# Find the CABLE utility.
UTILITY_SOURCE(CABLE cable Utilities/Cable/Executables)
FIND_PROGRAM(CABLE cable)

