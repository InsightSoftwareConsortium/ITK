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

CompleteDashboard:
	$(MAKE) DashboardBuild
	$(MAKE) DashboardTest
	$(MAKE) DashboardCoverage
	$(MAKE) Dashboard

Dashboard:
	$(MAKE) -C Testing/HTML/TestingResults/Dashboard Dashboard

RemakeDashboard:
	$(MAKE) -C Testing/HTML/TestingResults/Dashboard RemakeDashboard

DashboardBuild:
	$(TCLSH) Testing/Utilities/Build.tcl

DashboardTest:
	$(TCLSH) Testing/Utilities/Test.tcl

DashboardCoverage:
	$(TCLSH) Testing/Utilities/Coverage.tcl

DashboardSubmission:
	$(TCLSH) Testing/Utilities/Submit.tcl

END MAKE VERBATIM

