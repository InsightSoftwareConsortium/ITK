BEGIN MAKE VERBATIM

# Ugly.  Again, this will make your head hurt.  Look away before it
# starts...  For each instance, pick off the last build stamp, and
# return all of the latest build stamps as a list.  The function
# $(words) counts the number of white space separated words in a list,
# similar to llength.  The function $(wordlist) picks the 1-index'd
# words from the white space separated words in a list, similar to
# lindex.

BuildNames = $(wildcard ../Sites/*/*/)

LatestBuildStamps = $(foreach BuildName, $(BuildNames), $(wordlist $(words $(wildcard $(BuildName)*)),$(words $(wildcard $(BuildName)*)), $(sort $(wildcard $(BuildName)*))))

DashboardStamp := $(shell date -u +%Y%m%d%H%M)
PreviousDashboards = $(wildcard 20*)


MAKEFILES = $(CMAKE_CONFIG_DIR)/CMake/CMakeVariables.make $(CMAKE_CONFIG_DIR)/Testing/Utilities/CMakeTestingDashboardRules.make 

Dashboard: $(LatestBuildStamps)
	mkdir $(DashboardStamp)
	echo 'BuildStamps = $(foreach stamp, $(LatestBuildStamps), ../$(stamp))' > $(DashboardStamp)/DashboardTargets.make
	$(MAKE) -C $(DashboardStamp) Dashboard
	$(shell rm -f MostRecentResults)
	$(shell ln -s $(DashboardStamp) MostRecentResults)

RemakeDashboard:
	$(foreach Previous, $(PreviousDashboards), \
	$(MAKE) -C $(Previous) RemakeDashboard; \
	)


END MAKE VERBATIM
