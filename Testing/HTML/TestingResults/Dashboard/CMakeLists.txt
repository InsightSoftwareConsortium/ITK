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
NightlyDashboardStamp := $(shell date -u +%Y%m%d)-Nightly

Dashboards = $(sort $(wildcard 20*))
LatestDashboard = $(wordlist $(words $(Dashboards)),$(words $(Dashboards)), $(Dashboards))



MAKEFILES = $(CMAKE_CONFIG_DIR)/CMake/CMakeVariables.make $(CMAKE_CONFIG_DIR)/Testing/Utilities/CMakeTestingDashboardRules.make 

StartNightlyDashboard: 
	mkdir $(NightlyDashboardStamp)

NightlyDashboardUpdate:
	$(shell cd $(NightlyDashboardStamp); (cd $(topdir); $(TCLSH) Testing/Utilities/Update.tcl) > Update.xml )

EndNightlyDashboard:
	echo 'BuildStamps = $(foreach stamp, $(LatestBuildStamps), ../$(stamp))' > $(NightlyDashboardStamp)/DashboardTargets.make
	$(MAKE) -C $(NightlyDashboardStamp) EndDashboard
	$(shell rm -f MostRecentResults)
	$(shell ln -s $(NightlyDashboardStamp) MostRecentResults)



EndDashboard:
	echo 'BuildStamps = $(foreach stamp, $(LatestBuildStamps), ../$(stamp))' > $(LatestDashboard)/DashboardTargets.make
	@echo "Latest Dashboard: $(LatestDashboard)"
	$(MAKE) -C $(LatestDashboard) EndDashboard
	$(shell rm -f MostRecentResults)
	$(shell ln -s $(DashboardStamp) MostRecentResults)


DashboardUpdate:
	$(shell cd $(LatestDashboard); (cd $(topdir); $(TCLSH) Testing/Utilities/Update.tcl) > Update.xml )


Dashboard: $(LatestBuildStamps)
	mkdir $(DashboardStamp)
	echo 'BuildStamps = $(foreach stamp, $(LatestBuildStamps), ../$(stamp))' > $(DashboardStamp)/DashboardTargets.make
	$(MAKE) -C $(DashboardStamp) EndDashboard

RemakeDashboard:
	$(foreach Previous, $(PreviousDashboards), \
	$(MAKE) -C $(Previous) RemakeDashboard; \
	)


END MAKE VERBATIM
