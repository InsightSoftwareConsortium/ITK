BEGIN MAKE VERBATIM

# Ugly.  Again, this will make your head hurt.  Look away before it
# starts...  For each instance, pick off the last build stamp, and
# return all of the latest build stamps as a list.  The function
# $(words) counts the number of white space separated words in a list,
# similar to llength.  The function $(wordlist) picks the 1-index'd
# words from the white space separated words in a list, similar to
# lindex.

Instances = $(wildcard ../Sites/*/*/)
LatestBuildStamps = $(foreach Instance, $(Instances), \
$(wordlist $(words $(wildcard $(Instance)*)), $(words $(wildcard $(Instance)*)),$(sort $(wildcard $(Instance)*))))

# LatestBuildStamps = $(foreach Instance, $(Instances), $(word 1, $(shell ls $(Instance)*  | sort -r )))
LatestBuildStamps = $(foreach Instance, $(Instances), $(wordlist $(words $(wildcard $(Instance)*)),$(words $(wildcard $(Instance)*)), $(sort $(wildcard $(Instance)*))))


MAKEFILES = $(CMAKE_CONFIG_DIR)/CMake/CMakeVariables.make $(CMAKE_CONFIG_DIR)/Testing/Utilities/CMakeTestingRules.make 
XALAN = $(topdir)/Code/Insight3DParty/xalan/xalan.jar
XMLHEADER = <?xml version="1.0" encoding="UTF-8"?>

Dashboard: Dashboard.html

Dashboard.html: $(LatestBuildStamps)
	@echo "LatestBuildStamps: $(LatestBuildStamps)"
	echo '$(XMLHEADER)' > Dashboard.xml
	echo '$(XMLHEADER)' > DashboardByTest.xml
	echo '<Dashboard>' >> Dashboard.xml
	echo '<Dashboard>' >> DashboardByTest.xml
	$(foreach dir,$(LatestBuildStamps), \
	$(MAKE) -C $(dir) Build.html Test.html BuildSummary.xml TestSummary.xml SummaryByTest.xml; \
	echo '<Instance>' >> Dashboard.xml; \
	grep -v "<?xml" $(dir)/BuildSummary.xml >> Dashboard.xml; \
	grep -v "<?xml" $(dir)/TestSummary.xml >> Dashboard.xml; \
	grep -v "<?xml" $(dir)/SummaryByTest.xml >> DashboardByTest.xml; \
	echo '</Instance>' >> Dashboard.xml; \
  )
	echo '</Dashboard>' >> Dashboard.xml
	echo '</Dashboard>' >> DashboardByTest.xml
	$(JAVA) -jar $(XALAN) -Q -OUT Dashboard.html -IN Dashboard.xml -XSL $(topdir)/Testing/Utilities/XSL/Dashboard.xsl	
	$(JAVA) -jar $(XALAN) -Q -OUT DashboardByTest.html -IN DashboardByTest.xml -XSL $(topdir)/Testing/Utilities/XSL/DashboardByTest.xsl	

END MAKE VERBATIM
