<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="/Site">
<xsl:copy>
	<SiteName><xsl:value-of select="@Name"/></SiteName>
	<BuildName><xsl:value-of select="@BuildName"/></BuildName>
	<BuildStamp><xsl:value-of select="@BuildStamp"/></BuildStamp>

	<Coverage>
	<StartDateTime><xsl:value-of select="Coverage/StartDateTime"/></StartDateTime>
	<PercentCoverage><xsl:value-of select="Coverage/PercentCoverage"/>
</PercentCoverage>
	<LOCTested><xsl:value-of select="Coverage/LOCTested"/></LOCTested>
	<LOCUntested><xsl:value-of select="Coverage/LOCUntested"/></LOCUntested>
	<LOC><xsl:value-of select="Coverage/LOC"/></LOC>
	</Coverage>
</xsl:copy>
</xsl:template>

<!-- Don't match these things... -->
<xsl:template match="Directory|File|LOCTested|LOCUntested|StartDateTime|EndDateTime|PercentCoverage"/>

</xsl:stylesheet>