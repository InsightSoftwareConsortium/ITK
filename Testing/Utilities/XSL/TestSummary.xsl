<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!-- Don't copy these forward -->
<xsl:template match="Log | Results | Measurement | Value | Name" />

<xsl:template match="Testing">
	<xsl:copy>
	<PassedCount><xsl:value-of select="count(Test[@Status='passed'])"/></PassedCount>
	<FailedCount><xsl:value-of select="count(Test[@Status='failed'])"/></FailedCount>
	<xsl:apply-templates />
	</xsl:copy>
</xsl:template>

<xsl:template match="Site">
	<xsl:copy>
	<SiteName><xsl:value-of select="@Name"/></SiteName>
	<BuildName><xsl:value-of select="@BuildName"/></BuildName>
	<BuildStamp><xsl:value-of select="@BuildStamp"/></BuildStamp>
	<xsl:apply-templates />
	</xsl:copy>
</xsl:template>


<xsl:template match="StartDateTime | EndDateTime">
	<xsl:copy>
	<xsl:apply-templates />
	</xsl:copy>
</xsl:template>


<xsl:template match="/">
	<xsl:copy>
	<xsl:apply-templates />
	</xsl:copy>
</xsl:template>
	

</xsl:stylesheet>