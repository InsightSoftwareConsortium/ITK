<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!-- Don't copy these forward -->
<xsl:template match="Log" />
<xsl:template match="PreContext" />
<xsl:template match="PostContext" />
<xsl:template match="Text" />
<xsl:template match="BuildLogLine" />

<xsl:template match="Build">
	<xsl:copy>
	<WarningCount><xsl:value-of select="count(Warning)"/></WarningCount>
	<ErrorCount><xsl:value-of select="count(Error)"/></ErrorCount>
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