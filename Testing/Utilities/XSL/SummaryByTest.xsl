<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!-- Don't copy these forward -->
<xsl:template match="Log | Results | Measurement | Value | Name" />

<xsl:template match="/">
	<xsl:variable name="SiteName"><xsl:value-of select="Site/@Name"/></xsl:variable>
	<xsl:variable name="BuildName"><xsl:value-of select="Site/@BuildName"/></xsl:variable>
	<xsl:variable name="BuildStamp"><xsl:value-of select="Site/@BuildStamp"/></xsl:variable>
	
	<xsl:for-each select="Site/Testing/Test">
		<Test><xsl:attribute name="Status"><xsl:value-of select="@Status"/></xsl:attribute>
		<xsl:attribute name="Name"><xsl:value-of select="Name"/></xsl:attribute>
		<SiteName><xsl:value-of select="$SiteName"/></SiteName>
		<BuildName><xsl:value-of select="$BuildName"/></BuildName>
		<BuildStamp><xsl:value-of select="$BuildStamp"/></BuildStamp>
		</Test>
	</xsl:for-each>
</xsl:template>

</xsl:stylesheet>