<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="/Dashboard">
<html>
<head>
  <title>Insight Dashboard - Most Recent Results</title>
</head>
<body bgcolor="#ffffff">

<h1>Insight testing dashboard</h1>

<table>
	<tr>
	<th>Site</th>
	<th>Instance</th>
	<th>Build Errors</th>
	<th>Build Warnings</th>
	<th>Passed</th>
	<th>Failed</th>
	</tr>



	


	<xsl:for-each select="Instance">
	
	<xsl:variable name="URLBase">
		../Sites/<xsl:value-of select="Site/SiteName"/>/<xsl:value-of select="Site/BuildName"/>/<xsl:value-of select="Site/BuildStamp"/>
	</xsl:variable>
	<tr>
	<td><xsl:value-of select="Site/SiteName"/></td>
	<td><xsl:value-of select="Site/BuildName"/></td>
	<td>
	<a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Build.html</xsl:attribute>

	<xsl:value-of select="Site/Build/ErrorCount"/>
	</a></td>
	<td>
		<a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Build.html</xsl:attribute>

	<xsl:value-of select="Site/Build/WarningCount"/>
	</a>
	</td>
	<td>
		<a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html</xsl:attribute>
		<xsl:value-of select="Site/Testing/PassedCount"/>
	 	</a>
	</td>
	<td>
		<a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html</xsl:attribute>
		<xsl:value-of select="count(Site/Testing/FailedCount)"/>
		</a>
	</td>
	</tr>
	</xsl:for-each>

</table>
</body>
</html>

</xsl:template>
</xsl:stylesheet>