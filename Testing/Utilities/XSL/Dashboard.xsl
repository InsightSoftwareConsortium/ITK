<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="/Dashboard">
<html>
<head>
  <title>Build log</title>
</head>
<body bgcolor="#ffffff">

<h1>Insight testing dashboard</h1>

<table>
	<tr>
	<th>Site</th>
	<th>Instance</th>
	<th>Build Warnings</th>
	<th>Build Errors</th>
	<th>Passed</th>
	<th>Failed</th>
	</tr>



	


	<xsl:for-each select="Instance">
	
	<xsl:variable name="URLBase">
		../Sites/<xsl:value-of select="Site/@Name"/>/<xsl:value-of select="Site/@BuildName"/>/<xsl:value-of select="Site/@BuildStamp"/>
	</xsl:variable>
	<tr>
	<td><xsl:value-of select="Site/@Name"/></td>
	<td><xsl:value-of select="Site/@BuildName"/></td>
	<td>
	<a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Build.html</xsl:attribute>

	<xsl:value-of select="count(Site/Build/Error)"/>
	</a></td>
	<td>
		<a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Build.html</xsl:attribute>

	<xsl:value-of select="count(Site/Build/Warning)"/>
	</a>
	</td>
	<td>
		<a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html</xsl:attribute>
		<xsl:value-of select="count(Site/Testing/Test[@Status='passed'])"/>
	 	</a>
	</td>
	<td>
		<a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html</xsl:attribute>
		<xsl:value-of select="count(Site/Testing/Test[@Status='failed'])"/>
		</a>
	</td>
	</tr>
	</xsl:for-each>

</table>
</body>
</html>

</xsl:template>
</xsl:stylesheet>