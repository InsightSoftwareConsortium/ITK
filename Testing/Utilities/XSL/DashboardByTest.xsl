<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="/Dashboard">
<html>
<head>
  <title>Dashboard By Test</title>
</head>
<body bgcolor="#ffffff">

<h1>Insight testing dashboard</h1>

<table>
	<tr>
	<th>Test Name</th>
	<th>BuildName</th>
	</tr>

	<xsl:variable name="URLBase">../Sites/<xsl:value-of select="Site/SiteName"/>/<xsl:value-of select="Site/BuildName"/>/<xsl:value-of select="Site/BuildStamp"/></xsl:variable>
	<xsl:for-each select="Test">
	<tr>
		<td>test<xsl:value-of select="@Name"/></td>
		<td>
	<xsl:choose>
		<xsl:when test="contains('failed',@Status)">
			<font color="#FF0000">
				<xsl:value-of select="SiteName"/>-<xsl:value-of select="BuildName"/>
			</font>
		</xsl:when>
		<xsl:when test="contains('passed',@Status)">
			<font color="#00AA00">
				<xsl:value-of select="SiteName"/>-<xsl:value-of select="BuildName"/>
			</font>
		</xsl:when>
	</xsl:choose>
		</td>
	</tr>
	</xsl:for-each>
</table>
</body>
</html>

</xsl:template>
</xsl:stylesheet>