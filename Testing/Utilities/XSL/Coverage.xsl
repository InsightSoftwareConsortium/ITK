<?xml version="1.0" encoding="ISO-8859-1"?>
<!DOCTYPE xsl:stylesheet>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:variable name="TOC" select="1"/>

<xsl:template match="/">
<html>
<head>
  <title>Coverage log</title>
</head>
<body bgcolor="#ffffff">
	<h2>Coverage started on <xsl:value-of select="Site/Coverage/StartDateTime"/></h2>
	<h3><xsl:value-of select="Site/Coverage/PercentCoverage"/>% Coverage: <xsl:value-of select="Site/Coverage/LOCTested"/> Tested lines of <xsl:value-of select="Site/Coverage/LOCUntested"/> of <xsl:value-of select="Site/Coverage/LOC"/> Total Lines of Code</h3>
	<hr/>

<xsl:variable name="TOC" select="1"/>


	<table>
	<tr><th>Filename</th>
	    <th>Lines Covered</th>
	    <th>Lines Not Covered</th>
	    <th>Percentage</th>
	    <th>Date</th>
	</tr>

	<xsl:apply-templates/>
	</table>
</body>
</html>
</xsl:template>

<xsl:template match="Directory">

	<tr><th><xsl:value-of select="@FullPath"/></th></tr>
	<xsl:if test="count(File) > 0">
	<xsl:for-each select="File">
	<xsl:sort select="@Covered"/>
		<xsl:choose>
			<xsl:when test="@Covered='true'">
			<tr><td><xsl:value-of select="@Name"/></td>
                            <td><xsl:value-of select="LOCTested"/></td>
			    <td><xsl:value-of select="LOCUntested"/></td>
			    <td><xsl:value-of select="PercentCoverage"/>%</td>
			</tr>
			</xsl:when>
		</xsl:choose>
	</xsl:for-each>
	</xsl:if>
	<xsl:apply-templates/>

</xsl:template>

<!-- Don't match these things... -->
<xsl:template match="File|LOCTested|LOCUntested|StartDateTime|EndDateTime|PercentCoverage"/>

</xsl:stylesheet>