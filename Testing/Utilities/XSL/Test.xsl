<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:template match="/">
<html>
<head>
  <title>Test log</title>
</head>
<body bgcolor="#ffffff">
	<h2>Testing started on <xsl:value-of select="//Testing/StartDateTime"/></h2>
	<h3><xsl:value-of select="count(//Testing/Test[@Status='passed'])"/> passed, <xsl:value-of select="count(//Testing/Test[@Status='failed'])"/> failed </h3><br/>

	<ul>
	<xsl:for-each select="//Testing/Test">
	<li>
	<a><xsl:attribute name="HREF">#<xsl:value-of select="generate-id()"/></xsl:attribute><xsl:value-of select="Name"/></a>
	<xsl:choose>
		<xsl:when test="contains('failed',@Status)">
			<font color="#FF0000"> Failed</font>
		</xsl:when>
		<xsl:when test="contains('passed',@Status)">
			<font color="#00AA00"> Passed</font>
		</xsl:when>
	</xsl:choose>
	</li>

	</xsl:for-each>
	</ul>
	<hr/>
	
	<xsl:for-each select="//Testing/Test">
	<!-- Add the anchor -->
	<a><xsl:attribute name="NAME"><xsl:value-of select="generate-id()"/></xsl:attribute>
		<xsl:value-of select="Name"/>
	</a>
	<xsl:choose>
		<xsl:when test="contains('failed',@Status)">
			<font color="#FF0000"> Failed</font>
		</xsl:when>
		<xsl:when test="contains('passed',@Status)">
			<font color="#00AA00"> Passed</font>
		</xsl:when>
	</xsl:choose>
	
	<pre>
		<xsl:value-of select="Results/Measurement/Value"/>
	</pre>
	<hr/>
	</xsl:for-each>

	<!-- <xsl:apply-templates/> -->
</body>
</html>
</xsl:template>
	

</xsl:stylesheet>
