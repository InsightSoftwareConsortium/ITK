<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<!-- Classpath
setenv CLASSPATH /home/blezek/src/FrostInstall/xalan_1_2_D01/xalan.jar:/home/blezek/src/FrostInstall/xalan_1_2_D01/xerces.jar:.


java org.apache.xalan.xslt.Process -IN Build.xml -OUT Build.html -XSL /home/blezek/src/Insight/Testing/Utilities/XSL/Build.xsl
-->

<xsl:template match="/">
<html>
<head>
  <title>Build log</title>
</head>
<body bgcolor="#ffffff">
	<h2>Build started on <xsl:value-of select="///Build/StartDateTime"/></h2>
	<h3>Found <xsl:value-of select="count(//Build/Error)"/> Errors and <xsl:value-of select="count(//Build/Warning)"/> Warnings</h3><br/>


	<!-- Loop over Errors -->
	<xsl:for-each select="//Build/Error">
		<h3>Error</h3> Build Log line <xsl:value-of select="BuildLogLine"/><br/>
		<pre><xsl:value-of select="PreContext"/></pre>
		<strong><xsl:value-of select="Text"/></strong><br/>
		<pre><xsl:value-of select="PostContext"/></pre>
	</xsl:for-each>

	<!-- Loop over Warnings -->
	<xsl:for-each select="//Build/Warning">
		<h3>Warning</h3> Build Log line <xsl:value-of select="BuildLogLine"/><br/>
		<pre><xsl:value-of select="PreContext"/></pre>
		<strong><xsl:value-of select="Text"/></strong><br/>
		<pre><xsl:value-of select="PostContext"/></pre>
	</xsl:for-each>

</body>
</html>
</xsl:template>
	
<!-- Match Log, StartDateTime, and EndDateTime to keep them from appearing in the output -->
<xsl:template match="/Build/Log | /Build/StartDateTime | /Build/EndDateTime"/>
<xsl:template match="/Build/Error | /Build/Warning"/>

</xsl:stylesheet>