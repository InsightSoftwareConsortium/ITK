<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.0"
    xmlns:lxslt="http://xml.apache.org/xslt"
    xmlns:redirect="org.apache.xalan.lib.Redirect"
    extension-element-prefixes="redirect">

  <!--
       Use DashboardStamp as a parameter, default to most recent
       The proper flags to Xalan are in the form -PARAM DashboardStamp "string('foo')"
       -->
  <xsl:param name="DashboardStamp" select="string('MostRecentResults-Nightly')"/>
  <xsl:variable name="DashboardDir" select="concat('../../Dashboard/', $DashboardStamp)"/>
  <xsl:param name="TestDocDir">.</xsl:param>
  <xsl:include href="Insight.xsl"/>

  <xsl:template match="/">
    <xsl:call-template name="Summary" select="Doxygen"/>
    <xsl:call-template name="InsightHeader">
      <xsl:with-param name="Title">Doxygen log</xsl:with-param>
      <xsl:with-param name="IconDir">../../Icons</xsl:with-param>
    </xsl:call-template>

    <h2>Doxygen started on <xsl:value-of select="Doxygen/StartDateTime"/></h2>
    <h3>
      Found 
      <a href="#Error">
        <xsl:value-of select="count(Doxygen/Error)"/> Errors
      </a>
      and 
      <a href="#Warning">
        <xsl:value-of select="count(Doxygen/Warning)"/> Warnings
      </a>
    </h3>
    <br/>
    <hr/>
    <a name="Error">
      <h2>Errors</h2>
    </a>
    <xsl:for-each select="Doxygen/Error">
      <hr/>
      <h3>Error # <xsl:number level="single" count="Doxygen/Error" format="1"/>: Build Log line <xsl:value-of select="LogLine"/></h3>
      <br/>
      <xsl:call-template name="FormatContext"/>
    </xsl:for-each>
    
    <hr/>
    <a name="Warning">
      <h2>Warnings</h2>
    </a>
    <xsl:for-each select="Doxygen/Warning">
      <hr/>
      <h3>Warning # <xsl:number level="single" count="Doxygen/Warning" format="1"/>: Build Log line <xsl:value-of select="LogLine"/></h3>
      <br/>
      <xsl:call-template name="FormatContext"/>
    </xsl:for-each>
    <xsl:call-template name="InsightFooter"/>

  </xsl:template>
	

  <xsl:template name="FormatContext">
    <xsl:choose>
      <xsl:when test="SourceFile != ''">
        File: 
        <b><xsl:value-of select="SourceFile"/></b>
        Line: 
        <b><xsl:value-of select="SourceLineNumber"/></b>
      </xsl:when>
    </xsl:choose>
    <pre>
      <xsl:value-of select="PreContext"/>
      <b><xsl:value-of select="Text"/></b>
      <xsl:value-of select="PostContext"/>
    </pre>
  </xsl:template>

  <xsl:template name="Summary">
    <redirect:write select="concat(string('{$TestDocDir}'), '/DoxygenSummary.xml' )">
      <Doxygen>
        <StartDateTime><xsl:value-of select="/Doxygen/StartDateTime"/></StartDateTime>
        <ErrorCount><xsl:value-of select="count(/Doxygen/Error)"/></ErrorCount>
        <WarningCount><xsl:value-of select="count(/Doxygen/Warning)"/></WarningCount>
      </Doxygen>
    </redirect:write>
  </xsl:template>

</xsl:stylesheet>