<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <!--
       Use DashboardStamp as a parameter, default to most recent
       The proper flags to Xalan are in the form -PARAM DashboardStamp "string('foo')"
       -->
  <xsl:param name="DashboardStamp" select="string('MostRecentResults-Nightly')"/>
  <xsl:variable name="DashboardDir" select="concat('../../../../Dashboard/', $DashboardStamp)"/>
  <xsl:param name="Type" select="Error"/>

  <xsl:include href="Insight.xsl"/>

    <xsl:template match="/">
      <xsl:choose>
        <xsl:when test="contains($Type, 'Error')">
          <xsl:call-template name="InsightHeader">
            <xsl:with-param name="Title">Insight Errors</xsl:with-param>
            <xsl:with-param name="IconDir">../../../../Icons</xsl:with-param>
          </xsl:call-template>
        </xsl:when>
        <xsl:when test="contains($Type, 'Warning')">
          <xsl:call-template name="InsightHeader">
            <xsl:with-param name="Title">Insight Warnings</xsl:with-param>
            <xsl:with-param name="IconDir">../../../../Icons</xsl:with-param>
          </xsl:call-template>
        </xsl:when>
      </xsl:choose>

    <h2>Build started on <xsl:value-of select="Site/Build/StartDateTime"/></h2>
    <h3>
      <xsl:choose>
        <xsl:when test="contains($Type, 'Error')">
          Found <xsl:value-of select="count(Site/Build/Error)"/> Errors
        </xsl:when>
        <xsl:when test="contains($Type, 'Warning')">
          Found <xsl:value-of select="count(Site/Build/Warning)"/> Warnings
        </xsl:when>
      </xsl:choose>
    </h3>
    <br/>
    <hr/>
    <xsl:choose>
      <xsl:when test="contains($Type, 'Error')">
        <xsl:for-each select="Site/Build/Error">
          <hr/>
          <h3>Error: Build Log line <xsl:value-of select="BuildLogLine"/></h3>
          <br/>
          <xsl:call-template name="FormatContext"/>
        </xsl:for-each>
      </xsl:when>
      <xsl:when test="contains($Type, 'Warning')">
        <xsl:for-each select="Site/Build/Warning">
          <hr/>
          <h3>Error: Build Log line <xsl:value-of select="BuildLogLine"/></h3>
          <br/>
          <xsl:call-template name="FormatContext"/>
        </xsl:for-each>
      </xsl:when>
    </xsl:choose>
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

</xsl:stylesheet>