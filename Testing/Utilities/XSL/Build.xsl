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
  <xsl:variable name="DashboardDir" select="concat('../../../../Dashboard/', $DashboardStamp)"/>
  <xsl:param name="TestDocDir">.</xsl:param>

  <xsl:include href="Insight.xsl"/>

  <xsl:template match="/">
    <xsl:call-template name="Summary"/>

    <!--
         Write both the Errors and Warnings
         -->
      <xsl:call-template name="ErrorLog"/>
    <redirect:write select="concat(string('{$TestDocDir}'), '/BuildWarning.html' )">
      <xsl:call-template name="WarningLog"/>
    </redirect:write>

  </xsl:template>

  <xsl:template name="ErrorLog">
    <redirect:write select="concat(string('{$TestDocDir}'), '/BuildError.html' )">
      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Insight Errors</xsl:with-param>
        <xsl:with-param name="IconDir">../../../../Icons</xsl:with-param>
      </xsl:call-template>
      Found <xsl:value-of select="count(Site/Build/Error)"/> Errors<br/>
      <xsl:for-each select="Site/Build/Error">
        <xsl:call-template name="FormatContext"/>
      </xsl:for-each>
      <xsl:call-template name="InsightFooter"/>
    </redirect:write>
  </xsl:template>

  <xsl:template name="WarningLog">
    <redirect:write select="concat(string('{$TestDocDir}'), '/BuildWarning.html' )">
      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Insight Warnings</xsl:with-param>
        <xsl:with-param name="IconDir">../../../../Icons</xsl:with-param>
      </xsl:call-template>
      Found <xsl:value-of select="count(Site/Build/Warning)"/> Warnings<br/>
      <xsl:for-each select="Site/Build/Warning">
        <xsl:call-template name="FormatContext"/>
      </xsl:for-each>
      <xsl:call-template name="InsightFooter"/>
    </redirect:write>
  </xsl:template>

  <xsl:template name="FormatContext">
  <hr/>
  <h3>Build Log line <xsl:value-of select="BuildLogLine"/></h3>
  <br/>
  <xsl:choose>
    <xsl:when test="SourceFile != ''">
      File: 
      <b><xsl:value-of select="SourceFile"/></b>
      Line: 
      <b><xsl:value-of select="SourceLineNumber"/></b>
      <a>
        <xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:value-of select="SourceFile"/></xsl:attribute>
        CVS Access
      </a>
      <xsl:if test="SourceFileTail != ''">
        <a>
          <xsl:attribute name="href"><xsl:value-of select="$DoxygenURL"/><xsl:value-of select="SourceFileTail"/>-source.html</xsl:attribute>
        Doxygen
        </a>
      </xsl:if>
    </h5>
        
    </xsl:when>
  </xsl:choose>
  <pre><xsl:value-of select="PreContext" disable-output-escaping="yes"/><b><xsl:value-of select="Text" disable-output-escaping="yes"/></b>
<xsl:value-of select="PostContext" disable-output-escaping="yes"/></pre>
</xsl:template>


<xsl:template name="Summary">
  <redirect:write select="concat(string('{$TestDocDir}'), '/BuildSummary.xml' )">

    <Build>
      <SiteName><xsl:value-of select="Site/@Name"/></SiteName>
      <BuildName><xsl:value-of select="Site/@BuildName"/></BuildName>
      <BuildStamp><xsl:value-of select="Site/@BuildStamp"/></BuildStamp>
      <StartDateTime><xsl:value-of select="Site/Build/StartDateTime"/></StartDateTime>

      <WarningCount><xsl:value-of select="count(Site/Build/Warning)"/></WarningCount>
      <ErrorCount><xsl:value-of select="count(Site/Build/Error)"/></ErrorCount>
      <EndDateTime><xsl:value-of select="Site/Build/EndDateTime"/></EndDateTime>
    </Build>
  </redirect:write>
</xsl:template>

</xsl:stylesheet>