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
  <xsl:preserve-space elements="xsl:text"/>
  <xsl:include href="Insight.xsl"/>
  <xsl:output method="html"/>
  <xsl:template match="/">
    <xsl:call-template name="Summary"/>

    <!--
         Write both the Errors and Warnings
         -->
    <redirect:write select="concat(string('{$TestDocDir}'), '/BuildError.html' )">
      <xsl:call-template name="ErrorLog"/>
    </redirect:write>
    <redirect:write select="concat(string('{$TestDocDir}'), '/BuildWarning.html' )">
      <xsl:call-template name="WarningLog"/>
    </redirect:write>

  </xsl:template>

  <xsl:template name="ErrorLog">
      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Insight Errors</xsl:with-param>
        <xsl:with-param name="IconDir">../../../../Icons</xsl:with-param>
        <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
      </xsl:call-template>
     <p><b><xsl:text>Site:</xsl:text></b><xsl:value-of select="Site/@Name"/></p><p>
<b><xsl:text>Build Name:</xsl:text></b><xsl:value-of select="Site/@BuildName"/></p> 
Found <xsl:value-of select="count(Site/Build/Error)"/> Errors<br/>
<p><a href="BuildWarning.html">Warnings</a> are here.</p>
      <xsl:for-each select="Site/Build/Error">
        <xsl:call-template name="FormatContext"/>
      </xsl:for-each>
      <xsl:call-template name="InsightFooter"/>
  </xsl:template>

  <xsl:template name="WarningLog">
    <redirect:write select="concat(string('{$TestDocDir}'), '/BuildWarning.html' )">
      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Insight Warnings</xsl:with-param>
        <xsl:with-param name="IconDir">../../../../Icons</xsl:with-param>
        <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
      </xsl:call-template>
    <p><b><xsl:text>Site:</xsl:text></b><xsl:value-of select="Site/@Name"/></p><p>
<b><xsl:text>Build Name:</xsl:text></b><xsl:value-of select="Site/@BuildName"/></p>       
 Found <xsl:value-of select="count(Site/Build/Warning)"/> Warnings<br/>
<p><a href="BuildError.html">Errors</a> are here.</p>
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
      <b>
        <xsl:value-of select="SourceFile"/>
      </b>
      Line: 
      <b>
        <xsl:value-of select="SourceLineNumber"/>
      </b>
      <a>
        <xsl:attribute name="href">
          <xsl:value-of select="$CVSWebURL"/><xsl:value-of select="SourceFile"/>?annotate=HEAD
        </xsl:attribute>
        CVS
		  </a><xsl:text>     and            </xsl:text>
      <xsl:if test="SourceFileTail != ''">
        <a>
          <xsl:attribute name="href"><xsl:value-of select="$DoxygenURL"/><xsl:value-of select="translate ( SourceFileTail, '.', '_' )"/>-source.html</xsl:attribute>
        Doxygen
        </a><xsl:text> access </xsl:text>
      </xsl:if>
    </xsl:when>
  </xsl:choose>
  <pre><xsl:value-of select="PreContext"/><b><xsl:value-of select="Text"/></b>
<xsl:value-of select="PostContext"/></pre>
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