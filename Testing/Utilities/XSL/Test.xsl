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
  <xsl:param name="TestDocDir">.</xsl:param>
  <xsl:variable name="DashboardDir" select="concat('../../../../Dashboard/', $DashboardStamp)"/>
  <xsl:include href="Insight.xsl"/>

  <xsl:template match="/">
    <xsl:call-template name="Summary"/>
    <xsl:call-template name="InsightHeader">
      <xsl:with-param name="Title">Test Log</xsl:with-param>
      <xsl:with-param name="IconDir">../../../../Icons</xsl:with-param>
      <xsl:with-param name="DashboardDir">../../../../Dashboard/</xsl:with-param>
    </xsl:call-template>
    <h2>Testing started on <xsl:value-of select="Site/Testing/StartDateTime"/></h2>
    <h3>
      <a href="#Passed">
        <xsl:value-of select="count(Site/Testing/Test[@Status='passed'])"/>
      </a> passed, 
      <a href="#Failed">
        <xsl:value-of select="count(Site/Testing/Test[@Status='failed'])"/>
      </a> failed,
      <a href="#NotRun">
        <xsl:value-of select="count(Site/Testing/Test[@Status='notrun'])"/>
      </a> not run
    </h3>
    <br/>
    <ul>
      <xsl:for-each select="Site/Testing/Test[@Status='failed']">
        <xsl:sort select="Name"/>
        <li>
          <a>
            <xsl:attribute name="href"><xsl:value-of select="concat ( 'Results/', translate ( FullName, '/.', '__' ), '.html' )"/></xsl:attribute><xsl:value-of select="Name"/>
          </a>
          <font color="#FF0000"> Failed</font>
        </li>
      </xsl:for-each>
    </ul>

    <hr/>
    <ul>
      <xsl:for-each select="Site/Testing/Test[@Status='notrun']">
        <xsl:sort select="Name"/>
        <li>
          <xsl:value-of select="Name"/><font color="#FF0000"> Not Run</font>
        </li>
      </xsl:for-each>
    </ul>
    <hr/>
    <ul>
      <xsl:for-each select="Site/Testing/Test[@Status='passed']">
        <xsl:sort select="Name"/>
        <li>
          <a>
            <xsl:attribute name="HREF"><xsl:value-of select="concat ( 'Results/', translate ( FullName, '/.', '__' ), '.html' )"/></xsl:attribute><xsl:value-of select="Name"/>
            <font color="#00AA00"> Passed</font>
          </a>
        </li>
      </xsl:for-each>
    </ul>
    <hr/>
    
    <xsl:for-each select="//Testing/Test">
      <xsl:apply-templates select="."/>
    </xsl:for-each>

    <xsl:call-template name="InsightFooter"/>

  </xsl:template>
    
  <xsl:template match="Test">
      <redirect:write select="concat(string('{$TestDocDir}'), '/Results/', translate ( FullName, '/.', '__' ) , '.html' )" file="dan.html">
      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Test results for <xsl:value-of select="Name"/></xsl:with-param>
        <xsl:with-param name="IconDir">../../../../../Icons</xsl:with-param>
        <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
      </xsl:call-template>
      <xsl:value-of select="Name"/>
      <xsl:choose>
        <xsl:when test="contains('failed',@Status)">
          <font color="#FF0000"> Failed</font>
        </xsl:when>
        <xsl:when test="contains('passed',@Status)">
          <font color="#00AA00"> Passed</font>
        </xsl:when>
      </xsl:choose>
      
      <pre>
        <xsl:value-of select="Results/Measurement/Value" disable-output-escaping="yes"/>
      </pre>
      <xsl:call-template name="InsightFooter"/>
    </redirect:write>
  </xsl:template>
    
<xsl:template name="Summary">
  <redirect:write select="concat(string('{$TestDocDir}'), '/TestSummary.xml' )">
    <Testing>
      <SiteName><xsl:value-of select="Site/@Name"/></SiteName>
      <BuildName><xsl:value-of select="Site/@BuildName"/></BuildName>
      <BuildStamp><xsl:value-of select="Site/@BuildStamp"/></BuildStamp>
      <StartDateTime><xsl:value-of select="Site/Testing/StartDateTime"/></StartDateTime>
      
      <PassedCount><xsl:value-of select="count(Site/Testing/Test[@Status='passed'])"/></PassedCount>
      <FailedCount><xsl:value-of select="count(Site/Testing/Test[@Status='failed'])"/></FailedCount>
      <NotRunCount><xsl:value-of select="count(Site/Testing/Test[@Status='notrun'])"/></NotRunCount>
      <EndDateTime><xsl:value-of select="Site/Testing/EndDateTime"/></EndDateTime>
      <xsl:for-each select="Test">
      </xsl:for-each>
    </Testing>
  </redirect:write>
</xsl:template>

</xsl:stylesheet>
  