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
  <xsl:output method="html"/>
  <xsl:param name="DashboardPath"/>
  <xsl:param name="DashboardStamp" select="string('MostRecentResults-Nightly')"/>
  <xsl:param name="PreviousDashboardStamp" select="string('')"/>
  <xsl:variable name="DashboardDir" select="concat('../', $DashboardStamp)"/>
  <xsl:variable name="IconDir" select="string('../../Icons')"/>

  <xsl:include href="Insight.xsl"/>
  <xsl:template match="/">
    <xsl:call-template name="InsightHeader">
      <xsl:with-param name="Title">Insight Test Overview</xsl:with-param>
      <xsl:with-param name="IconDir">../../Icons</xsl:with-param>
      <xsl:with-param name="TestsIcon">TestsBlue.gif</xsl:with-param>
      <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
    </xsl:call-template>

    <table>
      <tr>
        <th rowspan="2">Test Name</th>
        <th colspan="3">Count</th>
      </tr>
      <tr>
        <th>Passed</th>
        <th>Failed</th>
        <th>Not Run</th>
      </tr>

      <xsl:for-each select="/TestOverview/Test">
        <xsl:call-template name="TestDetail"/>
      </xsl:for-each>

      <xsl:for-each select="/TestOverview/Test">
        <xsl:sort select="count (Result/Status[node()='notrun'])" data-type="number" order="descending"/>
        <xsl:sort select="count (Result/Status[node()='failed'])" data-type="number" order="descending"/>
        <xsl:sort select="Name"/>
        <tr>
          <xsl:if test="position() mod 2 = 0">
            <xsl:attribute name="bgcolor"><xsl:value-of select="$LightBlue"/></xsl:attribute>
          </xsl:if>
          <td>
            <a>
              <xsl:attribute name="href">
                <xsl:value-of select="concat('TestDetail/', translate ( Name, '/.', '__' ) , '.html' )"/>
              </xsl:attribute>
              <xsl:value-of select="Name"/>
            </a>
          </td>
          <td><xsl:value-of select="count (Result/Status[node()='passed'])"/></td>
          <td>
            <xsl:choose>
              <xsl:when test="count (Result/Status[node()='failed']) &gt; 0">
                <xsl:attribute name="bgcolor"><xsl:value-of select="$Red"/></xsl:attribute>
              </xsl:when>
            </xsl:choose>
            <xsl:value-of select="count (Result/Status[node()='failed'])"/>
          </td>
          <td>
            <xsl:choose>
              <xsl:when test="count (Result/Status[node()='notrun']) &gt; 0">
                <xsl:attribute name="bgcolor"><xsl:value-of select="$Red"/></xsl:attribute>
              </xsl:when>
            </xsl:choose>
            <xsl:value-of select="count (Result/Status[node()='notrun'])"/>
          </td>
        </tr>
      </xsl:for-each>
    </table>

    <xsl:call-template name="InsightFooter"/>
  </xsl:template>


  <xsl:template name="TestDetail">
    <redirect:write select="concat(string('{$DashboardPath}'), '/TestDetail/', translate ( Name, '/.', '__' ) , '.html' )" file="dan.html">
      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Insight Test Detail - <xsl:value-of select="Name"/></xsl:with-param>
        <xsl:with-param name="IconDir">../../../Icons</xsl:with-param>
        <xsl:with-param name="DashboardDir">..</xsl:with-param>
      </xsl:call-template>
      <table border="1">
        <tr>
          <th>Site</th>
          <th>Build Name</th>
          <th>Build Stamp</th>
          <th>Status</th>
        </tr>
        <xsl:for-each select="Result">
          <xsl:sort select="SiteName"/>
          <xsl:sort select="BuildName"/>
          <tr>
            <xsl:if test="position() mod 2 = 0">
              <xsl:attribute name="bgcolor"><xsl:value-of select="$LightBlue"/></xsl:attribute>
            </xsl:if>
            <td><xsl:value-of select="SiteName"/></td>
            <td>
              <a>
                <xsl:attribute name="href">../../../Sites/<xsl:value-of select="SiteName"/>/<xsl:value-of select="BuildName"/>/<xsl:value-of select="BuildStamp"/>/Test.html</xsl:attribute>
                <xsl:value-of select="BuildName"/>
              </a>
              </td>
            <td><xsl:value-of select="BuildStamp"/></td>
            <td>
              <xsl:choose>
                <xsl:when test="Status = 'failed'"><xsl:attribute name="bgcolor"><xsl:value-of select="$Red"/></xsl:attribute></xsl:when>
                <xsl:when test="Status = 'notrun'"><xsl:attribute name="bgcolor"><xsl:value-of select="$Red"/></xsl:attribute></xsl:when>
              </xsl:choose>
              <xsl:choose>
                <xsl:when test="Status = 'passed'">
                  <a>
                    <xsl:attribute name="href">../../../Sites/<xsl:value-of select="SiteName"/>/<xsl:value-of select="BuildName"/>/<xsl:value-of select="BuildStamp"/>/Results/<xsl:value-of select="concat ( translate ( ../Name, '/.', '__' ), '.html' )"/></xsl:attribute>
                    Passed
                  </a>
                </xsl:when>
                <xsl:when test="Status = 'failed'">
                  <a>
                    <xsl:attribute name="href">../../../Sites/<xsl:value-of select="SiteName"/>/<xsl:value-of select="BuildName"/>/<xsl:value-of select="BuildStamp"/>/Results/<xsl:value-of select="concat ( translate ( ../Name, '/.', '__' ), '.html' )"/></xsl:attribute>
                    Failed
                  </a>
                </xsl:when>
                <xsl:when test="Status = 'notrun'">Not Run</xsl:when>
              </xsl:choose>
            </td>
          </tr>
        </xsl:for-each>
      </table>
      <xsl:call-template name="InsightFooter"/>
    </redirect:write>
  </xsl:template>

</xsl:stylesheet>
