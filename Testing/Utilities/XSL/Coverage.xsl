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
      <xsl:with-param name="Title">Coverage Log</xsl:with-param>
      <xsl:with-param name="IconDir">../../../../Icons</xsl:with-param>
      <xsl:with-param name="CoverageIcon">CoverageBlue.gif</xsl:with-param>
      <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
    </xsl:call-template>

    <h3>Coverage started on <xsl:value-of select="Site/Coverage/StartDateTime"/></h3>
    <table border="4" cellpadding="0" cellspacing="2" width="250">
      <tr>
        <td align="left" width="180"> Total Coverage</td> 
        <td>
          <xsl:choose>
            <xsl:when test="Site/Coverage/PercentCoverage &lt; 50">
              <xsl:attribute name="bgcolor">#FF7F50</xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="bgcolor">#00ff7f</xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
          <xsl:value-of select="Site/Coverage/PercentCoverage"/>%
        </td>
      </tr>    
      <tr>
        <td align="left"> Tested lines</td>
        <td><xsl:value-of select="Site/Coverage/LOCTested"/></td>
      </tr>
      <tr>
        <td align="left">Untested lines</td> 
        <td><xsl:value-of select="Site/Coverage/LOCUntested"/></td>
      </tr>
      <tr>
        <td align="left">Files Coverage</td>
        <td><xsl:value-of select="count(Site/Coverage/File[@Covered='true'])"/>  of <xsl:value-of select="count(Site/Coverage/File)"/></td>
      </tr>
      <tr>
        <td align="left">Covered &gt; 70.0%</td>
        <td><xsl:value-of select="count(Site/Coverage/File[@Covered='true']/PercentCoverage[node() &gt;= 70.0])"/> / <xsl:value-of select="count(Site/Coverage/File[@Covered='true']/PercentCoverage[node() &lt; 70.0])"/></td>
      </tr>
    </table>

    <table>
      <tr>
        <th>Filename</th>
        <th>Percentage</th>
      </tr>
      <xsl:for-each select="Site/Coverage/File">
        <xsl:sort select="@Covered" order="descending"/>
        <xsl:sort select="@FullPath"/>
        <xsl:call-template name="File"/>
      </xsl:for-each>
    </table>
    <xsl:call-template name="InsightFooter"/>
  </xsl:template>

  <xsl:template name="File">
    <xsl:choose>
      <xsl:when test="@Covered='true'">
        <tr>
          <xsl:choose>
            <xsl:when test="PercentCoverage &gt;= 70.0">
              <xsl:attribute name="bgcolor">#00FF7f</xsl:attribute>
            </xsl:when>
            <xsl:when test="PercentCoverage &lt;= 50.0">
              <xsl:attribute name="bgcolor">#FF7F50</xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="bgcolor">#aaaa00</xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
          <td align="left"><xsl:value-of select="@FullPath"/></td>
          <td align="center"><xsl:value-of select="PercentCoverage"/>%</td>
        </tr>
      </xsl:when>
      <xsl:when test="@Covered='false'">
        <tr bgcolor="#FF6666">
          <td align="left"><xsl:value-of select="@FullPath"/></td>
          <td align="center">UNTESTED</td>
        </tr>
      </xsl:when>
    </xsl:choose>
  </xsl:template>

<xsl:template name="Summary">
  <redirect:write select="concat(string('{$TestDocDir}'), '/CoverageSummary.xml' )">

    <Coverage>
      <SiteName><xsl:value-of select="Site/@Name"/></SiteName>
      <BuildName><xsl:value-of select="Site/@BuildName"/></BuildName>
      <BuildStamp><xsl:value-of select="Site/@BuildStamp"/></BuildStamp>
      <StartDateTime><xsl:value-of select="Site/Coverage/StartDateTime"/></StartDateTime>
      <PercentCoverage><xsl:value-of select="Site/Coverage/PercentCoverage"/></PercentCoverage>
      <LOCTested><xsl:value-of select="Site/Coverage/LOCTested"/></LOCTested>
      <LOCUntested><xsl:value-of select="Site/Coverage/LOCUntested"/></LOCUntested>
      <LOC><xsl:value-of select="Site/Coverage/LOC"/></LOC>
      <Passed><xsl:value-of select="count(Site/Coverage/File/PercentCoverage[node() &gt;= 70])"/></Passed>
      <Failed><xsl:value-of select="count(Site/Coverage/File/PercentCoverage[node() &lt; 70])"/></Failed>
      <EndDateTime><xsl:value-of select="Site/Coverage/EndDateTime"/></EndDateTime>
    </Coverage>
  </redirect:write>
</xsl:template>

</xsl:stylesheet>