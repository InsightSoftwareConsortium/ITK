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
    <xsl:message>Starting</xsl:message>
    <xsl:call-template name="TestOverviewByTest"/>
    <xsl:call-template name="TestOverviewByCount"/>
    <xsl:call-template name="TestColorOverviewByTest"/>
    <xsl:call-template name="TestColorOverviewByCount"/>
    <xsl:for-each select="/TestOverview/Test">
      <xsl:call-template name="TestDetail"/>
    </xsl:for-each>
  </xsl:template>

  <xsl:template name="TestOverviewByCount">
    <redirect:write select="concat(string('{$DashboardPath}'), '/TestOverviewByCount.html' )" file="dan.html">

      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Insight Test Overview</xsl:with-param>
        <xsl:with-param name="IconDir">../../Icons</xsl:with-param>
        <xsl:with-param name="TestsIcon">TestsBlue.gif</xsl:with-param>
        <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
      </xsl:call-template>
      <a href="TestColorOverviewByCount.html">Color overview</a>
      
      <table>
        <tr>
          <th rowspan="2">Test Name ( <a href="TestOverviewByTest.html">sort by </a> )</th>
          <th colspan="3">Count <img border="0"><xsl:attribute name="src"><xsl:value-of select="$IconDir"/>/DownBlack.gif</xsl:attribute></img></th>
        </tr>
        <tr>
          <th>Passed</th>
          <th>Failed</th>
          <th>Not Run</th>
        </tr>
        
        <xsl:for-each select="/TestOverview/Test">
          <xsl:sort select="count (Result/Status[node()='notrun'])" data-type="number" order="descending"/>
          <xsl:sort select="count (Result/Status[node()='failed'])" data-type="number" order="descending"/>
          <xsl:sort select="Name"/>
          <xsl:call-template name="TestRows"/>
        </xsl:for-each>
      </table>
      <xsl:call-template name="InsightFooter"/>
    </redirect:write>
  </xsl:template>

  <xsl:template name="TestOverviewByTest">
    <redirect:write select="concat(string('{$DashboardPath}'), '/TestOverviewByTest.html' )" file="dan.html">
      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Insight Test Overview</xsl:with-param>
        <xsl:with-param name="IconDir">../../Icons</xsl:with-param>
        <xsl:with-param name="TestsIcon">TestsBlue.gif</xsl:with-param>
        <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
      </xsl:call-template>
      <a href="TestColorOverviewByTest.html">Color overview</a>

      
      <table>
        <tr>
          <th rowspan="2">Test Name <img border="0"><xsl:attribute name="src"><xsl:value-of select="$IconDir"/>/DownBlack.gif</xsl:attribute></img></th>
          <th colspan="3">Count (<a href="TestOverviewByCount.html">sort by</a>)</th>
        </tr>
        <tr>
          <th>Passed</th>
          <th>Failed</th>
          <th>Not Run</th>
        </tr>

        <xsl:for-each select="/TestOverview/Test">
          <xsl:sort select="Name"/>
          <xsl:call-template name="TestRows"/>
        </xsl:for-each>
      </table>

      <xsl:call-template name="InsightFooter"/>
    </redirect:write>
  </xsl:template>

  <xsl:template name="BuildNameLegend">
    <table>
      <tr>
        <th colspan="3">Color legend</th>
      </tr>
      <tr>
        <td><xsl:attribute name="bgcolor"><xsl:value-of select="$Green"/></xsl:attribute>Passed</td>
        <td><xsl:attribute name="bgcolor"><xsl:value-of select="$Red"/></xsl:attribute>Failed</td>
        <td><xsl:attribute name="bgcolor"><xsl:value-of select="$DarkRed"/></xsl:attribute>Not Run</td>
      </tr>
    </table>

    <table>
      <tr>
        <th>Number</th>
        <th>Site Name</th>
        <th>Build Name</th>
        <th>Build Stamp</th>
        <xsl:for-each select="/TestOverview/Test[1]/Result">
          <xsl:sort select="SiteName"/>
          <xsl:sort select="BuildName"/>
          <tr>
            <xsl:if test="position() mod 2 = 0">
              <xsl:attribute name="bgcolor"><xsl:value-of select="$LightBlue"/></xsl:attribute>
            </xsl:if>
            <td><xsl:number value="position()" format="1"/></td>
            <td><xsl:value-of select="SiteName"/></td>
            <td><xsl:value-of select="BuildName"/></td>
            <td><xsl:value-of select="BuildStamp"/></td>
          </tr>
        </xsl:for-each>
      </tr>
    </table>
  </xsl:template>


  <xsl:template name="TestColorOverviewByTest">
    <xsl:message>TestOvenview By Build Name</xsl:message>
    <redirect:write select="concat(string('{$DashboardPath}'), '/TestColorOverviewByTest.html' )" file="dan.html">
      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Insight Test Color Overview by Test</xsl:with-param>
        <xsl:with-param name="IconDir">../../Icons</xsl:with-param>
        <xsl:with-param name="TestsIcon">TestsBlue.gif</xsl:with-param>
        <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
      </xsl:call-template>
      <a href="TestOverviewByTest.html">Numeric overview</a>
      <xsl:call-template name="BuildNameLegend"/>
      <table>
        <tr>
          <th rowspan="2">Test <img border="0"><xsl:attribute name="src"><xsl:value-of select="$IconDir"/>/DownBlack.gif</xsl:attribute></img></th>
          <th>
            <xsl:attribute name="colspan"><xsl:value-of select="count(/TestOverview/Test[1]/Result)"/></xsl:attribute>
            Status ( <a href="TestColorOverviewByCount.html">sort by</a> )
          </th>
        </tr>
        <tr>
          <xsl:for-each select="/TestOverview/Test[1]/Result">
            <xsl:sort select="SiteName"/>
            <xsl:sort select="BuildName"/>
            <th><xsl:number value="position()" format="1"/></th>
          </xsl:for-each>
        </tr>

        <xsl:for-each select="/TestOverview/Test">
          <xsl:sort select="Name"/>
          <xsl:call-template name="ColorOverviewRow"/>
        </xsl:for-each>
      </table>

      <xsl:call-template name="BuildNameLegend"/>

    <xsl:call-template name="InsightFooter"/>
    </redirect:write>
  </xsl:template>

  <xsl:template name="TestColorOverviewByCount">
    <redirect:write select="concat(string('{$DashboardPath}'), '/TestColorOverviewByCount.html' )" file="dan.html">
      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Insight Test Color Overview by Count</xsl:with-param>
        <xsl:with-param name="IconDir">../../Icons</xsl:with-param>
        <xsl:with-param name="TestsIcon">TestsBlue.gif</xsl:with-param>
        <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
      </xsl:call-template>
      <a href="TestOverviewByCount.html">Numeric overview</a>
      <xsl:call-template name="BuildNameLegend"/>
      <table>
        <tr>
          <th rowspan="2">Test ( <a href="TestColorOverviewByTest.html">sort by</a> )</th>
          <th>
            <xsl:attribute name="colspan"><xsl:value-of select="count(/TestOverview/Test[1]/Result)"/></xsl:attribute>
            Status <img border="0"><xsl:attribute name="src"><xsl:value-of select="$IconDir"/>/DownBlack.gif</xsl:attribute></img>
          </th>
        </tr>
        <tr>
          <xsl:for-each select="/TestOverview/Test[1]/Result">
            <xsl:sort select="SiteName"/>
            <xsl:sort select="BuildName"/>
            <th><xsl:number value="position()" format="1"/></th>
          </xsl:for-each>
        </tr>

        <xsl:for-each select="/TestOverview/Test">
          <xsl:sort select="count (Result/Status[node()='notrun'])" data-type="number" order="descending"/>
          <xsl:sort select="count (Result/Status[node()='failed'])" data-type="number" order="descending"/>
          <xsl:sort select="Name"/>
          <xsl:call-template name="ColorOverviewRow"/>
        </xsl:for-each>
      </table>

      <xsl:call-template name="BuildNameLegend"/>

    <xsl:call-template name="InsightFooter"/>
    </redirect:write>
  </xsl:template>

  <xsl:template name="ColorOverviewRow">
    <tr>
      <xsl:if test="position() mod 2 = 0">
        <xsl:attribute name="bgcolor"><xsl:value-of select="$LightBlue"/></xsl:attribute>
      </xsl:if>
      <td>
        <a>
          <xsl:attribute name="href">
            <xsl:call-template name="TranslateTestName">
              <xsl:with-param name="Prefix">TestDetail/</xsl:with-param>
              <xsl:with-param name="TestName" select="Name"/>
              <xsl:with-param name="PostFix">.html</xsl:with-param>
            </xsl:call-template>
          </xsl:attribute>
          <xsl:value-of select="Name"/>
        </a>
      </td>
      <xsl:for-each select="Result">
        <xsl:sort select="SiteName"/>
        <xsl:sort select="BuildName"/>
        <xsl:choose>
          <xsl:when test="contains(Status,'passed')">
            <td>
              <xsl:attribute name="bgcolor"><xsl:value-of select="$Green"/></xsl:attribute>
              <a style="text-decoration:none">
                <xsl:attribute name="href">
                  <xsl:call-template name="TranslateTestName">
                    <xsl:with-param name="Prefix">../../Sites/<xsl:value-of select="SiteName"/>/<xsl:value-of select="BuildName"/>/<xsl:value-of select="BuildStamp"/>/Results/</xsl:with-param>
                    <xsl:with-param name="TestName" select="Name"/>
                    <xsl:with-param name="PostFix">.html</xsl:with-param>
                  </xsl:call-template>
                </xsl:attribute>
                <xsl:text disable-output-escaping="yes"><![CDATA[&nbsp;&nbsp;&nbsp;]]></xsl:text>
              </a>
            </td>
          </xsl:when>
          <xsl:when test="contains(Status,'notrun')">
            <td>
              <xsl:attribute name="bgcolor"><xsl:value-of select="$DarkRed"/></xsl:attribute>
              <xsl:text disable-output-escaping="yes"><![CDATA[&nbsp;&nbsp;&nbsp;]]></xsl:text>
            </td>
          </xsl:when>
          <xsl:when test="contains(Status,'failed')">
            <td>
              <xsl:attribute name="bgcolor"><xsl:value-of select="$Red"/></xsl:attribute>
              <a style="text-decoration:none">
                <xsl:attribute name="href">
                  <xsl:call-template name="TranslateTestName">
                    <xsl:with-param name="Prefix">../../Sites/<xsl:value-of select="SiteName"/>/<xsl:value-of select="BuildName"/>/<xsl:value-of select="BuildStamp"/>/Results/</xsl:with-param>
                    <xsl:with-param name="TestName" select="Name"/>
                    <xsl:with-param name="PostFix">.html</xsl:with-param>
                  </xsl:call-template>
                </xsl:attribute>
                <xsl:text disable-output-escaping="yes"><![CDATA[&nbsp;&nbsp;&nbsp;]]></xsl:text>
              </a>
            </td>
          </xsl:when>
        </xsl:choose>
      </xsl:for-each>
    </tr>
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






  <xsl:template name="TestRows">
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
  </xsl:template>
  

</xsl:stylesheet>
