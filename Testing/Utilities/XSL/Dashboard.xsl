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

  <xsl:template match="/Dashboard">
    <xsl:call-template name="TestOverview"/>
    <xsl:call-template name="InsightHeader">
      <xsl:with-param name="Title">Insight Dashboard</xsl:with-param>
      <xsl:with-param name="IconDir">../../Icons</xsl:with-param>
      <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
      <xsl:with-param name="HomeIcon">HomeBlue.gif</xsl:with-param>
    </xsl:call-template>
    <h3>
      <xsl:choose>
        <xsl:when test="Update/ChangedFileCount != ''">
          <a href="Update.html">
            <xsl:value-of select="Update/ChangedFileCount"/> Files Changed
            by <xsl:value-of select="Update/AuthorCount"/> Authors
            as of <xsl:value-of select="Update/StartDateTime"/>
          </a>
        </xsl:when>
        <xsl:otherwise>
          No Update information available!
        </xsl:otherwise>
      </xsl:choose>
      <br/>
      
    </h3>

    <h3>
      <xsl:choose>
        <xsl:when test="string-length(Doxygen/StartDateTime) != 0">
          <a>
            <xsl:attribute name="href">Doxygen.html</xsl:attribute>
            Doxygen: <xsl:value-of select="Doxygen/ErrorCount"/> Errors
            and <xsl:value-of select="Doxygen/WarningCount"/> Warnings
          </a>
        </xsl:when>
        <xsl:otherwise>
          No Doxygen information available!
        </xsl:otherwise>
      </xsl:choose>
      <br/>
     
    </h3>
    <table cellspacing="0" border="0">
      <tr>
        <td>Reported Builds</td>
        <td><xsl:value-of select="count(BuildStamp/Build/BuildStamp)"/></td>
      </tr>
      <tr>
        <td><xsl:value-of select="sum(/Dashboard/BuildStamp/Build/ErrorCount)"/></td>
        <td>Reported Errors</td>
      </tr>
      <tr>
        <td>Reported Warnings</td>
        <td><xsl:value-of select="sum(/Dashboard/BuildStamp/Build/WarningCount)"/></td>
      </tr>
      <tr>
        <td>Passed Tests</td>
        <td><xsl:value-of select="sum(/Dashboard/BuildStamp/Testing/PassedCount)"/></td>
      </tr>
      <tr>
        <td>Failed Tests</td>
        <td><xsl:value-of select="sum(/Dashboard/BuildStamp/Testing/FailedCount)"/></td>
      </tr>
      <tr>
        <td>Tests Not Run</td>
        <td><xsl:value-of select="sum(/Dashboard/BuildStamp/Testing/NotRunCount)"/></td>
      </tr>
    </table>
    
      
    
            
    <table border="2" width="100%">
    <xsl:choose>
      <xsl:when test="count(BuildStamp/Build/BuildStamp[contains(node(),'Nightly')])">
        <tr><td colspan="9" valign="center"><h3>Nightly Builds</h3></td></tr>
        <xsl:call-template name="BuildTableHeader"/>
        <xsl:for-each select="BuildStamp">
          <xsl:sort select="Build/SiteName"/>
          <xsl:sort select="Build/BuildName"/>
          <xsl:if test="contains(Build/BuildStamp,'Nightly')">
            <xsl:call-template name="BuildStamp"/>
          </xsl:if>
        </xsl:for-each>
      </xsl:when>
      <xsl:otherwise>
        <tr><td colspan="9"><h3>No Nightly Builds</h3></td></tr>
      </xsl:otherwise>        
    </xsl:choose>

    <xsl:choose>
      <xsl:when test="count(BuildStamp/Build/BuildStamp[not(contains(node(),'Nightly'))])">
        <tr><td colspan="9" valign="center"><h3>Experimental Builds</h3></td></tr>
          <xsl:call-template name="BuildTableHeader"/>
          <xsl:for-each select="BuildStamp">
            <xsl:sort select="Build/SiteName"/>
            <xsl:sort select="Build/BuildName"/>
            <xsl:if test="not ( contains(Build/BuildStamp,'Nightly') )">
              <xsl:call-template name="BuildStamp"/>
            </xsl:if>
          </xsl:for-each>
      
      </xsl:when>
      <xsl:otherwise>
        <tr><td colspan="9"><h3>No Experimental Builds</h3></td></tr>
      </xsl:otherwise>
    </xsl:choose>
        </table>
            
              <xsl:choose>
                <xsl:when test="count(BuildStamp/Coverage)">
                  <h3>Coverage</h3>
                  <table border="4" cellpadding="0" cellspacing="2" width="100%">
                    <tr>
                      <th align="center" bgcolor="#eeeeee">Site</th>
                      <th align="center" bgcolor="#eeeeee">Build Name</th>
                      <th align="center" bgcolor="#eeeeee" width="80">Percentage</th>
                      <th align="center" bgcolor="#eeeeee">Passed</th>
                      <th align="center" bgcolor="#eeeeee">Failed</th>
                      <th align="center" bgcolor="#eeeeee">Date</th>
                      <th align="center" bgcolor="#eeeeee">Submission Date</th>
                    </tr>
                    <!--
                         Loop over each instance
                         -->
                    <xsl:for-each select="BuildStamp">
                      <xsl:sort select="Build/SiteName"/>
                      <xsl:sort select="Build/BuildName"/>
                      <xsl:if test="Coverage/LOC != ''">
                        <xsl:variable name="URLBase">../../Sites/<xsl:value-of select="Coverage/SiteName"/>/<xsl:value-of select="Coverage/BuildName"/>/<xsl:value-of select="Coverage/BuildStamp"/></xsl:variable>
                        <tr>
                          <td align="left"><xsl:value-of select="Coverage/SiteName"/></td>
                          <td align="left"><xsl:value-of select="Coverage/BuildName"/></td>
                          <td align="center">
                            <xsl:choose>
                              <xsl:when test="Coverage/PercentCoverage &lt; 50">
                                <xsl:attribute name="bgcolor"><xsl:value-of select="$Red"/></xsl:attribute>
                              </xsl:when>
                              <xsl:otherwise>
                                <xsl:attribute name="bgcolor"><xsl:value-of select="$Green"/></xsl:attribute>
                              </xsl:otherwise>
                            </xsl:choose>
                            <a>
                              <xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Coverage.html</xsl:attribute><xsl:value-of select="Coverage/PercentCoverage"/>%
                            </a>
                          </td>
                          <td align="right">
                            <xsl:value-of select="Coverage/Passed"/>
                          </td>
                          <td align="right">
                            <xsl:value-of select="Coverage/Failed"/>
                          </td>
                          <td align="left"><xsl:value-of select="Coverage/StartDateTime"/></td>
                          <td align="left"><xsl:value-of select="CoverageSubmissionDateTime"/></td>
                        </tr>
                      </xsl:if>
                    </xsl:for-each>
                  </table>
                </xsl:when>
                <xsl:otherwise>
                  <h3>No coverage information</h3><br/>
                </xsl:otherwise>
              </xsl:choose>
              <xsl:if test="Information/Yesterday != ''">
                <h3>
                  <a>
                    <xsl:attribute name="href">
                      ../<xsl:value-of select="Information/Yesterday"/>/Dashboard.html
                    </xsl:attribute>
                    Yesterday's dashboard
                  </a>
                </h3>
                <br/>
              </xsl:if>
              <xsl:call-template name="InsightFooter"/>
  </xsl:template>


  <xsl:template name="BuildStamp">
    <xsl:variable name="URLBase">../../Sites/<xsl:value-of select="Build/SiteName"/>/<xsl:value-of select="Build/BuildName"/>/<xsl:value-of select="Build/BuildStamp"/></xsl:variable>

    <tr>
      <td align="left">
        <xsl:value-of select="Build/SiteName"/>
      </td>
      <td align="left">
        <xsl:value-of select="Build/BuildName"/>
      </td>
      <td align="right">
        <xsl:choose>
          <xsl:when test="Build/ErrorCount > 0">
            <xsl:attribute name="bgcolor"><xsl:value-of select="$Red"/></xsl:attribute>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="bgcolor"><xsl:value-of select="$Green"/></xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <b><a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/BuildError.html</xsl:attribute><xsl:value-of select="Build/ErrorCount"/></a></b>
        <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;</xsl:text>
      </td>
      <td align="right">
        <xsl:choose>
          <xsl:when test="Build/WarningCount > 0">
            <xsl:attribute name="bgcolor"><xsl:value-of select="$Red"/></xsl:attribute>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="bgcolor"><xsl:value-of select="$Green"/></xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <b><a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/BuildWarning.html</xsl:attribute><xsl:value-of select="Build/WarningCount"/></a></b>
        <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
      </td>

      <xsl:variable name="BGColor"><xsl:value-of select="$Red"/></xsl:variable>
      <xsl:choose>
        <xsl:when test="Testing/FailedCount + Testing/NotRunCount > 0">
          <xsl:variable name="BGColor"><xsl:value-of select="$Green"/></xsl:variable>
        </xsl:when>
      </xsl:choose>
      <td align="right">
        <xsl:attribute name="bgcolor"><xsl:value-of select="$BGColor"/></xsl:attribute>
        <b><a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html</xsl:attribute><xsl:value-of select="Testing/PassedCount"/></a></b>
        <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
      </td>
      <td align="right">
        <xsl:choose>
          <xsl:when test="Testing/FailedCount > 0">
            <xsl:attribute name="bgcolor"><xsl:value-of select="$Red"/></xsl:attribute>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="bgcolor"><xsl:value-of select="$Green"/></xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <b><a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html</xsl:attribute><xsl:value-of select="Testing/FailedCount"/></a></b>
        <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
      </td>
      <td align="right">
        <xsl:choose>
          <xsl:when test="Testing/NotRunCount > 0">
            <xsl:attribute name="bgcolor"><xsl:value-of select="$Red"/></xsl:attribute>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="bgcolor"><xsl:value-of select="$Green"/></xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <b><a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html</xsl:attribute><xsl:value-of select="Testing/NotRunCount"/></a></b>
        <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
      </td>
      <td><xsl:value-of select="Build/StartDateTime"/></td>
      <td><xsl:value-of select="TestSubmissionDateTime"/></td>
    </tr>
    
    
  </xsl:template>
  
  <xsl:template name="BuildTableHeader">
    <tr bgcolor="#eeeeee">
      <th align="center" rowspan="2">Site</th>
      <th align="center" rowspan="2">Build Name</th>
      <th align="center" colspan="2">Build</th>
      <th align="center" colspan="3">Test</th>
      <th align="center" rowspan="2">Build Date</th>
      <th align="center" rowspan="2">Submission Date</th>
    </tr>
    <tr bgcolor="#eeeeee">
      <th align="center">Errors</th>
      <th align="center">Warnings</th>
      <th align="center">Passed</th>
      <th align="center">Failed</th>
      <th align="center">NotRun</th>
    </tr>
  </xsl:template>

  <xsl:template name="TestOverview">
    <redirect:write select="concat ( string('{$DashboardPath}'), '/TestOverview.xml' )">
      <TestOverview>
        <xsl:for-each select="/Dashboard/BuildStamp[1]/Testing/Tests/Test">
          <xsl:sort select="FullName"/>
            <xsl:variable name="TestName"><xsl:value-of select="FullName"/></xsl:variable>
            <Test>
              <Name><xsl:value-of select="$TestName"/></Name>
              <xsl:for-each select="/Dashboard/BuildStamp/Testing/Tests/Test[FullName=$TestName]">
                <Result>
                  <SiteName><xsl:value-of select="../../SiteName"/></SiteName>
                  <BuildName><xsl:value-of select="../../BuildName"/></BuildName>
                  <BuildStamp><xsl:value-of select="../../BuildStamp"/></BuildStamp>
                  <Status><xsl:value-of select="Status"/></Status>
                </Result>
              </xsl:for-each>
            </Test>
        </xsl:for-each>
      </TestOverview>
    </redirect:write>
  </xsl:template>

  
</xsl:stylesheet>
