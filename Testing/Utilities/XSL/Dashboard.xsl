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
  <xsl:param name="CreationDate" select="string('')"/>
  <xsl:variable name="DashboardDir" select="concat('../', $DashboardStamp)"/>
  <xsl:variable name="IconDir" select="string('../../Icons')"/>

  <xsl:include href="Insight.xsl"/>

  <xsl:template match="/Dashboard">
    <xsl:call-template name="TestOverview"/>
    <xsl:call-template name="InsightHeader">
      <xsl:with-param name="Title">Insight Dashboard - <xsl:value-of select="$CreationDate"/></xsl:with-param>
      <xsl:with-param name="IconDir">../../Icons</xsl:with-param>
      <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
      <xsl:with-param name="HomeIcon">HomeBlue.gif</xsl:with-param>
    </xsl:call-template>
    <h3>
      <xsl:choose>
        <xsl:when test="Update/ChangedFileCount != ''">
          <a href="Update.html">
            <xsl:value-of select="Update/ChangedFileCount"/> Files Changed
          </a>
          by <xsl:value-of select="Update/AuthorCount"/> Authors
          as of <xsl:value-of select="Update/StartDateTime"/>
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
          <a href="Doxygen.html">
            Doxygen:
          </a>
          <xsl:value-of select="Doxygen/ErrorCount"/> Errors
          and <xsl:value-of select="Doxygen/WarningCount"/> Warnings
        </xsl:when>
        <xsl:otherwise>
          No Doxygen information available!
        </xsl:otherwise>
      </xsl:choose>
      <br/>
     
    </h3>
            
    <table border="2" width="100%">
    <xsl:choose>
      <xsl:when test="count(BuildStamp/Build/BuildStamp[contains(node(),'Nightly')])">
        <tr><td colspan="9" valign="center"><h3>Nightly Builds</h3></td></tr>
        <xsl:call-template name="BuildTableHeader"/>
        <xsl:for-each select="BuildStamp">
          <xsl:sort select="Build/SiteName"/>
          <xsl:sort select="Build/BuildName"/>
          <xsl:sort select="Testing/SiteName"/>
          <xsl:sort select="Testing/BuildName"/>
          <xsl:if test="contains(Build/BuildStamp,'Nightly') or contains(Testing/BuildStamp,'Nightly')">
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
            <xsl:sort select="Build/BuildStamp" order="descending"/>
            <xsl:sort select="Build/SiteName"/>
            <xsl:sort select="Build/BuildName"/>
            <xsl:if test="not ( contains(Build/BuildStamp,'Nightly') or contains(Testing/BuildStamp,'Nightly') ) and count(Build/SiteName | Testing/Sitename)">
              <xsl:call-template name="BuildStamp"/>
            </xsl:if>
          </xsl:for-each>
      
      </xsl:when>
      <xsl:otherwise>
        <tr><td colspan="9"><h3>No Experimental Builds</h3></td></tr>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:if test="count(BuildStamp/Build/BuildStamp)">
      <tr>
        <td align="left">
          Totals
        </td>
        <td align="center">
          <b><xsl:value-of select="count(BuildStamp/Build/BuildStamp)"/> Builds</b>
        </td>
        <td align="right">
          <xsl:choose>
            <xsl:when test="sum(/Dashboard/BuildStamp/Build/ErrorCount) &gt; 0">
              <xsl:attribute name="bgcolor"><xsl:value-of select="$ErrorColor"/></xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
          <b><xsl:value-of select="sum(/Dashboard/BuildStamp/Build/ErrorCount)"/></b>
          <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
        </td>
        <td align="right">
          <xsl:choose>
            <xsl:when test="sum(/Dashboard/BuildStamp/Build/WarningCount) &gt; 0">
              <xsl:attribute name="bgcolor"><xsl:value-of select="$WarningColor"/></xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
          <b><xsl:value-of select="sum(/Dashboard/BuildStamp/Build/WarningCount)"/></b>
          <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
        </td>
        <td align="right">
          <xsl:choose>
            <xsl:when test="sum(/Dashboard/BuildStamp/Testing/NotRunCount) &gt; 0">
              <xsl:attribute name="bgcolor"><xsl:value-of select="$ErrorColor"/></xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
          <b><xsl:value-of select="sum(/Dashboard/BuildStamp/Testing/NotRunCount)"/></b>
          <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
        </td>
        <td align="right">
          <xsl:choose>
            <xsl:when test="sum(/Dashboard/BuildStamp/Testing/FailedCount) &gt; 0">
              <xsl:attribute name="bgcolor"><xsl:value-of select="$WarningColor"/></xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
          <b><xsl:value-of select="sum(/Dashboard/BuildStamp/Testing/FailedCount)"/></b>
          <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
        </td>
        <td align="right">
          <xsl:choose>
            <xsl:when test="sum(/Dashboard/BuildStamp/Testing/FailedCount) + sum(/Dashboard/BuildStamp/Testing/NotRunCount) &gt; 0">
              <xsl:attribute name="bgcolor"><xsl:value-of select="$WarningColor"/></xsl:attribute>
            </xsl:when>
            <xsl:otherwise>
              <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
            </xsl:otherwise>
          </xsl:choose>
          <b><xsl:value-of select="sum(/Dashboard/BuildStamp/Testing/PassedCount)"/></b>
          <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
        </td>
        <td/><td/><td/>
      </tr>
    </xsl:if>

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
                                <xsl:attribute name="bgcolor"><xsl:value-of select="$WarningColor"/></xsl:attribute>
                              </xsl:when>
                              <xsl:otherwise>
                                <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
                              </xsl:otherwise>
                            </xsl:choose>
                            <a>
                              <xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/CoverageByName.html</xsl:attribute><b><xsl:value-of select="Coverage/PercentCoverage"/>%</b>
                            </a>
                          </td>
                          <td align="right">
                            <b><xsl:value-of select="Coverage/Passed"/></b>
                          </td>
                          <td align="right">
                            <b><xsl:value-of select="Coverage/Failed"/></b>
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

        <xsl:choose>
          <xsl:when test="count(BuildStamp/Purify)">
            <h3>Purify</h3>
            <table border="4" cellpadding="0" cellspacing="2" width="100%">
              <tr>
                <th align="center" bgcolor="#eeeeee">Site</th>
                <th align="center" bgcolor="#eeeeee">Build Name</th>
                <th align="center" bgcolor="#eeeeee">Defect Count</th>
                <th align="center" bgcolor="#eeeeee">Date</th>
                <th align="center" bgcolor="#eeeeee">Submission Date</th>
              </tr>
              <!--
                   Loop over each instance
                   -->
              <xsl:for-each select="BuildStamp">
                <xsl:sort select="Purify/SiteName"/>
                <xsl:sort select="Purify/BuildName"/>
                <xsl:variable name="URLBase">../../Sites/<xsl:value-of select="Purify/SiteName"/>/<xsl:value-of select="Purify/BuildName"/>/<xsl:value-of select="Purify/BuildStamp"/></xsl:variable>
                <tr>
                  <td align="left"><xsl:value-of select="Purify/SiteName"/></td>
                  <td align="left"><xsl:value-of select="Purify/BuildName"/></td>
                  <td align="center">
                    <xsl:choose>
                      <xsl:when test="Purify/DefectCount != 0">
                        <xsl:attribute name="bgcolor"><xsl:value-of select="$WarningColor"/></xsl:attribute>
                      </xsl:when>
                      <xsl:otherwise>
                        <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
                      </xsl:otherwise>
                    </xsl:choose>
                    <a>
                      <xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Purify.html</xsl:attribute><b><xsl:value-of select="Purify/DefectCount"/></b>
                    </a>
                  </td>
                  <td align="left"><xsl:value-of select="Purify/StartDateTime"/></td>
                  <td align="left"><xsl:value-of select="PurifySubmissionDateTime"/></td>
                </tr>
              </xsl:for-each>
            </table>
          </xsl:when>
          <xsl:otherwise>
            <h3>No purify information</h3><br/>
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
        <xsl:choose>
          <xsl:when test="count(Build/SiteName)">
            <xsl:value-of select="Build/SiteName"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="Testing/SiteName"/>
          </xsl:otherwise>
        </xsl:choose>
      </td>
      <td align="left">
        <xsl:choose>
          <xsl:when test="count(Build/BuildName)">
            <xsl:value-of select="Build/BuildName"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="Testing/BuildName"/>
          </xsl:otherwise>
        </xsl:choose>
      </td>
      <td align="right">
        <xsl:choose>
          <xsl:when test="Build/ErrorCount > 0">
            <xsl:attribute name="bgcolor"><xsl:value-of select="$ErrorColor"/></xsl:attribute>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <b><a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/BuildError.html</xsl:attribute><xsl:value-of select="Build/ErrorCount"/></a></b>
        <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;</xsl:text>
      </td>
      <td align="right">
        <xsl:choose>
          <xsl:when test="Build/WarningCount > 0">
            <xsl:attribute name="bgcolor"><xsl:value-of select="$WarningColor"/></xsl:attribute>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <b><a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/BuildWarning.html</xsl:attribute><xsl:value-of select="Build/WarningCount"/></a></b>
        <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
      </td>

      <td align="right">
        <xsl:choose>
          <xsl:when test="Testing/NotRunCount > 0">
            <xsl:attribute name="bgcolor"><xsl:value-of select="$ErrorColor"/></xsl:attribute>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <b><a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html</xsl:attribute><xsl:value-of select="Testing/NotRunCount"/></a></b>
        <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
      </td>
      <td align="right">
        <xsl:choose>
          <xsl:when test="Testing/FailedCount > 0">
            <xsl:attribute name="bgcolor"><xsl:value-of select="$WarningColor"/></xsl:attribute>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <b><a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html</xsl:attribute><xsl:value-of select="Testing/FailedCount"/></a></b>
        <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
      </td>
      <td align="right">
        <xsl:choose>
          <xsl:when test="(Testing/NotRunCount + Testing/FailedCount) > 0">
            <xsl:attribute name="bgcolor"><xsl:value-of select="$WarningColor"/></xsl:attribute>
          </xsl:when>
          <xsl:otherwise>
            <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
          </xsl:otherwise>
        </xsl:choose>
        <b><a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html</xsl:attribute><xsl:value-of select="Testing/PassedCount"/></a></b>
        <xsl:text disable-output-escaping="yes">&amp;nbsp;&amp;nbsp;&amp;nbsp;</xsl:text>
      </td>
      <td>
        <xsl:choose>
          <xsl:when test="count(Build/StartDateTime)">
            <xsl:value-of select="Build/StartDateTime"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="Testing/StartDateTime"/>
          </xsl:otherwise>
        </xsl:choose>
      </td>
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
      <th align="center">NotRun</th>
      <th align="center">Failed</th>
      <th align="center">Passed</th>
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
              <xsl:for-each select="/Dashboard/BuildStamp/Testing">
                <Result>
                  <SiteName><xsl:value-of select="SiteName"/></SiteName>
                  <BuildName><xsl:value-of select="BuildName"/></BuildName>
                  <BuildStamp><xsl:value-of select="BuildStamp"/></BuildStamp>
                  <xsl:choose>
                    <xsl:when test="count(Tests/Test[FullName=$TestName])">
                      <Status><xsl:value-of select="Tests/Test[FullName=$TestName]/Status"/></Status>
                    </xsl:when>
                    <xsl:otherwise>
                      <Status>notrun</Status>
                    </xsl:otherwise>
                  </xsl:choose>
                </Result>
              </xsl:for-each>
            </Test>
        </xsl:for-each>
      </TestOverview>
    </redirect:write>
  </xsl:template>

  
</xsl:stylesheet>
