<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <!--
       Use DashboardStamp as a parameter, default to most recent
       The proper flags to Xalan are in the form -PARAM DashboardStamp "string('foo')"
       -->
  <xsl:param name="DashboardStamp" select="string('MostRecentResults-Nightly')"/>
  <xsl:param name="PreviousDashboardStamp" select="string('')"/>
  <xsl:variable name="DashboardDir" select="concat('../../../../Dashboard/', $DashboardStamp)"/>
  <xsl:variable name="IconDir" select="string('../../Icons')"/>

  <xsl:include href="Insight.xsl"/>

  <xsl:template match="/Dashboard">
    <xsl:call-template name="InsightHeader">
      <xsl:with-param name="Title">Insight dashboard</xsl:with-param>
      <xsl:with-param name="IconDir">../../Icons</xsl:with-param>
    </xsl:call-template>
    <h3>
      <xsl:choose>
        <xsl:when test="Update/ChangedFileCount != ''">
          <a href="Update.html">
            <xsl:value-of select="Update/ChangedFileCount"/> Files Changed
            by <xsl:value-of select="Update/AuthorCount"/> Authors
          </a>
        </xsl:when>
        <xsl:otherwise>
          No Update information available!
        </xsl:otherwise>
      </xsl:choose>
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
    </h3>
    
    <h2>Nightly Builds</h2>
            
    <table border="2">
              
      <xsl:call-template name="BuildTableHeader"/>
      <xsl:for-each select="BuildStamp">
        <xsl:if test="contains(Build/BuildStamp,'Nightly')">
          <xsl:call-template name="BuildStamp"/>
        </xsl:if>
      </xsl:for-each>
    </table>
    

    <xsl:choose>
      <xsl:when test="count(Build/BuildStamp[contains(node(),'Nightly')])">
        <h2>Experimental Builds</h2>
        <table border="2">
          <xsl:call-template name="BuildTableHeader"/>
          <xsl:for-each select="BuildStamp">
            <xsl:if test="not ( contains(Build/BuildStamp,'Nightly') )">
              <xsl:call-template name="BuildStamp"/>
            </xsl:if>
          </xsl:for-each>
      
        </table>
      </xsl:when>
      <xsl:otherwise>
        <h3>No Experimental Builds</h3>
      </xsl:otherwise>
    </xsl:choose>
            
              <xsl:choose>
                <xsl:when test="count(BuildStamp/Coverage)">
                  <h3>Coverage</h3>
                  <table border="4" cellpadding="0" cellspacing="2" width="100%">
                    <tr>
                      <th align="center" bgcolor="#eeeeee">Site</th>
                      <th align="center" bgcolor="#eeeeee">Build Name</th>
                      <th align="center" bgcolor="#eeeeee" width="80">Percentage</th>
                      <th align="center" bgcolor="#eeeeee">Files Covered</th>
                      <th align="center" bgcolor="#eeeeee">Date</th>
                    </tr>
                    <!--
                         Loop over each instance
                         -->
                    <xsl:for-each select="BuildStamp">
                      <xsl:if test="Coverage/LOC != ''">
                        <xsl:variable name="URLBase">../../Sites/<xsl:value-of select="Coverage/SiteName"/>/<xsl:value-of select="Coverage/BuildName"/>/<xsl:value-of select="Coverage/BuildStamp"/></xsl:variable>
                        <tr>
                          <td align="center"><xsl:value-of select="Coverage/SiteName"/></td>
                          <td align="center"><xsl:value-of select="Coverage/BuildName"/></td>
                          <td align="center">
                            <xsl:choose>
                              <xsl:when test="Coverage/PercentCoverage &lt; 50">
                                <xsl:attribute name="bgcolor">#ff7f50</xsl:attribute>
                              </xsl:when>
                              <xsl:otherwise>
                                <xsl:attribute name="bgcolor">#00ff7f</xsl:attribute>
                              </xsl:otherwise>
                            </xsl:choose>
                            <a>
                              <xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Coverage.html</xsl:attribute><xsl:value-of select="Coverage/PercentCoverage"/>%
                            </a>
                          </td>
                          <td align="center"><xsl:value-of select="Coverage/StartDateTime"/></td>
                        </tr>
                      </xsl:if>
                    </xsl:for-each>
                  </table>
                </xsl:when>
                <xsl:otherwise>
                  <h3>No coverage information</h3><br/>
                </xsl:otherwise>
              </xsl:choose>
              <xsl:if test="$PreviousDashboardStamp != ''">
                <h3>
                  <a href="../{$PreviousDashboardStamp}/Dashboard.html">
                    Previous dashboard
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
                    <xsl:attribute name="bgcolor">#FF7F50</xsl:attribute>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:attribute name="bgcolor">#00ff7f</xsl:attribute>
                  </xsl:otherwise>
                </xsl:choose>
                <a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Build.html#Error</xsl:attribute><xsl:value-of select="Build/ErrorCount"/></a>
              </td>
              <td align="right">
                <xsl:choose>
                  <xsl:when test="Build/WarningCount > 0">
                    <xsl:attribute name="bgcolor">#FF7F50</xsl:attribute>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:attribute name="bgcolor">#00ff7f</xsl:attribute>
                  </xsl:otherwise>
                </xsl:choose>
                <a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Build.html#Warning</xsl:attribute><xsl:value-of select="Build/WarningCount"/></a>
              </td>
              <td align="right">
                <xsl:choose>
                  <xsl:when test="Testing/PassedCount = 0">
                    <xsl:attribute name="bgcolor">#FF7F50</xsl:attribute>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:attribute name="bgcolor">#00ff7f</xsl:attribute>
                  </xsl:otherwise>
                </xsl:choose>
                <a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html#Passed</xsl:attribute><xsl:value-of select="Testing/PassedCount"/></a>
              </td>
              <td>
                <xsl:choose>
                  <xsl:when test="Site/Testing/FailedCount > 0">
                    <xsl:attribute name="bgcolor">#FF7F50</xsl:attribute>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:attribute name="bgcolor">#00ff7f</xsl:attribute>
                  </xsl:otherwise>
                </xsl:choose>
                <xsl:attribute name="align">right</xsl:attribute>
                <a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html#Failed</xsl:attribute><xsl:value-of select="Testing/FailedCount"/></a>
              </td>
              <td>
                <xsl:choose>
                  <xsl:when test="Site/Testing/NotRunCount > 0">
                    <xsl:attribute name="bgcolor">#FF7F50</xsl:attribute>
                  </xsl:when>
                  <xsl:otherwise>
                    <xsl:attribute name="bgcolor">#00ff7f</xsl:attribute>
                  </xsl:otherwise>
                </xsl:choose>
                <xsl:attribute name="align">right</xsl:attribute>
                <a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html#NotRun</xsl:attribute><xsl:value-of select="Testing/NotRunCount"/></a>
              </td>
              <td><xsl:value-of select="Testing/StartDateTime"/></td>
            </tr>


</xsl:template>

<xsl:template name="BuildTableHeader">
          <tr>
            <th align="left">Site</th>
            <th align="left">Build Name</th>
            <th align="left">Build Errors</th>
            <th align="left">Build Warnings</th>
            <th align="left">Passed</th>
            <th align="left">Failed</th>
            <th align="left">NotRun</th>
            <th align="left">Date</th>
          </tr>
</xsl:template>

</xsl:stylesheet>
