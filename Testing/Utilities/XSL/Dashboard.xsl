<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:template match="/Dashboard">
    <html>
      <head>
        <title>
          Insight Dashboard - <xsl:value-of select="Information/LocalTime"/>
        </title>
      </head>
      <body bgcolor="#ffffff">
        <table border="4" cellpading="0" cellspacing="2" width="100%">
          <tr>
            <td width="140">
              <img src="../../Icons/Logo.gif" border="0"></img>
            </td>
            <td>
              <h1>Insight testing dashboard</h1>
            </td>
          </tr>
          <tr>
            <td valign="top" halign="center">
              <table width="100%" halign="center">
                <tr>
                  <td>
                    <a href="Update.html"><img src="../../Icons/Updates.gif" border="0"></img></a>
                  </td>
                </tr>
                <tr>
                  <td>
                    <a href="BuildError.html"><img src="../../Icons/Errors.gif" border="0"></img></a>
                  </td>
                </tr>
                <tr>
                  <td>
                    <a href="BuildWarning.html"><img src="../../Icons/Warnings.gif" border="0"></img></a>
                  </td>
                </tr>
                <tr>
                  <td>
                    <a href="Test.html"><img src="../../Icons/Tests.gif" border="0"></img></a>
                  </td>
                </tr>
                <tr>
                  <td>
                    <a href="Coverage.html"><img src="../../Icons/Coverage.gif" border="0"></img></a>
                  </td>
                </tr>
              </table>
            </td>		    
            <td>
              <h3>
                <a>
                  <xsl:attribute name="href">Update.html</xsl:attribute>
                  <xsl:value-of select="Update/ChangedFileCount"/> Files Changed
                  by <xsl:value-of select="Update/AuthorCount"/> Authors
                </a>
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
            
              <h2>Experimental Builds</h2>
            
              <table border="2">
              
                <xsl:call-template name="BuildTableHeader"/>
                <xsl:for-each select="BuildStamp">
                  <xsl:if test="not ( contains(Build/BuildStamp,'Nightly') )">
                    <xsl:call-template name="BuildStamp"/>
                  </xsl:if>
                </xsl:for-each>
              
              </table>
            
              <xsl:choose>
                <xsl:when test="count(BuildStamp/Coverage)">
                  <h3>Coverage</h3>
                  <table border="4" cellpadding="0" cellspacing="2" width="100%">
                    <tr>
                      <th align="center" bgcolor="#eeeeee">Site</th>
                      <th align="center" bgcolor="#eeeeee">Build Name</th>
                      <th align="center" bgcolor="#eeeeee" width="80">Percentage</th>
                      <th align="center" bgcolor="#eeeeee">Coverage</th>
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
            </td>
          </tr>
        </table>
      </body>
    </html>
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
