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

        <h1>Insight <xsl:value-of select="Information/Model"/> Testing Dashboard</h1>
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
            <table>
              <tr>
                <th>Site</th>
                <th>Build Name</th>
                <th>Percentage</th>
                <th>Coverage</th>
                <th>Date</th>
              </tr>
              
            <!-- Loop over each instance -->
            <xsl:for-each select="BuildStamp">
              <xsl:if test="Coverage/LOC != ''">
                
                <xsl:variable name="URLBase">../../Sites/<xsl:value-of select="Coverage/SiteName"/>/<xsl:value-of select="Coverage/BuildName"/>/<xsl:value-of select="Coverage/BuildStamp"/></xsl:variable>
                
                <tr>
                  <td><xsl:value-of select="Coverage/SiteName"/></td>
                  <td><xsl:value-of select="Coverage/BuildName"/></td>
                  <td><a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Coverage.html</xsl:attribute><xsl:value-of select="Coverage/PercentCoverage"/>%</a></td>
                  <td><xsl:value-of select="Coverage/LOCTested"/><xsl:text> of </xsl:text><xsl:value-of select="Coverage/LOC"/></td>
                  <td><xsl:value-of select="Coverage/StartDateTime"/></td>
                </tr>
              </xsl:if>
            </xsl:for-each>
          </table>
        </xsl:when>
        <xsl:otherwise>
          <h3>No coverage information</h3><br/>
        </xsl:otherwise>
      </xsl:choose>
      

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
                <a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Build.html#Error</xsl:attribute><xsl:value-of select="Build/ErrorCount"/></a>
              </td>
              <td>
                <xsl:attribute name="align">right</xsl:attribute>
                <a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Build.html#Warning</xsl:attribute><xsl:value-of select="Build/WarningCount"/></a>
              </td>
              <td>
                <xsl:attribute name="align">right</xsl:attribute>
                <a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html#Passed</xsl:attribute><xsl:value-of select="Testing/PassedCount"/></a>
              </td>
              <td>
                <xsl:attribute name="align">right</xsl:attribute>
                <a><xsl:attribute name="HREF"><xsl:value-of select="$URLBase"/>/Test.html#Failed</xsl:attribute><xsl:value-of select="Testing/FailedCount"/></a>
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
            <th align="left">Date</th>
          </tr>
</xsl:template>

</xsl:stylesheet>

