<?xml version="1.0" encoding="ISO-8859-1"?>
<!DOCTYPE xsl:stylesheet>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:template match="/">
    <html>
      <head>
        <title>Coverage log</title>
      </head>
      <body bgcolor="#ffffff">
        <h2>Coverage started on <xsl:value-of select="Site/Coverage/StartDateTime"/></h2>
        <h4>
          Coverage: <xsl:value-of select="Site/Coverage/PercentCoverage"/>%<br/>
         <xsl:value-of select="Site/Coverage/LOCTested"/> Tested lines of <xsl:value-of select="Site/Coverage/LOCUntested"/> of <xsl:value-of select="Site/Coverage/LOC"/> Total Lines of Code
        </h4>
        <xsl:value-of select="count(File[@Covered='true'])"/> Files Covered
        <xsl:value-of select="count(Directory//File)"/> Files Not Covered
        <hr/>


        <table>
          <tr>
            <th>Filename</th>
            <th>Lines Covered</th>
            <th>Lines Not Covered</th>
            <th>Percentage</th>
            <th>Date</th>
          </tr>

          <xsl:apply-templates select="Site/Coverage/Directory"/>
        </table>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="Directory">
    <tr>
      <th>
        <xsl:attribute name="align">left</xsl:attribute>
        <xsl:value-of select="@FullPath"/>
      </th>
    </tr>
    <xsl:if test="count(File) > 0">
      <xsl:for-each select="File">
        <xsl:sort select="@Covered" order="descending"/>
        <xsl:sort select="@Name"/>
        <xsl:choose>
          <xsl:when test="@Covered='true'">
            <tr>
              <td><xsl:value-of select="@Name"/></td>
              <td><xsl:value-of select="LOCTested"/></td>
              <td><xsl:value-of select="LOCUntested"/></td>
              <td><xsl:value-of select="PercentCoverage"/>%</td>
            </tr>
          </xsl:when>
          <xsl:when test="@Covered='false'">
            <tr>
              <xsl:attribute name="bgcolor">#FF6666</xsl:attribute>
              <td><xsl:value-of select="@Name"/></td>
              <td>
                <xsl:attribute name="colspan">3</xsl:attribute>
                UNTESTED
              </td>
            </tr>
          </xsl:when>
        </xsl:choose>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates select="Directory"/>
    
  </xsl:template>

  <xsl:template match="File|LOCTested|LOCUntested|StartDateTime|EndDateTime|PercentCoverage"/>

</xsl:stylesheet>