<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output method="xml" indent="yes"/>
<xsl:template match="/">
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
</xsl:template>

</xsl:stylesheet>