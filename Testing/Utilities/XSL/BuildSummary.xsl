<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output method="xml" indent="yes"/>


<xsl:template match="/">
  <Build>
    <SiteName><xsl:value-of select="Site/@Name"/></SiteName>
    <BuildName><xsl:value-of select="Site/@BuildName"/></BuildName>
    <BuildStamp><xsl:value-of select="Site/@BuildStamp"/></BuildStamp>
    <StartDateTime><xsl:value-of select="Site/Build/StartDateTime"/></StartDateTime>

    <WarningCount><xsl:value-of select="count(Site/Build/Warning)"/></WarningCount>
    <ErrorCount><xsl:value-of select="count(Site/Build/Error)"/></ErrorCount>
    <EndDateTime><xsl:value-of select="Site/Build/EndDateTime"/></EndDateTime>
  </Build>
</xsl:template>

</xsl:stylesheet>