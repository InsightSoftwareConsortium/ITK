<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output method="xml" indent="yes"/>

<xsl:template match="/">
  <Testing>
    <SiteName><xsl:value-of select="Site/@Name"/></SiteName>
    <BuildName><xsl:value-of select="Site/@BuildName"/></BuildName>
    <BuildStamp><xsl:value-of select="Site/@BuildStamp"/></BuildStamp>
    <StartDateTime><xsl:value-of select="Site/Testing/StartDateTime"/></StartDateTime>

    <PassedCount><xsl:value-of select="count(Site/Testing/Test[@Status='passed'])"/></PassedCount>
    <FailedCount><xsl:value-of select="count(Site/Testing/Test[@Status='failed'])"/></FailedCount>
    <NotRunCount><xsl:value-of select="count(Site/Testing/Test[@Status='notrun'])"/></NotRunCount>
    <EndDateTime><xsl:value-of select="Site/Testing/EndDateTime"/></EndDateTime>
  </Testing>
</xsl:template>

</xsl:stylesheet>