<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output method="xml" indent="yes"/>

<xsl:template match="/Doxygen">
  <Doxygen>
    <StartDateTime><xsl:value-of select="StartDateTime"/></StartDateTime>
    <ErrorCount><xsl:value-of select="count(Error)"/></ErrorCount>
    <WarningCount><xsl:value-of select="count(Warning)"/></WarningCount>
  </Doxygen>
</xsl:template>
</xsl:stylesheet>
