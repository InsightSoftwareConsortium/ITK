<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output method="text" indent="no" media-type="text/plain"/>

  <xsl:template match="/">
<xsl:value-of select="Site/Build/Log/@Encoding"/><xsl:text> </xsl:text><xsl:value-of select="Site/Build/Log/@Compression"/>
  <xsl:value-of select="Site/Build/Log"/>
  </xsl:template>
	
</xsl:stylesheet>