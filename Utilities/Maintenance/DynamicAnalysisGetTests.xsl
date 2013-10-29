<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version='1.0'>
    <xsl:output method="text"/>
    <xsl:template match="/">
      <xsl:for-each select="Site/DynamicAnalysis/Test">
        <xsl:value-of select="Name"/>
<!--newline-->
<xsl:text>
</xsl:text>
      </xsl:for-each>
    </xsl:template>
</xsl:stylesheet>
