<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:template match="/">
    <html>
      <head>
        <title>Build log</title>
      </head>
      <body bgcolor="#ffffff">
        <h2>Build started on <xsl:value-of select="Site/Build/StartDateTime"/></h2>
        <h3>Found 
        <a href="#Error">
          <xsl:value-of select="count(Site/Build/Error)"/> Errors
        </a>
        and 
        <a href="#Warning">
          <xsl:value-of select="count(Site/Build/Warning)"/> Warnings
        </a>
      </h3>
      <br/>
      <hr/>
      <a name="Error">
        <h2>Errors</h2>
      </a>
      <xsl:for-each select="Site/Build/Error">
        <h3>Error</h3> <xsl:call-template name="Report"/>
      </xsl:for-each>

      <hr/>
      <a name="Warning">
        <h2>Warnings</h2>
      </a>

      <xsl:for-each select="//Build/Warning">
        <h3>Warning</h3> <xsl:call-template name="Report"/>
      </xsl:for-each>
      
    </body>
  </html>
</xsl:template>

<xsl:template name="Report">
Build Log line
         <a>
           <xsl:attribute name="href">BuildLog.html.gz#<xsl:value-of select="BuildLogLine"/></xsl:attribute>
           <xsl:value-of select="BuildLogLine"/>
         </a>
         <xsl:if test="count(SourceFile) > 0">
         in <xsl:value-of select="SourceFile"/> line <xsl:value-of select="SourceLineNumber"/>
         </xsl:if>
         
        <br/>
        <pre><xsl:value-of select="PreContext"/></pre>
        <strong><xsl:value-of select="Text"/></strong><br/>
        <pre><xsl:value-of select="PostContext"/></pre>
</xsl:template>
	
</xsl:stylesheet>