<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:template match="/">
    <html>
      <head>
        <title>Build log</title>
      </head>
      <body bgcolor="#ffffff">
        <h2>Build started on <xsl:value-of select="///Build/StartDateTime"/></h2>
        <h3>Found 
        <a>
          <xsl:attribute name="href">#Error</xsl:attribute>
          <xsl:value-of select="count(//Build/Error)"/> Errors
        </a>
        and 
        <a>
          <xsl:attribute name="href">#Warning</xsl:attribute>
          <xsl:value-of select="count(//Build/Warning)"/> Warnings
        </a>
      </h3>
      <br/>
      <hr/>
      <a>
        <xsl:attribute name="name">Error</xsl:attribute>
        <h2>Errors</h2>
      </a>
      <xsl:for-each select="//Build/Error">
        <h3>Error</h3> Build Log line <xsl:value-of select="BuildLogLine"/><br/>
        <pre><xsl:value-of select="PreContext"/></pre>
        <strong><xsl:value-of select="Text"/></strong><br/>
        <pre><xsl:value-of select="PostContext"/></pre>
      </xsl:for-each>

      <hr/>
      <a>
        <xsl:attribute name="name">Warning</xsl:attribute>
        <h2>Warnings</h2>
      </a>
      <xsl:for-each select="//Build/Warning">
        <h3>Warning</h3> Build Log line <xsl:value-of select="BuildLogLine"/><br/>
        <pre><xsl:value-of select="PreContext"/></pre>
        <strong><xsl:value-of select="Text"/></strong>
        <br/>
        <pre><xsl:value-of select="PostContext"/></pre>
      </xsl:for-each>
      
    </body>
  </html>
</xsl:template>
	
<!-- Match Log, StartDateTime, and EndDateTime to keep them from appearing in the output -->
<xsl:template match="/Build/Log | /Build/StartDateTime | /Build/EndDateTime"/>
<xsl:template match="/Build/Error | /Build/Warning"/>

</xsl:stylesheet>