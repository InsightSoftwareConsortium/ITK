<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:template match="/">
    <html>
      <head>
        <title>Test log</title>
      </head>
      <body bgcolor="#ffffff">
        <h2>Testing started on <xsl:value-of select="Site/Testing/StartDateTime"/></h2>
        <h3>
          <a href="#Passed">
            <xsl:value-of select="count(Site/Testing/Test[@Status='passed'])"/>
          </a> passed, 
          <a href="#Failed">
            <xsl:value-of select="count(Site/Testing/Test[@Status='failed'])"/>
          </a> failed </h3>
          <br/>
          <ul>
            <xsl:for-each select="Site/Testing/Test">
              <xsl:sort select="@Status"/>
              <li>
                <a><xsl:attribute name="HREF">#<xsl:value-of select="generate-id()"/></xsl:attribute><xsl:value-of select="Name"/></a>
                <xsl:choose>
                  <xsl:when test="contains('failed',@Status)">
                    <font color="#FF0000"> Failed</font>
                  </xsl:when>
                  <xsl:when test="contains('passed',@Status)">
                    <font color="#00AA00"> Passed</font>
                  </xsl:when>
                </xsl:choose>
              </li>
              
            </xsl:for-each>
          </ul>
          <hr/>
          
          <a><xsl:attribute name="name">Passed</xsl:attribute><h3>Passed Tests</h3></a><hr/>
          <xsl:for-each select="//Testing/Test[@Status='passed']">
            <xsl:apply-templates select="."/>
          </xsl:for-each>
          
          <a><xsl:attribute name="name">Failed</xsl:attribute><h3>Failed Tests</h3></a><hr/>
          <xsl:for-each select="//Testing/Test[@Status='failed']">
            <xsl:apply-templates select="."/>
          </xsl:for-each>
          
        </body>
      </html>
    </xsl:template>
    
    <xsl:template match="Test">
      <a>
        <xsl:attribute name="NAME"><xsl:value-of select="generate-id()"/></xsl:attribute>
        <xsl:value-of select="Name"/>
      </a>
      <xsl:choose>
        <xsl:when test="contains('failed',@Status)">
          <font color="#FF0000"> Failed</font>
        </xsl:when>
        <xsl:when test="contains('passed',@Status)">
          <font color="#00AA00"> Passed</font>
        </xsl:when>
      </xsl:choose>
      
      <pre>
        <xsl:value-of select="Results/Measurement/Value"/>
      </pre>
      <hr/>
    </xsl:template>
    
    
  </xsl:stylesheet>
  