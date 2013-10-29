<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version='1.0'>


   <xsl:output method="xml" indent="yes"  doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN"
   doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" />

    <xsl:template match="/">
      <html>
       <head>
       <title><xsl:value-of select="cdash/title"/></title>
        <meta name="robots" content="noindex,nofollow" />
         <link rel="StyleSheet" type="text/css">
          <xsl:attribute name="href">stylish.css</xsl:attribute>
         </link>
       </head>
       <body bgcolor="#111111">

<br/>

<!-- Main -->
<br/>
<h3>Dynamic analysis started on <xsl:value-of select="Site/DynamicAnalysis/StartDateTime"/></h3>
<table border="0">
  <tr><td align="right"><b>Site Name:</b></td><td><xsl:value-of select="Site/@Hostname"/></td></tr>
  <tr><td align="right"><b>Build Name:</b></td><td><xsl:value-of select="Site/@BuildStamp"/></td></tr>
  <tr><td align="right"><b>Test Result:</b></td>
    <td>
      <font>
        <xsl:attribute name="color">
          <xsl:choose>
            <xsl:when test="Site/DynamicAnalysis/Test[Name='@TESTNAME@']/@Status='passed'">
            #00aa00
            </xsl:when>
            <xsl:otherwise>
            #ffcc66
            </xsl:otherwise>
          </xsl:choose>
        </xsl:attribute>
      <xsl:value-of select="Site/DynamicAnalysis/Test[Name='@TESTNAME@']/@Status"/>
      </font>
   </td>
  </tr>
  <tr><td align="right"><b>Name:</b></td><td><xsl:value-of select="Site/DynamicAnalysis/Test[Name='@TESTNAME@']/Name"/></td></tr>
  <tr><td align="right"><b>Path:</b></td><td><xsl:value-of select="Site/DynamicAnalysis/Test[Name='@TESTNAME@']/Path"/></td></tr>
  <tr><td align="right"><b>Command:</b></td><td><xsl:value-of select="Site/DynamicAnalysis/Test[Name='@TESTNAME@']/FullCommandLine"/></td></tr>
</table>

<pre><xsl:value-of select="Site/DynamicAnalysis/Test[Name='@TESTNAME@']/Log"/></pre>
 <br/>

<!-- FOOTER -->
<br/>
        </body>
      </html>
    </xsl:template>
</xsl:stylesheet>
