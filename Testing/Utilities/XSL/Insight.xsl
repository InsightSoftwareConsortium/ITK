<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:template name="InsightFooter">
    <xsl:text disable-output-escaping="yes">
            &lt;/td>
          &lt;/tr>
        &lt;/table>
      &lt;/body>
    &lt;/html>
    </xsl:text>
  </xsl:template>


  <xsl:template name="InsightHeader">
    <xsl:param name="Title">Insight Dashboard</xsl:param>
    <xsl:param name="IconDir">../../Icons</xsl:param>
    <xsl:param name="DashboardDir">../MostRecentResults-Nightly</xsl:param>

    <xsl:text disable-output-escaping="yes">
    &lt;html>
      &lt;head>
        &lt;title>
    </xsl:text>
    <xsl:value-of select="$Title"/>
    <xsl:text disable-output-escaping="yes">
        &lt;/title>
      &lt;/head>
      &lt;body bgcolor="#ffffff">
        &lt;table border="4" cellpading="0" cellspacing="2" width="100%">
          &lt;tr>
            &lt;td width="140">
              &lt;img src="</xsl:text>
    <xsl:value-of select="$IconDir"/>
    <xsl:text disable-output-escaping="yes">/Logo.gif" border="0">&lt;/img>
            &lt;/td>
            &lt;td>
              &lt;h1>Insight testing dashboard&lt;/h1>
            &lt;/td>
          &lt;/tr>
          &lt;tr>
            &lt;td valign="top" halign="center">
              &lt;table width="100%" halign="center">
                &lt;tr>
                  &lt;td>
                    &lt;a href="</xsl:text>
    <xsl:value-of select="$DashboardDir"/>
    <xsl:text disable-output-escaping="yes">/Update.html">&lt;img src="</xsl:text>
    <xsl:value-of select="$IconDir"/>
    <xsl:text disable-output-escaping="yes">/Updates.gif" border="0">&lt;/img>&lt;/a>
                  &lt;/td>
                &lt;/tr>
                &lt;tr>
                  &lt;td>
                    &lt;a>&lt;img src="</xsl:text>
      <xsl:value-of select="$IconDir"/>
      <xsl:text disable-output-escaping="yes">/Errors.gif" border="0">&lt;/img>&lt;/a>
                  &lt;/td>
                &lt;/tr>
                &lt;tr>
                  &lt;td>
                    &lt;a>&lt;img src="</xsl:text>
      <xsl:value-of select="$IconDir"/>
    <xsl:text disable-output-escaping="yes">/Warnings.gif" border="0">&lt;/img>&lt;/a>
                  &lt;/td>
                &lt;/tr>
                &lt;tr>
                  &lt;td>
                    &lt;a>&lt;img src="
    </xsl:text>
    <xsl:value-of select="$IconDir"/>
    <xsl:text disable-output-escaping="yes">
/Tests.gif" border="0">&lt;/img>&lt;/a>
                  &lt;/td>
                &lt;/tr>
                &lt;tr>
                  &lt;td>
                    &lt;a >&lt;img src="
    </xsl:text>
    <xsl:value-of select="$IconDir"/>
    <xsl:text disable-output-escaping="yes">
/Coverage.gif" border="0">&lt;/img>&lt;/a>
                  &lt;/td>
                &lt;/tr>
              &lt;/table>
            &lt;/td>		    
            &lt;td>
            </xsl:text>
            </xsl:template>
          </xsl:stylesheet>