<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
    version="1.0"
    xmlns:lxslt="http://xml.apache.org/xslt"
    xmlns:redirect="org.apache.xalan.lib.Redirect"
    extension-element-prefixes="redirect">

  <!--
       Use DashboardStamp as a parameter, default to most recent
       The proper flags to Xalan are in the form -PARAM DashboardStamp "string('foo')"
       -->
 <xsl:param name="DashboardPath"/> 
 <xsl:param name="DashboardStamp" select="string('MostRecentResults-Nightly')"/>
 <xsl:param name="TestDocDir">.</xsl:param>
 <xsl:variable name="DashboardDir" select="concat('../../../../Dashboard/', $DashboardStamp)"/>
 <xsl:variable name="IconDir">../../../../Icons</xsl:variable>
 <xsl:include href="Insight.xsl"/>
 <xsl:output type="html"/>

  <xsl:template match="/">
    <xsl:for-each select="Site/CoverageLog/File">

    <xsl:message>Working on <xsl:value-of select="@Name"/></xsl:message>
    <xsl:message>  FullName on <xsl:value-of select="@FullPath"/></xsl:message>
    <redirect:write select="concat(string('{$TestDocDir}'), '/../Coverage/', translate ( @FullPath, '/.', '__' ), '.html' )" file="dan.html">
      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Annotated Coverage for <xsl:value-of select="@Name"/></xsl:with-param>
        <xsl:with-param name="IconDir">../../../../Icons</xsl:with-param>
        <xsl:with-param name="CoverageIcon">CoverageBlue.gif</xsl:with-param>
        <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
      </xsl:call-template>
        <b>Site Name: </b> <xsl:value-of select="/Site/@Name"/>
      <p>
        <b>Build Name: </b> <xsl:value-of select="/Site/@BuildName"/>
      </p>
      <p>
        <b>Coverage Date: </b> <xsl:value-of select="/Site/CoverageLog/StartDateTime"/>
      </p>

      <hr/>
      <pre>
        <xsl:for-each select="Report/Line">
          <xsl:sort select="@Number" data-type="number"/>
          <xsl:choose>
            <xsl:when test="@Count &lt; 0">
              <xsl:text disable-output-escaping="yes"><![CDATA[&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;]]></xsl:text>
            </xsl:when>
            <xsl:when test="@Count = 0">
              <xsl:text disable-output-escaping="yes"><![CDATA[########]]></xsl:text>
            </xsl:when>
            <xsl:otherwise>
              <xsl:value-of select="format-number ( @Count, '00000000' )"/>
            </xsl:otherwise>
          </xsl:choose>

          <xsl:text disable-output-escaping="yes"><![CDATA[&nbsp;&nbsp;]]></xsl:text>
          <xsl:value-of select="."/>
        <xsl:text disable-output-escaping="yes">
</xsl:text>
        </xsl:for-each>
      </pre>
      <xsl:call-template name="InsightFooter"/>
    </redirect:write> 
    </xsl:for-each>
  </xsl:template>

</xsl:stylesheet>