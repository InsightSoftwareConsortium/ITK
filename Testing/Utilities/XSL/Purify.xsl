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
  <xsl:param name="DashboardStamp" select="string('MostRecentResults-Nightly')"/>
  <xsl:variable name="DashboardDir" select="concat('../../../../Dashboard/', $DashboardStamp)"/>
  <xsl:param name="TestDocDir">.</xsl:param>
  <xsl:preserve-space elements="xsl:text"/>
  <xsl:include href="Insight.xsl"/>
  <xsl:output method="html"/>
  <xsl:template match="/">
    <xsl:call-template name="Summary"/>

    <!--
         Write both the Errors and Warnings
         -->
    <redirect:write select="concat(string('{$TestDocDir}'), '/Purify.html' )">
      <xsl:call-template name="Purify"/>
    </redirect:write>

  </xsl:template>

  <xsl:template name="Purify">
      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Purify <xsl:value-of select="Site/@Name"/> -- <xsl:value-of select="Site/@BuildName"/></xsl:with-param>
        <xsl:with-param name="IconDir">../../../../Icons</xsl:with-param>
        <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
      </xsl:call-template>
     <p><b><xsl:text>Site:</xsl:text></b><xsl:value-of select="Site/@Name"/></p>
     <p>
       <b>
         <xsl:text>Build Name:</xsl:text>
       </b>
       <xsl:value-of select="Site/@BuildName"/>
     </p> 

    <table cellspacing="0">
      <tr>
        <th>Name</th>
        <th>Status</th>
        <th>MLK</th>
        <th>ABR</th>
        <th>ABW</th>
        <th>COR</th>
        <th>FMM</th>
        <th>FUM</th>
        <th>FMR</th>
        <th>FMW</th>
        <th>MAF</th>
        <th>UMC</th>
        <th>UMR</th>
      </tr>

      <xsl:for-each select="Site/Purify/Test">
        <xsl:sort select="@Status" order="descending"/>
        <xsl:sort select="Name"/>
        <xsl:call-template name="Test"/>
        <tr>
          <xsl:if test="position() mod 2 = 0">
            <xsl:attribute name="bgcolor"><xsl:value-of select="$LightBlue"/></xsl:attribute>
          </xsl:if>
          <td>
            <a>
              <xsl:attribute name="href">
                <xsl:call-template name="TranslateTestName">
                  <xsl:with-param name="Prefix"><xsl:value-of select="$DashboardDir"/>/Purify/</xsl:with-param>
                  <xsl:with-param name="TestName" select="FullName"/>
                  <xsl:with-param name="Postfix">.html</xsl:with-param>
                </xsl:call-template>
              </xsl:attribute>
              <xsl:value-of select="Name"/>
            </a>
          </td>
          <td>
            <xsl:choose>
              <xsl:when test="contains('passed',@Status)">
                <xsl:attribute name="bgcolor"><xsl:value-of select="$NormalColor"/></xsl:attribute>
                Passed
              </xsl:when>
              <xsl:when test="contains('failed',@Status)">
                <xsl:attribute name="bgcolor"><xsl:value-of select="$WarningColor"/></xsl:attribute>
                Failed
              </xsl:when>
              <xsl:otherwise>
                <xsl:attribute name="bgcolor"><xsl:value-of select="$ErrorColor"/></xsl:attribute>
                Not Run
              </xsl:otherwise>
            </xsl:choose>
          </td>
          <td align="center"><xsl:value-of select="Results/MLK"/></td>
          <td align="center"><xsl:value-of select="Results/ABR"/></td>
          <td align="center"><xsl:value-of select="Results/ABW"/></td>
          <td align="center"><xsl:value-of select="Results/COR"/></td>
          <td align="center"><xsl:value-of select="Results/FMM"/></td>
          <td align="center"><xsl:value-of select="Results/FUM"/></td>
          <td align="center"><xsl:value-of select="Results/FMR"/></td>
          <td align="center"><xsl:value-of select="Results/FMW"/></td>
          <td align="center"><xsl:value-of select="Results/MAF"/></td>
          <td align="center"><xsl:value-of select="Results/UMC"/></td>
          <td align="center"><xsl:value-of select="Results/UMR"/></td>
        </tr>
      </xsl:for-each>
    </table>

    <xsl:call-template name="InsightFooter"/>

  </xsl:template>


   <xsl:template name="Test">
     <redirect:write select="concat(string('{$TestDocDir}'), '/Purify/', translate ( FullName, '/.', '__' ) , '.html' )" file="dan.html">
       <xsl:call-template name="InsightHeader">
         <xsl:with-param name="Title">Purify results for <xsl:value-of select="Name"/></xsl:with-param>
         <xsl:with-param name="IconDir">../../../../../Icons</xsl:with-param>
         <xsl:with-param name="DashboardDir">../<xsl:value-of select="$DashboardDir"/></xsl:with-param>
       </xsl:call-template>
       <p>
         <b>Site Name: </b> <xsl:value-of select="/Site/@Name"/>
       </p>
       <p>
         <b>Build Name: </b> <xsl:value-of select="/Site/@BuildName"/>
       </p>
       <a>
         <xsl:attribute name="href">
           <xsl:call-template name="TranslateTestName">
             <xsl:with-param name="Prefix">../<xsl:value-of select="$DashboardDir"/>/TestDetail/</xsl:with-param>
             <xsl:with-param name="TestName" select="FullName"/>
             <xsl:with-param name="Postfix">.html</xsl:with-param>
           </xsl:call-template>
         </xsl:attribute>
         <xsl:value-of select="Name"/> 
       </a>
       <xsl:choose>
         <xsl:when test="contains('failed',@Status)">
           <font>
             <xsl:attribute name="color"><xsl:value-of select="$WarningColor"/></xsl:attribute>
             Failed
           </font>
         </xsl:when>
         <xsl:when test="contains('passed',@Status)">
           <font>
             <xsl:attribute name="color"><xsl:value-of select="$NormalColor"/></xsl:attribute>
             Passed
           </font>
         </xsl:when>
       </xsl:choose>
      
       <pre>
         <xsl:value-of select="Log" disable-output-escaping="yes"/>
       </pre>
       <xsl:call-template name="InsightFooter"/>
     </redirect:write>
   </xsl:template>



<xsl:template name="Summary">
  <redirect:write select="concat(string('{$TestDocDir}'), '/BuildSummary.xml' )">
    <Build>
      <SiteName><xsl:value-of select="Site/@Name"/></SiteName>
      <BuildName><xsl:value-of select="Site/@BuildName"/></BuildName>
      <BuildStamp><xsl:value-of select="Site/@BuildStamp"/></BuildStamp>
      <StartDateTime><xsl:value-of select="Site/Build/StartDateTime"/></StartDateTime>

      <WarningCount><xsl:value-of select="count(Site/Build/Warning)"/></WarningCount>
      <ErrorCount><xsl:value-of select="count(Site/Build/Error)"/></ErrorCount>
      <EndDateTime><xsl:value-of select="Site/Build/EndDateTime"/></EndDateTime>
    </Build>
  </redirect:write>
</xsl:template>

</xsl:stylesheet>