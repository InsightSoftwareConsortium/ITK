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
  <xsl:param name="TestDocDir">.</xsl:param>
  <xsl:variable name="DashboardDir" select="concat('../../../../Dashboard/', $DashboardStamp)"/>
  <xsl:include href="Insight.xsl"/>

  <xsl:template match="/">
    <xsl:call-template name="Summary"/>
    <xsl:call-template name="InsightHeader">
      <xsl:with-param name="Title">Test Log</xsl:with-param>
      <xsl:with-param name="IconDir">../../../../Icons</xsl:with-param>
      <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
    </xsl:call-template>
    <h2>Testing started on <xsl:value-of select="Site/Testing/StartDateTime"/></h2>
    <p>
      <b>Site Name: </b> <xsl:value-of select="/Site/@Name"/>
    </p>
    <p>
      <b>Build Name: </b> <xsl:value-of select="/Site/@BuildName"/>
    </p>
    <br/>
    <h3>
      <xsl:value-of select="count(Site/Testing/Test[@Status='passed'])"/> passed, 
      <xsl:value-of select="count(Site/Testing/Test[@Status='failed'])"/> failed,
      <xsl:value-of select="count(Site/Testing/Test[@Status='notrun'])"/> not run
    </h3>
    <br/>
    <table cellspacing="0">
      <tr>
        <th>Name</th>
        <th>Status</th>
      </tr>
      <xsl:for-each select="Site/Testing/Test[@Status='failed']">
        <xsl:sort select="Name"/>
        <tr>
          <td>
            <a>
              <xsl:attribute name="href">
                <xsl:call-template name="TranslateTestName">
                  <xsl:with-param name="Prefix"><xsl:value-of select="$DashboardDir"/>/TestDetail/</xsl:with-param>
                  <xsl:with-param name="TestName" select="FullName"/>
                  <xsl:with-param name="Postfix">.html</xsl:with-param>
                </xsl:call-template>
              </xsl:attribute><xsl:value-of select="Name"/>
            </a>
          </td>
          <td>
            <a>
              <xsl:attribute name="href">
                <xsl:call-template name="TranslateTestName">
                  <xsl:with-param name="Prefix">Results/</xsl:with-param>
                  <xsl:with-param name="TestName" select="FullName"/>
                  <xsl:with-param name="Postfix">.html</xsl:with-param>
                </xsl:call-template>
              </xsl:attribute>
              <font color="#FF0000"> Failed </font>
            </a>
          </td>
        </tr>
      </xsl:for-each>
      <tr>
        <td colspan="2"></td>
      </tr>
      <xsl:for-each select="Site/Testing/Test[@Status='notrun']">
        <xsl:sort select="Name"/>
        <tr>
          <td>
            <a>
              <xsl:attribute name="href">
                <xsl:call-template name="TranslateTestName">
                  <xsl:with-param name="Prefix"><xsl:value-of select="$DashboardDir"/>/TestDetail/</xsl:with-param>
                  <xsl:with-param name="TestName" select="FullName"/>
                  <xsl:with-param name="Postfix">.html</xsl:with-param>
                </xsl:call-template>
              </xsl:attribute>
            <xsl:value-of select="Name"/>
            </a>
          </td>
          <td>
            <font color="#FF0000"> Not Run</font>
          </td>
        </tr>
      </xsl:for-each>
      <tr>
        <td colspan="2"></td>
      </tr>
      <xsl:for-each select="Site/Testing/Test[@Status='passed']">
        <xsl:sort select="Name"/>
        <tr>
          <td>
            <a>
              <xsl:attribute name="HREF">
                <xsl:call-template name="TranslateTestName">
                  <xsl:with-param name="Prefix"><xsl:value-of select="$DashboardDir"/>/TestDetail/</xsl:with-param>
                  <xsl:with-param name="TestName" select="FullName"/>
                  <xsl:with-param name="Postfix">.html</xsl:with-param>
                </xsl:call-template>
              </xsl:attribute>
              <xsl:value-of select="Name"/>
            </a>
          </td>
          <td>
            <a>
              <xsl:attribute name="href">
                <xsl:call-template name="TranslateTestName">
                  <xsl:with-param name="Prefix">Results/</xsl:with-param>
                  <xsl:with-param name="TestName" select="FullName"/>
                  <xsl:with-param name="Postfix">.html</xsl:with-param>
                </xsl:call-template>
              </xsl:attribute>
              <font color="#00AA00">Passed</font>
            </a>
          </td>
        </tr>
      </xsl:for-each>
    </table>
    
    <xsl:for-each select="//Testing/Test">
      <xsl:call-template name="Test"/>
    </xsl:for-each>

    <xsl:call-template name="InsightFooter"/>

  </xsl:template>
    
  <xsl:template name="Test">
      <redirect:write select="concat(string('{$TestDocDir}'), '/Results/', translate ( FullName, '/.', '__' ) , '.html' )" file="dan.html">
      <xsl:call-template name="InsightHeader">
        <xsl:with-param name="Title">Test results for <xsl:value-of select="Name"/></xsl:with-param>
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
          <font color="#FF0000"> Failed</font>
        </xsl:when>
        <xsl:when test="contains('passed',@Status)">
          <font color="#00AA00"> Passed</font>
        </xsl:when>
      </xsl:choose>
      
      <pre>
        <xsl:value-of select="Results/Measurement/Value" disable-output-escaping="yes"/>
      </pre>
      <xsl:call-template name="InsightFooter"/>
    </redirect:write>
  </xsl:template>
    
<xsl:template name="Summary">
  <redirect:write select="concat(string('{$TestDocDir}'), '/TestSummary.xml' )">
    <Testing>
      <SiteName><xsl:value-of select="Site/@Name"/></SiteName>
      <BuildName><xsl:value-of select="Site/@BuildName"/></BuildName>
      <BuildStamp><xsl:value-of select="Site/@BuildStamp"/></BuildStamp>
      <StartDateTime><xsl:value-of select="Site/Testing/StartDateTime"/></StartDateTime>
      
      <PassedCount><xsl:value-of select="count(Site/Testing/Test[@Status='passed'])"/></PassedCount>
      <FailedCount><xsl:value-of select="count(Site/Testing/Test[@Status='failed'])"/></FailedCount>
      <NotRunCount><xsl:value-of select="count(Site/Testing/Test[@Status='notrun'])"/></NotRunCount>
      <EndDateTime><xsl:value-of select="Site/Testing/EndDateTime"/></EndDateTime>
      <Tests>
        <xsl:for-each select="Site/Testing/Test">
            <Test>
              <FullName><xsl:value-of select="FullName"/></FullName>
              <Status><xsl:value-of select="@Status"/></Status>
            </Test>
        </xsl:for-each>
      </Tests>
    </Testing>
  </redirect:write>
</xsl:template>

</xsl:stylesheet>
  