<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:output method="html" indent="yes"/>

  <!--
       Use DashboardStamp as a parameter, default to most recent
       The proper flags to Xalan are in the form -PARAM DashboardStamp "string('foo')"
       -->
  <xsl:param name="DashboardStamp" select="string('MostRecentResults-Nightly')"/>
  <xsl:variable name="DashboardDir" select="concat('../../../../Dashboard/', $DashboardStamp)"/>

  <xsl:variable name="CVSWebURL">http://public.kitware.com/cgi-bin/itkcvsweb.cgi/Insight/</xsl:variable>
  <xsl:include href="Insight.xsl"/>

  <xsl:template match="/Update">
    <html>
      <head>
        <title>Changed files</title>
        <style>
          #foldheader{cursor:hand ; font-weight:bold ;
          list-style-image:url "fold.gif"}
          #foldinglist{list-style-image:url(list.gif)}
        </style>
      </head>

      <body bgcolor="#ffffff">
        <table border="4" cellpading="0" cellspacing="2" width="100%">
          <tr>
            <td width="140">
              <a href="Dashboard.html"><img src="../../Icons/Logo.gif" border="0"></img></a>
            </td>
            <td>
              <h1>Insight Nightly Testing Dashboard</h1>
            </td>
          </tr>
          <tr>
            <td width="23%" valign="top" halign="center">
              <table width="100%">
                <tr>
                  <td>
                    <img src="../../Icons/UpdatesBlue.gif" border="0"></img>
                  </td>
                </tr>
              <tr>
                <td>
                  <a href="BuildError.html"><img src="../../Icons/Errors.gif" border="0"></img></a>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="BuildWarning.html"><img src="../../Icons/Warnings.gif" border="0"></img></a>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="test.html"><img src="../../Icons/Tests.gif" border="0"></img></a>
                </td>
              </tr>
              <tr>
                <td>
                  <a href="coverage.html"><img src="../../Icons/Coverage.gif" border="0"></img></a>
                </td>
              </tr>
            </table>
            <hr width="75%"/>
            <table border="0" cellpadding="0" cellspacing="0" width="100%">
              <tr>
                <td>
                  <a href="Dashboard.html"><img src="../../Icons/Home.gif" border="0"></img></a> 
                </td>
              </tr>
            </table>

          </td>	    
          <td>		       
          <h3>Insight Changed Files - <xsl:value-of select="StartDateTime"/></h3>
          <xsl:call-template name="JavaScriptHeader"/>
          <script LANGUAGE="JavaScript">
            dbAdd (true, "Updated files  (<xsl:value-of select="count(Updated)"/>)", "", 0, "", 0)

            <xsl:for-each select="Updated">
              <xsl:call-template name="dbAdd"/>
            </xsl:for-each>
            dbAdd (true, "Modified files  (<xsl:value-of select="count(Modified)"/>)", "", 0, "", 0)
            <xsl:for-each select="Modified">
              <xsl:call-template name="dbAdd"/>
            </xsl:for-each>
            dbAdd (true, "Conflicting files  (<xsl:value-of select="count(Conflicting)"/>)", "", 0, "", 0)
            <xsl:for-each select="Conflicting">
              <xsl:call-template name="dbAdd"/>
            </xsl:for-each>

          </script>
          <xsl:call-template name="JavaScriptFooter"/>

        </td>
      </tr>
    </table>

  </body>
</html>
</xsl:template>

<!--
  <xsl:template match="Updated|Conflicting|Modified">
    <br/>
    <strong>
      <a><xsl:attribute name="name"><xsl:value-of select="FullName"/></xsl:attribute></a>
      <a><xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/></xsl:attribute><xsl:value-of select="File"/></a>
    </strong> by <a><xsl:attribute name="href">#<xsl:value-of select="Author"/></xsl:attribute><xsl:value-of select="Author"/></a> in <a><xsl:attribute name="href">#<xsl:value-of select="File/@Directory"/></xsl:attribute><xsl:value-of select="File/@Directory"/></a>
    Revision: 
    <a><xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/>?rev=<xsl:value-of select="Revision"/>&amp;content-type=text/x-cvsweb-markup</xsl:attribute><xsl:value-of select="Revision"/></a>
    
    <xsl:if test="count(PriorRevision) != 0">
      Diff to Previous:
      <a><xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/>.diff?r1=<xsl:value-of select="PriorRevision"/>&amp;r2=<xsl:value-of select="Revision"/></xsl:attribute>
      <xsl:value-of select="PriorRevision"/></a>
    </xsl:if>
    <br/>
    <pre>
      <xsl:value-of select="Log"/>
    </pre>
  </xsl:template>
-->

  <xsl:template name="dbAdd">
    dbAdd (true, "<xsl:value-of select="File/@Directory"/><xsl:text>   </xsl:text><b><xsl:value-of select="File"/></b>", "<xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/>.diff?r1=<xsl:value-of select="PriorRevision"/>&amp;r2=<xsl:value-of select="Revision"/>", 1, "", 0)
    dbAdd ( false, "Author: <xsl:value-of select="Author"/>", "", 2, "", 0 )
    dbAdd ( false, "Log: <xsl:value-of select="normalize-space ( Log )"/>", "", 2, "", 0 )
  </xsl:template>

<xsl:template match="Updated|Conflicting|Modified">
  <hr/>
  <strong>
    <a><xsl:attribute name="name"><xsl:value-of select="FullName"/></xsl:attribute></a>
    <a><xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/></xsl:attribute><xsl:value-of select="File"/></a>
  </strong> by <a><xsl:attribute name="href">#<xsl:value-of select="Author"/></xsl:attribute><xsl:value-of select="Author"/></a> in <a><xsl:attribute name="href">#<xsl:value-of select="File/@Directory"/></xsl:attribute><xsl:value-of select="File/@Directory"/></a>
  Revision: 
  <a><xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/>?rev=<xsl:value-of select="Revision"/>&amp;content-type=text/x-cvsweb-markup</xsl:attribute><xsl:value-of select="Revision"/></a>

  <xsl:if test="count(PriorRevision) != 0">
    Diff to Previous:
    <a><xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/>.diff?r1=<xsl:value-of select="PriorRevision"/>&amp;r2=<xsl:value-of select="Revision"/></xsl:attribute>
    <xsl:value-of select="PriorRevision"/></a>
  </xsl:if>
  <br/>

  <pre>
    <xsl:value-of select="Log"/>
  </pre>
</xsl:template>


<xsl:template match="Author|Directory">
  <br/>
  <h4>
    <a>
      <xsl:attribute name="name"><xsl:value-of select="Name"/></xsl:attribute>
      <xsl:value-of select="Name"/>
    </a>
  </h4>
  <br/>
  <xsl:for-each select="File">
    <a><xsl:attribute name="href">#<xsl:value-of select="@Directory"/></xsl:attribute><xsl:value-of select="@Directory"/></a><xsl:text>  /  </xsl:text><a><xsl:attribute name="href">#<xsl:value-of select="@Directory"/>/<xsl:value-of select="."/></xsl:attribute><xsl:value-of select="."/></a>
  <br/>
  </xsl:for-each>
</xsl:template>


</xsl:stylesheet>