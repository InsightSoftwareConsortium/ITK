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
  <xsl:variable name="DashboardDir" select="concat('../', $DashboardStamp)"/>
  <xsl:param name="TestDocDir">.</xsl:param>

  <xsl:include href="Insight.xsl"/>
  <xsl:output method="html"/>

  <xsl:template match="/Update">
    <xsl:call-template name="Summary"/>
    <xsl:call-template name="InsightHeader">
      <xsl:with-param name="Title">Insight Update</xsl:with-param>
      <xsl:with-param name="IconDir">../../Icons</xsl:with-param>
      <xsl:with-param name="DashboardDir" select="$DashboardDir"/>
      <xsl:with-param name="UpdatesIcon">UpdatesBlue.gif</xsl:with-param>
    </xsl:call-template>

    <h3>Changed files as of  <xsl:value-of select="StartDateTime"/></h3>
      <xsl:call-template name="JavaScriptHeader"/>
    <a href="javascript:history.go(0)" onMouseOver="window.parent.status='Expand all';return true;" onClick="explode()">Expand all</a>
    <a href="javascript:history.go(0)" onMouseOver="window.parent.status='Collapse all';return true;" onClick="contract()">Collapse all</a>
    <p/>
    <script LANGUAGE="JavaScript">

      dbAdd (true, "Updated files  (<xsl:value-of select="count(/Update/Directory/Updated)"/>)", "", 0, "", "0")
      <xsl:for-each select="Directory">
        <xsl:sort select="Name"/>
        <xsl:if test="count(Updated)">
          dbAdd (true, "<b><xsl:value-of select="Name"/> (<xsl:value-of select="count(Updated)"/>)</b>", "", 1, "", "0")
          <xsl:for-each select="Updated">
            <xsl:sort select="Name"/>
            <xsl:call-template name="dbAdd">
              <xsl:with-param name="Level">2</xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </xsl:if>
      </xsl:for-each>

      dbAdd (true, "Modified files  (<xsl:value-of select="count(/Update/Directory/Modified)"/>)", "", 0, "", "0")
      <xsl:for-each select="Directory">
        <xsl:sort select="Name"/>
        <xsl:if test="count(Modified)">
          dbAdd (true, "<b><xsl:value-of select="Name"/> (<xsl:value-of select="count(Modified)"/>)</b>", "", 1, "", "0")
          <xsl:for-each select="Modified">
            <xsl:sort select="Name"/>
            <xsl:call-template name="dbAdd">
              <xsl:with-param name="Level">2</xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </xsl:if>
      </xsl:for-each>

      dbAdd (true, "Conflicting files  (<xsl:value-of select="count(/Update/Directory/Conflicting)"/>)", "", 0, "", "0")
      <xsl:for-each select="Directory">
        <xsl:sort select="Name"/>
        <xsl:if test="count(Conflicting)">
          dbAdd (true, "<b><xsl:value-of select="Name"/> (<xsl:value-of select="count(Conflicting)"/>)</b>", "", 1, "", "0")
          <xsl:for-each select="Conflicting">
            <xsl:sort select="Name"/>
            <xsl:call-template name="dbAdd">
              <xsl:with-param name="Level">2</xsl:with-param>
            </xsl:call-template>
          </xsl:for-each>
        </xsl:if>
      </xsl:for-each>

      </script>
    <xsl:call-template name="JavaScriptFooter"/>

    <a href="javascript:history.go(0)" onMouseOver="window.parent.status='Expand all';return true;" onClick="explode()">Expand all</a>
    <a href="javascript:history.go(0)" onMouseOver="window.parent.status='Collapse all';return true;" onClick="contract()">Collapse all</a>
    <xsl:call-template name="InsightFooter"/>

  </xsl:template>

  <xsl:template name="dbAdd">
    <xsl:param name="Level">1</xsl:param>
    <xsl:variable name="Level2"><xsl:value-of select="$Level + 1"/></xsl:variable>

    <xsl:choose>
      <xsl:when test="PriorRevision = Revision">
        dbAdd (true, "<xsl:value-of select="File"/> Rev: <xsl:value-of select="Revision"/> by <xsl:value-of select="Author"/>", "<xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/>?rev=<xsl:value-of select="Revision"/>&amp;content-type=text/x-cvsweb-markup", <xsl:value-of select="$Level"/>, "", "0")
      </xsl:when>
      <xsl:otherwise>
        dbAdd (true, "<xsl:value-of select="File"/> Rev: <xsl:value-of select="Revision"/> by <xsl:value-of select="Author"/>", "<xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/>.diff?r1=<xsl:value-of select="PriorRevision"/>&amp;r2=<xsl:value-of select="Revision"/>", <xsl:value-of select="$Level"/>, "", "0")
      </xsl:otherwise>
    </xsl:choose>

    dbAdd ( false, "<xsl:value-of select="translate ( normalize-space ( Log ), '&quot;', ' ' )"/>", "", <xsl:value-of select="$Level2"/>, "", "1" )
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


  <xsl:template name="Summary">
    <redirect:write select="concat(string('{$TestDocDir}'), '/UpdateSummary.xml' )">
      <xsl:text disable-output-escaping="yes">&lt;?xml version="1.0" encoding="ISO-8859-1"?&gt;
</xsl:text>
      <Update>
        <StartDateTime><xsl:value-of select="StartDateTime"/></StartDateTime>
        <ChangedFileCount><xsl:value-of select="count(Directory/Updated|Directory/Modified|Directory/Conflicting)"/></ChangedFileCount>
        <AuthorCount><xsl:value-of select="count(Author)"/></AuthorCount>
        <DirectoryCount><xsl:value-of select="count(Directory)"/></DirectoryCount>
      </Update>
    </redirect:write>
  </xsl:template>


</xsl:stylesheet>