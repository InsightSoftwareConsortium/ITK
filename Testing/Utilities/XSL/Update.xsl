<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output method="html" indent="yes"/>

<xsl:variable name="CVSWebURL">http://public.kitware.com/cgi-bin/itkcvsweb.cgi/Insight/</xsl:variable>

<xsl:template match="/Update">
<html>
<head>
  <title>Changed files</title>
</head>
<body bgcolor="#ffffff">

<h1>Insight Changed Files - <xsl:value-of select="StartDateTime"/></h1>

<br/>
<a>
  <xsl:attribute name="HREF">#Updated</xsl:attribute><xsl:value-of select="count(Updated)"/>
</a>
<xsl:text> Updated/Patched files </xsl:text>
<a>
  <xsl:attribute name="HREF">#Conflicting</xsl:attribute><xsl:value-of select="count(Conflicting)"/>
</a> 
<xsl:text> Conflicting files</xsl:text> 
<xsl:value-of select="count(Modified)"/> Locally modified

<br/>
<h3>Updated files listed by <a href="#directory">directory</a></h3>

  <xsl:for-each select="Directory">
  <strong><xsl:value-of select="Name"/></strong> had <a><xsl:attribute name="href">#<xsl:value-of select="Name"/></xsl:attribute><xsl:value-of select="count(File)"/></a> updated files<br/>
  </xsl:for-each>
<hr/>

<h3>Updated by <a><xsl:attribute name="href">#author</xsl:attribute>author</a></h3>
<xsl:for-each select="Author">
  <strong><xsl:value-of select="Name"/></strong> had <a><xsl:attribute name="href">#<xsl:value-of select="Name"/></xsl:attribute><xsl:value-of select="count(File)"/></a> updated files<br/>
  </xsl:for-each>

<hr/>
<h2>Summary</h2>
<xsl:for-each select="Updated">
  <br/>
  <strong><a><xsl:attribute name="name"><xsl:value-of select="FullName"/></xsl:attribute></a>
  <a><xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/></xsl:attribute><xsl:value-of select="File"/></a>
  </strong> by <a><xsl:attribute name="href">#<xsl:value-of select="Author"/></xsl:attribute><xsl:value-of select="Author"/></a> in <a><xsl:attribute name="href">#<xsl:value-of select="File/@Directory"/></xsl:attribute><xsl:value-of select="File/@Directory"/></a>
    Revision: 
    <a><xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/>?rev=<xsl:value-of select="Revision"/>&amp;content-type=text/x-cvsweb-markup</xsl:attribute><xsl:value-of select="Revision"/></a>

  <xsl:if test="count(PriorRevision) != 0">
    Diff to Previous:
    <a><xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/>.diff?r1=<xsl:value-of select="PriorRevision"/>&amp;r2=<xsl:value-of select="Revision"/></xsl:attribute>
    <xsl:value-of select="PriorRevision"/></a>
  </xsl:if>
</xsl:for-each>


<h2><a><xsl:attribute name="name">author</xsl:attribute>Updated by Author</a></h2>
  <xsl:apply-templates select="Author"/>
<hr/>

<h2><a><xsl:attribute name="name">directory</xsl:attribute>Updated by Directory</a></h2>
  <xsl:apply-templates select="Directory"/>
<hr/>

<h2><a><xsl:attribute name="name">Updated</xsl:attribute>Updated</a></h2>
  <xsl:apply-templates select="Updated"/>
<hr/>

<h2><a><xsl:attribute name="name">Modified</xsl:attribute>Modified</a></h2>
  <xsl:apply-templates select="Modified"/>
<hr/>

<h2><a><xsl:attribute name="name">Conflicting</xsl:attribute>Conflicting</a></h2>
  <xsl:apply-templates select="Conflicting"/>
<hr/>


</body>
</html>


</xsl:template>

<xsl:template match="Updated|Conflicting|Modified">
  <br/>
  <strong><a><xsl:attribute name="name"><xsl:value-of select="FullName"/></xsl:attribute></a>
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