<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:template match="/">
    <html>
      <head>
        <title>Build log</title>
      </head>
      <body bgcolor="#ffffff">
        <table border="4" cellpading="0" cellspacing="2" width="100%">
	    <tr>
	      <td width="140">
	 	<a href="dashboard.html"> <img src="../../../../Icons/Logo.gif" border="0"></img></a>
              </td>
	      <td>
               <h1>Insight build log dashboard</h1>
        	</td>
	      </tr>
	   <tr> <td width="23%" valign="top" halign="center">
	<table width="100%">
              <tr><td>
             <a href="Update.html"> <img src="../../../../Icons/Updates.gif" border="0"></img></a>
              </td></tr>
               <tr><td>
              <a href="BuildError.html"><img src="../../../../Icons/Errors.gif" border="0"></img></a>
              </td></tr>
              <tr><td>
              <img src="../../../../Icons/WarningsBlue.gif" border="0"></img>
              </td></tr>
                <tr><td>
              <a href="Test.html"><img src="../../../../Icons/Tests.gif" border="0"></img></a>
              </td></tr>
              <tr><td>
              <a href="Coverage.html"><img src="../../../../Icons/Coverage.gif" border="0"></img></a>
             </td></tr></table>
            <hr width="75%"></hr>
<table width="100%">
<tr><td><a href="Dashboard.html"><img src="../../../../Icons/Home.gif" border="0"></img></a> 
 </td></tr></table>
</td>		    
<td>
<h2>Build started on <xsl:value-of select="Site/Build/StartDateTime"/></h2>
        <h3>Found 
        <a href="#Error">
          <xsl:value-of select="count(Site/Build/Error)"/> Errors
        </a>
        and 
        <a href="#Warning">
          <xsl:value-of select="count(Site/Build/Warning)"/> Warnings
        </a>
      </h3>
      <br/>
      <hr/>
      <a name="Error">
        <h2>Errors</h2>
      </a>
      <xsl:for-each select="Site/Build/Error">
        <hr/>
        <h3>Error # <xsl:number level="single" count="Site/Build/Error" format="1"/>: Build Log line <xsl:value-of select="BuildLogLine"/></h3>
        <br/>
        <xsl:call-template name="FormatContext"/>
      </xsl:for-each>

      <hr/>
      <a name="Warning">
        <h2>Warnings</h2>
      </a>
      <xsl:for-each select="//Build/Warning">
        <hr/>
        <h3>Warning # <xsl:number level="single" count="//Build/Warning" format="1"/>: Build Log line <xsl:value-of select="BuildLogLine"/></h3>
        <br/>
        <xsl:call-template name="FormatContext"/>
      </xsl:for-each>
    </td>
  </tr>
</table>
      
</body>
</html>
</xsl:template>
	

<xsl:template name="FormatContext">
  <xsl:choose>
    <xsl:when test="SourceFile != ''">
      File: 
      <b><xsl:value-of select="SourceFile"/></b>
      Line: 
      <b><xsl:value-of select="SourceLineNumber"/></b>
    </xsl:when>
  </xsl:choose>
  <pre>
    <xsl:value-of select="PreContext"/>
    <b><xsl:value-of select="Text"/></b>
    <xsl:value-of select="PostContext"/>
  </pre>
</xsl:template>

</xsl:stylesheet>