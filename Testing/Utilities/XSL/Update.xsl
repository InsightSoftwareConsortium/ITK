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

  <xsl:template match="/Update">
    <html>
      <head>
        <title>Changed files</title>
        <style>
          #foldheader{cursor:hand ; font-weight:bold ;
          list-style-image:url "fold.gif"}
          #foldinglist{list-style-image:url(list.gif)}
        </style>
        <script language="JavaScript1.2">
          var head="display:''"
          img1=new Image()
          img1.src="fold.gif"
          img2=new Image()
          img2.src="open.gif"
          function change(){
          if(!document.all)
          return
          if (event.srcElement.id=="foldheader") {
          var srcIndex = event.srcElement.sourceIndex
          var nested = document.all[srcIndex+1]
          if (nested.style.display=="none") {
          nested.style.display=''
          event.srcElement.style.listStyleImage="url(open.gif)"
          }
          else {
          nested.style.display="none"
          event.srcElement.style.listStyleImage="url(fold.gif)"
          }
          }
          }
          document.onclick=change

        </script>
      </head>

      <body bgcolor="#ffffff">
        <table border="4" cellpading="0" cellspacing="2" width="100%">
          <tr>
            <td width="140">
              <a href="Dashboard.html"> <img src="../../Icons/Logo.gif" border="0"></img></a>
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
          <table border="4" cellpadding="0" cellspacing="2" width="250">
            <tr>
              <td align="left" width="180">Update/Patched file(s)</td>
              <td align="center">
                <a><xsl:attribute name="HREF">#Updated</xsl:attribute><xsl:value-of select="count(Updated)"/></a>
              </td>
            </tr>
            <tr> 
              <td align="left">Conflicting file(s)</td>
              <td align="center">
                <a><xsl:attribute name="HREF">#Conflicting</xsl:attribute><xsl:value-of select="count(Conflicting)"/></a> 
              </td>
            </tr>
            <tr>
              <td align="left"> Locally modified file(s)</td>
              <td align="center">
                <a><xsl:attribute name="HREF">#Locallymodified</xsl:attribute><xsl:value-of select="count(Modified)"/></a>
              </td>
            </tr>
          </table>	    
          <hr/>
          <a name="Updated"></a><h2>Updated/Patched file</h2>
          
          <ul>
            <xsl:for-each select="Directory">   
            <li id="foldheader">
              <xsl:value-of select="Name"/>
            </li>
            <xsl:for-each select="File">
              <ul id="foldinglist" style="display:none">
                <li id="foldheader"><xsl:value-of select="."/></li>
                <ul id="foldinglist" style="display:none">
                  <li><a href="http://www.windows95.com">nested 1</a></li>
                  <li><a href="http://www.shareware.com">nested 2</a></li>  
                </ul>
              </ul>
            </xsl:for-each>
          </xsl:for-each>
        </ul>

        <!--
             <ul>
               <li id="foldheader"><xsl:apply-templates select="Directory"/></li>
               
               <li id="foldheader">Software</li>
               <ul id="foldinglist" style="display:none">
                 <li><a href="http://www.download.com">outer 1</a></li>
                 <li><a href="http://www.hotfiles.com">outer 2</a></li>
                 <li id="foldheader">Nested</li>
                 <ul id="foldinglist" style="display:none">
                   <li><a href="http://www.windows95.com">nested 1</a></li>
                   <li><a href="http://www.shareware.com">nested 2</a></li>
                 </ul>
                 <li><a href="http://www.windows95.com">outer 3</a></li>
                 <li><a href="http://www.shareware.com">outer 4</a></li>
               </ul>
               
             </ul>
             -->


        <script language="JavaScript1.2">
          <!--
               function get_cookie(Name) {
               var search = Name + "="
               var returnvalue = "";
               if (document.cookie.length > 0) {
               offset = document.cookie.indexOf(search)
               // if cookie exists
               if (offset != -1) { 
               offset += search.length
               // set index of beginning of value
               end = document.cookie.indexOf(";", offset);
               // set index of end of cookie value
               if (end == -1) end = document.cookie.length;
               returnvalue=unescape(document.cookie.substring(offset, end))
               }
               }
               return returnvalue;
               }

               if (get_cookie(window.location.pathname) != '') {
               var openresults = get_cookie(window.location.pathname).split(" ");
               for (i=0; i < openresults.length; i++) {
               foldinglist[openresults[i]].style.display=''
               document.all[foldinglist[openresults[i]].sourceIndex - 1].style.listStyleImage="url(open.gif)";
               }
               }

               if (document.all){
               var nodelength=foldinglist.length-1
               var nodes=new Array(nodelength)
               var openones=''
               }
               function check(){
               for (i=0 ; i <= nodelength ; i++){
               if (foldinglist[i].style.display=='')
               openones=openones + " " + i
               }
               document.cookie=window.location.pathname+"="+openones
               }
               if (document.all)
               document.body.onunload=check
               //-->
        </script>


        <br/>


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
 

        <table border="4" cellpadding="0" cellspacing="2" width="100%"> 
        <tr>
          <td>
            <center><b>#</b></center>
          </td>
          <td>
            <center><b>File Name</b></center>
          </td>
          <td>
            <center><b>Author</b></center>
          </td>
          <td>
            <center><b>Directory</b></center>
          </td>
          <td>
            <center><b>Modification</b></center>
          </td>
          <td>
            <center><b>New version</b></center>
          </td>
        </tr>
        <xsl:for-each select="Updated"> 
        <tr bgcolor="#bbbbbb">
          <td><xsl:number level="single" count="Updated" format="1"/>
          <xsl:choose>
            <xsl:when test="bgcolor=#bbbbbb"> bgcolor="#ffffff"</xsl:when>
            <xsl:otherwise> bgcolor="#bbbbbb"</xsl:otherwise>
          </xsl:choose>
        </td>
        <td>
          <strong>
            <a><xsl:attribute name="name"><xsl:value-of select="FullName"/></xsl:attribute></a>
            <a><xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/></xsl:attribute><xsl:value-of select="File"/></a>
          </strong>
        </td>
        <td>
          <a><xsl:attribute name="href">#<xsl:value-of select="Author"/></xsl:attribute><xsl:value-of select="Author"/></a>
        </td>
        <td>
          <a><xsl:attribute name="href">#<xsl:value-of select="File/@Directory"/></xsl:attribute><xsl:value-of select="File/@Directory"/></a>
        </td>
        <td>
          <xsl:if test="count(PriorRevision) != 0">
            Modified from: 
            <a><xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:text>Modified from:</xsl:text><xsl:value-of select="FullName"/>.diff?r1=<xsl:value-of select="PriorRevision"/>&amp;r2=<xsl:value-of select="Revision"/></xsl:attribute>
            <xsl:value-of select="PriorRevision"/></a>
          </xsl:if>
        </td>
        <td> 
        <a><xsl:attribute name="href"><xsl:value-of select="$CVSWebURL"/><xsl:value-of select="FullName"/>?rev=<xsl:value-of select="Revision"/>&amp;content-type=text/x-cvsweb-markup</xsl:attribute><xsl:value-of select="Revision"/></a>
      </td>	
    </tr>
  </xsl:for-each>
</table>


	

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
</td></tr></table>

</body>
</html>


</xsl:template>

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