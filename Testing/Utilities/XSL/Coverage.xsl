<?xml version="1.0" encoding="ISO-8859-1"?>
<!DOCTYPE xsl:stylesheet>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:template match="/">
    <html>
      <head>
        <title>Coverage log</title>
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
	 	<a href="dashboard.html"> <img src="Images/logo.gif" border="0"></img></a>
              </td>
	      <td>
               <h1>Insight testing dashboard</h1>
        	</td>
	      </tr>
	     <tr>
	      <td width="23%" valign="top" halign="center">
	<table width="100%">
              <tr><td>
              <a href="update.html"><img src="Images/Updates.gif" border="0"></img></a>
              </td></tr>
               <tr><td>
              <a href="BuildError.html"><img src="Images/Errors.gif" border="0"></img></a>
              </td></tr>
              <tr><td>
              <a href="BuildWarning.html"><img src="Images/Warnings.gif" border="0"></img></a>
              </td></tr>
                <tr><td>
              <a href="test.html"><img src="Images/Tests.gif" border="0"></img></a>
              </td></tr>
              <tr><td>
              <img src="Images/Coverageblue.gif" border="0"></img>
               </td></tr></table>
            <hr width="75%"></hr>
<table border="0" cellpadding="0" cellspacing="0" width="100%">
<tr><td><a href="dashboard.html"><img src="Images/Home.gif" border="0"></img></a> 
 </td></tr></table>
</td>		    
<td>		        
<h3>Coverage started on <xsl:value-of select="Site/Coverage/StartDateTime"/></h3>
<table border="4" cellpadding="0" cellspacing="2" width="250">
    <tr>
      <td align="left" width="180"> Total Coverage</td> 
      <td>
	<xsl:choose>
	  <xsl:when test="Site/Coverage/PercentCoverage &lt; 50">
	    <xsl:attribute name="bgcolor">#ff7f50</xsl:attribute>
	  </xsl:when>
	  <xsl:otherwise>
	    <xsl:attribute name="bgcolor">#00ff7f</xsl:attribute>
	  </xsl:otherwise>
	</xsl:choose> <xsl:value-of select="Site/Coverage/PercentCoverage"/>%</td>
</tr>    
 <tr>
       <td align="left"> Tested lines</td>
       <td><xsl:value-of select="Site/Coverage/LOCTested"/></td>
  </tr>
  <tr>
        <td align="left"> Untested lines</td> 
        <td> <xsl:value-of select="Site/Coverage/LOCUntested"/></td>
  </tr></table>
  <hr></hr>
  <h2>Level of coverage</h2>      
 <ul>
   <li id="foldheader">News</li>
   <ul id="foldinglist" style="display:none">
      <li><a href="http://www.cnn.com">CNN</a></li>
      <li><a href="http://www.abcnews.com">ABC News</a></li>
      <li><a href="http://www.vancouversun.com">Vancouver Sun</a></li>
   </ul>

   <li id="foldheader">Games</li>
   <ul id="foldinglist" style="display:none">
      <li><a href="http://www.gamespot.com">GameSpot</a></li>
      <li><a href="http://www.happypuppy.com">Happy Puppy</a></li>
      <li><a href="http://www.gamecenter.com">Game Center</a></li>
   </ul>

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
 <xsl:value-of select="count(File[@Covered='true'])"/> Files Covered
        <xsl:value-of select="count(Directory//File)"/> Files Not Covered
        <hr/>


        <table>
          <tr>
            <th>Filename</th>
            <th>Lines Covered</th>
            <th>Lines Not Covered</th>
            <th>Percentage</th>
            <th>Date</th>
          </tr>

          <xsl:apply-templates select="Site/Coverage/Directory"/>
        </table>
</td></tr>
</table>
      </body>
    </html>
  </xsl:template>

  <xsl:template match="Directory">
    <tr>
      <th>
        <xsl:attribute name="align">left</xsl:attribute>
        <xsl:value-of select="@FullPath"/>
      </th>
    </tr>
    <xsl:if test="count(File) > 0">
      <xsl:for-each select="File">
        <xsl:sort select="@Covered" order="descending"/>
        <xsl:sort select="@Name"/>
        <xsl:choose>
          <xsl:when test="@Covered='true'">
            <tr>
              <td><xsl:value-of select="@Name"/></td>
              <td><xsl:value-of select="LOCTested"/></td>
              <td><xsl:value-of select="LOCUntested"/></td>
              <td><xsl:value-of select="PercentCoverage"/>%</td>
            </tr>
          </xsl:when>
          <xsl:when test="@Covered='false'">
            <tr>
              <xsl:attribute name="bgcolor">#FF6666</xsl:attribute>
              <td><xsl:value-of select="@Name"/></td>
              <td>
                <xsl:attribute name="colspan">3</xsl:attribute>
                UNTESTED
              </td>
            </tr>
          </xsl:when>
        </xsl:choose>
      </xsl:for-each>
    </xsl:if>
    <xsl:apply-templates select="Directory"/>
    
  </xsl:template>

  <xsl:template match="File|LOCTested|LOCUntested|StartDateTime|EndDateTime|PercentCoverage"/>

</xsl:stylesheet>