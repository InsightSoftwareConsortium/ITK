<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

  <xsl:param name="DashboardDate" select="string('')"/>

  <xsl:variable name="Red">#ffcc66</xsl:variable>
  <xsl:variable name="Green">#00ff7f</xsl:variable>
  <xsl:variable name="Yellow">#ffff99</xsl:variable>
  <xsl:variable name="LightBlue">#b0c4de</xsl:variable>
  <xsl:variable name="CVSWebURL">http://public.kitware.com/cgi-bin/itkcvsweb.cgi/Insight/</xsl:variable>
  <xsl:variable name="DoxygenURL">http://public.kitware.com/Insight/Doxygen/html/</xsl:variable>

  <xsl:template name="InsightFooter">
    <xsl:text disable-output-escaping="yes">
            &lt;/td>
          &lt;/tr>
        &lt;/table>
      &lt;/font>
      &lt;/body>
    &lt;/html>
    </xsl:text>
  </xsl:template>


  <xsl:template name="InsightHeader">
    <xsl:param name="Title">Insight Dashboard</xsl:param>
    <xsl:param name="IconDir">../../Icons</xsl:param>
    <xsl:param name="DashboardDir">./</xsl:param>
    <xsl:param name="UpdatesIcon">Updates.gif</xsl:param>
    <xsl:param name="TestsIcon">Tests.gif</xsl:param>
    <xsl:param name="CoverageIcon">Coverage.gif</xsl:param>
    <xsl:param name="HomeIcon">Home.gif</xsl:param>

    <xsl:text disable-output-escaping="yes">
    &lt;html>
      &lt;head>
        &lt;title>
    </xsl:text>
    <xsl:value-of select="$Title"/> -- <xsl:value-of select="$DashboardDate"/>
    <xsl:text disable-output-escaping="yes">
        &lt;/title>
      &lt;/head>
      &lt;body bgcolor="#ffffff">
        &lt;font face="Arial,Helvetica">
        &lt;table border="0" cellpading="0" cellspacing="2" width="100%">
          &lt;tr>
            &lt;td width="140">
      &lt;a href="</xsl:text><xsl:value-of select="$DashboardDir"/><xsl:text disable-output-escaping="yes">/Dashboard.html">
              &lt;img src="</xsl:text>
    <xsl:value-of select="$IconDir"/>
    <xsl:text disable-output-escaping="yes">/Logo.gif" border="0">&lt;/img>&lt;/a>
            &lt;/td>
            &lt;td bgcolor="#6699cc" valign="top">
    &lt;h2>&lt;font color="#ffffff"></xsl:text><xsl:value-of select="$Title"/><xsl:text disable-output-escaping="yes">&lt;/font>&lt;/h2>&lt;h3></xsl:text><xsl:value-of select="$DashboardDate"/><xsl:text disable-output-escaping="yes">&lt;/h3>
            &lt;/td>
          &lt;/tr>
          &lt;tr>
            &lt;td valign="top" halign="center">
              &lt;table width="100%" halign="center">
                &lt;tr>
                  &lt;td align="center">
                    &lt;a href="</xsl:text>
    <xsl:value-of select="$DashboardDir"/>
    <xsl:text disable-output-escaping="yes">/Update.html">&lt;img src="</xsl:text>
    <xsl:value-of select="$IconDir"/>/<xsl:value-of select="$UpdatesIcon"/> <xsl:text disable-output-escaping="yes">" border="0">&lt;/img>&lt;/a>
                  &lt;/td>
                &lt;/tr>
                &lt;tr>
                  &lt;td align="center">
                    &lt;a href="</xsl:text>
    <xsl:value-of select="$DashboardDir"/>
    <xsl:text disable-output-escaping="yes">/TestOverview.html">&lt;img src="</xsl:text>
    <xsl:value-of select="$IconDir"/>/<xsl:value-of select="$TestsIcon"/><xsl:text disable-output-escaping="yes">" border="0">&lt;/img>&lt;/a>
                  &lt;/td>
                &lt;/tr>
                &lt;tr>
		&lt;td align="center">
                &lt;a href="http://public.kitware.com/cgi-bin/itkcvsweb.cgi/Insight/">
    &lt;img src="</xsl:text>
    <xsl:value-of select="$IconDir"/>
    <xsl:text disable-output-escaping="yes">/CVS.gif" border="0">&lt;/img>&lt;/a>
                  &lt;/td>
&lt;/tr>
		&lt;tr>
		&lt;td align="center">
                    &lt;a href="http://public.kitware.com/Insight/Doxygen/html/">&lt;img src="</xsl:text>
    <xsl:value-of select="$IconDir"/>
    <xsl:text disable-output-escaping="yes">/Doxygen.gif" border="0">&lt;/img>&lt;/a>
                  &lt;/td>
&lt;/tr>
		&lt;tr>
		&lt;td align="center">
                    &lt;a href="</xsl:text>
    <xsl:value-of select="$DashboardDir"/>
    <xsl:text disable-output-escaping="yes">/Dashboard.html">
&lt;img src="</xsl:text>
    <xsl:value-of select="$IconDir"/>/<xsl:value-of select="$HomeIcon"/><xsl:text disable-output-escaping="yes">" border="0">&lt;/img>&lt;/a>

                  &lt;/td>
              &lt;/table>
            &lt;/td>		    
            &lt;td valign="top">
            </xsl:text>
            </xsl:template>



<xsl:template name="JavaScriptHeader">
<script LANGUAGE="JavaScript">
<xsl:text disable-output-escaping="yes">
<![CDATA[<!--
var total=1;
var db = new Array();
  var Icons = "../../Icons/";

// -- Enter Values Here --
// Format: dbAdd(parent[true|false] , description, URL [blank for nohref], level , TARGET [blank for "content"], image [1=yes])

// Get current cookie setting
var current=getCurrState()
function getCurrState() {
  var label = "currState="
  var labelLen = label.length
  var cLen = document.cookie.length
  var i = 0
  while (i < cLen) {
    var j = i + labelLen
    if (document.cookie.substring(i,j) == label) {
      var cEnd = document.cookie.indexOf(";",j)
      if (cEnd == -1) { cEnd = document.cookie.length }
      return unescape(document.cookie.substring(j,cEnd))
    }
    i++
  }
  return ""
}

// Add an entry to the database
function dbAdd(mother,display,URL,indent,top,open) {
  db[total] = new Object;
  db[total].mother = mother
  db[total].display = display
  db[total].URL = URL
  db[total].indent = indent
  db[total].top = top
  db[total].open = open
  db[total].image = ""
  total++
  }

// Record current settings in cookie
function setCurrState(setting) {
  var expire = new Date();
  expire.setTime(expire.getTime() + ( 60*60*1000 ) ); // expire in 1 hour
  document.cookie = "currState=" + escape(setting) + "; expires=" + expire.toGMTString();
  }

// toggles an outline mother entry, storing new value in the cookie
function toggle(n) {
  if (n != 0) {
    var newString = ""
    var expanded = current.substring(n-1,n) // of clicked item
    newString += current.substring(0,n-1)
    newString += expanded ^ 1 // Bitwise XOR clicked item
    newString += current.substring(n,current.length)
    setCurrState(newString) // write new state back to cookie
  }
}

// returns padded spaces (in mulTIPles of 2) for indenting
function pad(n) {
  var result = ""
  for (var i = 1; i <= n; i++) { result += "&nbsp;&nbsp;&nbsp;&nbsp;" }
  return result
}

// Expand everything
function explode() {
  current = "";
  initState="";
  for (var i = 1; i < db.length; i++) { 
    initState += "1"
    current += "1"
    }
  setCurrState(initState);
  history.go(0);
  }

// Collapse everything
function contract() {
  current = "";
  initState="";
  for (var i = 1; i < db.length; i++) { 
    initState += "0"
    current += "0"
    }
  setCurrState(initState);
  history.go(0);
  }

function tree_close() {
  window.parent.location = window.parent.content.location;
  }

//end -->]]>
</xsl:text>
</script>

</xsl:template>

<xsl:template name="JavaScriptFooter">
<script LANGUAGE="JavaScript">
<xsl:text disable-output-escaping="yes">
<![CDATA[<!--
	// Set the initial state if no current state or length changed
	if (current == "" || current.length != (db.length-1)) {
	current = ""
	initState = ""
	for (i = 1; i < db.length; i++) { 
	initState += "0"
	current += "0"
	}
	setCurrState(initState)
	}
	var prevIndentDisplayed = 0
	var showMyDaughter = 0
	// end -->
	 
	<!--
      	var Outline=""
	// cycle through each entry in the outline array
	for (var i = 1; i < db.length; i++) {
	  var currIndent = db[i].indent           // get the indent level
	  var expanded = current.substring(i-1,i) // current state
	 var top = db[i].top
		 if (top == "") { top="content" }
		// display entry only if it meets one of three criteria
			if ((currIndent == 0 || currIndent <= prevIndentDisplayed || (showMyDaughter == 1 && (currIndent - prevIndentDisplayed == 1)))) {
			Outline += pad(currIndent)

		// Insert the appropriate GIF and HREF
		 image = "Blank";
		 if (db[i].image==1) { image="_bullet"; }
		 if (db[i].image==2) { image="_search"; }
		 if (db[i].image==3) { image="_cal"; }
		 if (db[i].image==4) { image="_upd"; }
		 if (db[i].image==5) { image="_admin"; }
		 if (!(db[i].mother)) {
    Outline += ""
		  } 
		 else { 
		  if (current.substring(i-1,i) == 1) {
			Outline += "<A HREF=\"javascript:history.go(0)\" onMouseOver=\"window.parent.status=\'Click to collapse\';return true;\" onClick=\"toggle(" + i + ")\">"
			Outline += "<IMG SRC=\"" + Icons + "Minus.gif\" WIDTH=16 HEIGHT=16 BORDER=0><IMG SRC=\"" + Icons + "Open.gif\" WIDTH=16 HEIGHT=16 BORDER=0>"
			Outline += "</A>"
			}
		  else {
			Outline += "<A HREF=\"javascript:history.go(0)\" onMouseOver=\"window.parent.status=\'Click to expand\';return true;\" onClick=\"toggle(" + i + ")\">"
			Outline += "<IMG SRC=\"" + Icons + "Plus.gif\" WIDTH=16 HEIGHT=16 BORDER=0><IMG SRC=\"" + Icons + "Closed.gif\" WIDTH=16 HEIGHT=16 BORDER=0>"
			Outline += "</A>"
			}
		  }
		Outline += "&nbsp;";
     
		if (db[i].URL == "" || db[i].URL == null) {
		  Outline += " " + db[i].display      // no link, just a listed item  
		  }
		else {
		  Outline += " <A HREF=\"" + db[i].URL + "\">" + db[i].display + "</A>"
		  }
		// Bold if at level 0
		if (currIndent == 0) { 
		  Outline = "<B>" + Outline + "</B>"
		  }
//		if (currIndent == 1) {
//		    Outline += "&nbsp;<a href=details_project.html><img src=../images/document_select.gif border=0 align=bottom></a>"
//		  }
		//if (currIndent == 2) {
		//  Outline += "&nbsp;&nbsp;<a href=overview.html><img src=../images/document_overv.gif border=0></a>&nbsp;<a href=list.html><img src=../images/document_list.gif border=0></a>"
		//  }
//		if (currIndent == 3) {
//		  Outline += "&nbsp;<a href=details_part.html><img src=../images/document_select.gif border=0 align=bottom></a>"
//		  }
		//if (currIndent == 4) {
		//  Outline += "&nbsp;&nbsp;<a href=overview.html><img src=../images/document_overv.gif border=0></a>&nbsp;<a href=definition.html><img src=../images/document_definition.gif border=0></a>"
		//  }
		Outline += "<BR>"
		prevIndentDisplayed = currIndent
		showMyDaughter = expanded
		// if (i == 1) { Outline = ""}
		if (db.length > 25) {
		  document.write(Outline)
			 Outline = ""
								  }
								}
		 }
	document.write(Outline)
	// end -->]]>
</xsl:text>

</script>
<br/>
</xsl:template>

          </xsl:stylesheet>