<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:dk="http://docbook.org/ns/docbook" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="dk">
<!--
  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.
-->
<!--
<xsl:output method="xml" omit-xml-declaration="yes" indent="yes" doctype-system="../dtds/docbookx.dtd" doctype-public="-//OASIS//DTD DocBook XML//EN"/>
-->
<xsl:output method="xml" omit-xml-declaration="no" encoding="UTF-8" indent="no" />

<xsl:template match="/">
    <xsl:comment>
  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.
</xsl:comment>
<xsl:text>
</xsl:text>
<xsl:comment>
This file was automatically created from a docbook version of PS 3.6-2011, which was then process by Part67.xsl

Manual changes:

1. DateTime -> Date Time (0008,002a)
</xsl:comment>
<xsl:text disable-output-escaping="yes">
&lt;!DOCTYPE doc [
  &lt;!ENTITY part7a SYSTEM "Part7a.xml"&gt;
  &lt;!ENTITY part7b SYSTEM "Part7b.xml"&gt;
  ]&gt;
</xsl:text>
<dicts edition="2011">
<xsl:text disable-output-escaping="yes">
  &amp;part7a;
  &amp;part7b;
</xsl:text>
  <xsl:apply-templates select="*/*/dk:table[@xml:id='table_6-1']" mode="m1"/>
  <xsl:apply-templates select="*/*/dk:table[@xml:id='table_7-1']" mode="m1"/>
  <xsl:apply-templates select="*/*/dk:table[@xml:id='table_8-1']" mode="m1"/>
  <xsl:apply-templates select="*/*/dk:table[@xml:id='table_A-1']" mode="m3"/>
  <xsl:apply-templates select="*/*/dk:table[@xml:id='table_A-2']" mode="m4"/>
<!--
  <xsl:apply-templates select="*/*/*/*/*/dk:table[@xml:id='table_9.3-1']" mode="m2"/>
-->
<xsl:text>
</xsl:text>
</dicts>
</xsl:template>

<xsl:template match="dk:tr" mode="m4">
  <xsl:variable name="value" select="translate(dk:td[1]/dk:para[not(dk:emphasis)] | dk:td[1]/dk:para/dk:emphasis,'&#x200B;','')"/>
  <xsl:variable name="nameStr0" select="dk:td[2]/dk:para/dk:emphasis | dk:td[2]/dk:para[not(dk:emphasis)]"/>
  <xsl:variable name="nameStr" select="translate($nameStr0,'&#x200B;','')"/>
  <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="contains($nameStr,'(Retired)')">
            <xsl:value-of select="normalize-space(substring-before($nameStr,'(Retired)'))"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="normalize-space($nameStr)"/>
        </xsl:otherwise>
      </xsl:choose>
  </xsl:variable>
  <xsl:variable name="type" select="dk:td[3]/dk:para[not(dk:emphasis)] | dk:td[3]/dk:para/dk:emphasis"/>
  <xsl:variable name="part">
      <xsl:choose>
        <xsl:when test="dk:td[4]/dk:para[not(dk:emphasis)]/dk:olink/@targetdoc | dk:td[4]/dk:para/dk:emphasis/dk:olink/@targetdoc">
            <xsl:value-of select="dk:td[4]/dk:para[not(dk:emphasis)]/dk:olink/@targetdoc | dk:td[4]/dk:para/dk:emphasis/dk:olink/@targetdoc"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="dk:td[4]/dk:para[not(dk:emphasis)]/text() | dk:td[4]/dk:para/dk:emphasis/text()"/>
        </xsl:otherwise>
      </xsl:choose>

  </xsl:variable>
  <xsl:variable name="retired">
      <xsl:choose>
        <xsl:when test="contains($nameStr,'(Retired)')">
            <xsl:value-of select="'true'"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="'false'"/>
        </xsl:otherwise>
      </xsl:choose>
  </xsl:variable>
  <xsl:if test="$value != ''">
  <uid value="{$value}" name="{$name}" normative-reference="{$type}"/>
  </xsl:if>
</xsl:template>

<xsl:template match="dk:tr" mode="m3">
  <xsl:variable name="value" select="translate(dk:td[1]/dk:para[not(dk:emphasis)] | dk:td[1]/dk:para/dk:emphasis,'&#x200B;','')"/>
  <xsl:variable name="nameStr0" select="dk:td[2]/dk:para/dk:emphasis | dk:td[2]/dk:para[not(dk:emphasis)]"/>
  <xsl:variable name="nameStr" select="translate($nameStr0,'&#x200B;','')"/>
  <xsl:variable name="name">
      <xsl:choose>
        <xsl:when test="contains($nameStr,'(Retired)')">
            <xsl:value-of select="normalize-space(substring-before($nameStr,'(Retired)'))"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="normalize-space($nameStr)"/>
        </xsl:otherwise>
      </xsl:choose>
  </xsl:variable>
  <xsl:variable name="type" select="dk:td[3]/dk:para[not(dk:emphasis)] | dk:td[3]/dk:para/dk:emphasis"/>
<!--
  <xsl:variable name="part1" select="dk:td[4]/dk:para[not(dk:emphasis)]/dk:olink/@targetdoc | dk:td[4]/dk:para/dk:emphasis/dk:olink/@targetdoc"/>
  <xsl:variable name="part2" select="dk:td[4]/dk:para[not(dk:emphasis)]/text() | dk:td[4]/dk:para/dk:emphasis/text()"/>
-->
  <xsl:variable name="part">
      <xsl:choose>
        <xsl:when test="dk:td[4]/dk:para[not(dk:emphasis)]/dk:olink/@targetdoc | dk:td[4]/dk:para/dk:emphasis/dk:olink/@targetdoc">
            <xsl:value-of select="dk:td[4]/dk:para[not(dk:emphasis)]/dk:olink/@targetdoc | dk:td[4]/dk:para/dk:emphasis/dk:olink/@targetdoc"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="dk:td[4]/dk:para[not(dk:emphasis)]/text() | dk:td[4]/dk:para/dk:emphasis/text()"/>
        </xsl:otherwise>
      </xsl:choose>

  </xsl:variable>
  <xsl:variable name="retired">
      <xsl:choose>
        <xsl:when test="contains($nameStr,'(Retired)')">
            <xsl:value-of select="'true'"/>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="'false'"/>
        </xsl:otherwise>
      </xsl:choose>
  </xsl:variable>
  <xsl:if test="$value != ''">
  <uid value="{$value}" name="{$name}" type="{$type}" part="{$part}" retired="{$retired}"/>
  </xsl:if>
</xsl:template>


<xsl:template match="dk:tr" mode="m2">
  <xsl:variable name="name" select="dk:td[1]/dk:para/text()"/>
  <xsl:variable name="tag" select="translate(dk:td[2]/dk:para/text(),'ABCDEF','abcdef')"/>
  <xsl:variable name="group" select="substring-after(substring-before($tag,','), '(')"/>
  <xsl:variable name="element" select="substring-after(substring-before($tag,')'), ',')"/>
  <xsl:variable name="keyword" select="translate(dk:td[3]/dk:para/text(),'&#x200B;','')"/>
  <xsl:variable name="vr" select="dk:td[3]/dk:para/text()"/>
  <xsl:variable name="vm" select="dk:td[4]/dk:para/text()"/>
  <entry group="{$group}" element="{$element}" keyword="{$keyword}" vr="{$vr}" vm="{$vm}" retired="false" name="{$name}"/>
</xsl:template>

<xsl:template match="dk:tr" mode="m1">
  <xsl:variable name="tagUpper">
<!--
 select="translate(dk:td[1]/dk:para/text(),'ABCDEF','abcdef')"/>
-->
    <xsl:apply-templates select="dk:td[1]/dk:para/dk:emphasis | dk:td[1]/dk:para[not(dk:emphasis)]"/>
  </xsl:variable>
  <xsl:variable name="tagLower" select="translate($tagUpper,'ABCDEF','abcdef')"/>
  <xsl:variable name="group" select="substring-after(substring-before($tagLower,','), '(')"/>
  <xsl:variable name="element" select="substring-after(substring-before($tagLower,')'), ',')"/>
  <xsl:variable name="name">
    <xsl:apply-templates select="dk:td[2]/dk:para/dk:emphasis | dk:td[2]/dk:para[not(dk:emphasis)]"/>
  </xsl:variable>
  <xsl:variable name="keyword" >
    <xsl:variable name="temp" >
      <xsl:apply-templates select="dk:td[3]/dk:para/dk:emphasis | dk:td[3]/dk:para[not(dk:emphasis)]"/>
    </xsl:variable>
    <xsl:value-of select="translate($temp,'&#x200B;','')"/>
  </xsl:variable>
  <xsl:variable name="vr">
    <xsl:apply-templates select="dk:td[4]/dk:para/dk:emphasis | dk:td[4]/dk:para[not(dk:emphasis)]"/>
  </xsl:variable>
  <xsl:variable name="vm">
    <xsl:apply-templates select="dk:td[5]/dk:para/dk:emphasis | dk:td[5]/dk:para[not(dk:emphasis)]"/>
  </xsl:variable>
  <xsl:variable name="retString">
    <xsl:apply-templates select="dk:td[6]/dk:para/dk:emphasis | dk:td[6]/dk:para[not(dk:emphasis)]"/>
  </xsl:variable>
  <!--
  (0018,9445) -> RET - See Note (should be the only attribute without known vr/vm, with 0028,0020)
  -->
  <xsl:variable name="ret" select="$retString = 'RET' or starts-with($retString, 'RET')"/>
  <xsl:if test="$group != ''">
  <entry group="{$group}" element="{$element}">
    <xsl:if test="$keyword != ''">
    <xsl:attribute name="keyword"><xsl:value-of select="$keyword"/></xsl:attribute>
    </xsl:if>
    <xsl:if test="$vr != ''">
    <xsl:attribute name="vr">
        <xsl:call-template name="process-vr">
          <xsl:with-param name="text" select="normalize-space($vr)"/>
        </xsl:call-template>
    </xsl:attribute>
    </xsl:if>
    <xsl:if test="$vm != ''">
    <xsl:attribute name="vm">
        <xsl:call-template name="process-vm">
          <xsl:with-param name="text" select="normalize-space($vm)"/>
        </xsl:call-template>
    </xsl:attribute>
    </xsl:if>
    <xsl:if test="$ret">
    <xsl:attribute name="retired"><xsl:value-of select="$ret"/></xsl:attribute>
    </xsl:if>
    <xsl:if test="$name != ''">
    <xsl:attribute name="name"><xsl:value-of select="translate($name,'&#x200B;','')"/></xsl:attribute>
    </xsl:if>
  </entry>
  </xsl:if>
</xsl:template>

<!-- remove thread -->
<xsl:template match="dk:thead"/>

<!--
<xsl:template match="dk:para">
   <xsl:apply-templates select="./node()"/>
</xsl:template>

<xsl:template match="dk:emphasis">
   <xsl:value-of select="./text()"/>
</xsl:template>
-->

<xsl:template match="dk:tbody">
   <xsl:apply-templates select="*"/>
</xsl:template>

<xsl:template match="dk:caption" mode="m1"/>
<xsl:template match="dk:caption" mode="m2"/>
<xsl:template match="dk:caption" mode="m3"/>
<xsl:template match="dk:caption" mode="m4"/>

<xsl:template match="dk:table[@xml:id='table_6-1']" mode="m1">
  <xsl:variable name="caption" select="dk:caption"/>
  <dict ref="6" name="{$caption}">
   <xsl:apply-templates select="*" mode="m1"/>
</dict>
</xsl:template>

<xsl:template match="dk:table[@xml:id='table_7-1']" mode="m1">
  <xsl:variable name="caption" select="dk:caption"/>
<xsl:text>
</xsl:text>
  <dict ref="7" name="{$caption}">
   <xsl:apply-templates select="*" mode="m1"/>
</dict>
</xsl:template>

<xsl:template match="dk:table[@xml:id='table_8-1']" mode="m1">
  <xsl:variable name="caption" select="dk:caption"/>
<xsl:text>
</xsl:text>
  <dict ref="8" name="{$caption}">
   <xsl:apply-templates select="*" mode="m1"/>
</dict>
</xsl:template>

<xsl:template match="dk:table[@xml:id='table_A-1']" mode="m3">
  <xsl:variable name="caption" select="dk:caption"/>
<xsl:text>
</xsl:text>
  <table ref="Table A-1" name="{$caption}">
   <xsl:apply-templates select="*" mode="m3"/>
</table>
</xsl:template>

<xsl:template match="dk:table[@xml:id='table_A-2']" mode="m4">
  <xsl:variable name="caption" select="dk:caption"/>
<xsl:text>
</xsl:text>
  <table ref="Table A-2" name="{$caption}">
   <xsl:apply-templates select="*" mode="m4"/>
</table>
</xsl:template>

<!--
<xsl:template match="@*|node()">
  <xsl:copy>
   <xsl:apply-templates select="@*|node()" />
  </xsl:copy>
</xsl:template>
-->

<!-- http://stackoverflow.com/questions/5268182/how-to-remove-namespaces-from-xml-using-xslt -->
    <!-- template to copy elements -->
    <xsl:template match="*">
        <xsl:element name="{local-name()}">
            <xsl:apply-templates select="@* | node()"/>
        </xsl:element>
    </xsl:template>

    <!-- template to copy attributes -->
    <xsl:template match="@*">
        <xsl:attribute name="{local-name()}">
            <xsl:value-of select="."/>
        </xsl:attribute>
    </xsl:template>

    <!-- template to copy the rest of the nodes -->
    <xsl:template match="comment() | text() | processing-instruction()">
        <xsl:copy/>
    </xsl:template>


<!--
  template to process VR from PDF representation into GDCM representation
-->
  <xsl:template name="process-vr">
    <xsl:param name="text"/>
    <xsl:choose>
      <xsl:when test="$text='See Note'">
        <xsl:value-of select="''"/>
      </xsl:when>
      <xsl:when test="$text='US or OW'">
        <xsl:value-of select="'US_OW'"/>
      </xsl:when>
      <xsl:when test="$text='US or SS or OW'">
        <xsl:value-of select="'US_SS_OW'"/>
      </xsl:when>
      <xsl:when test="$text='US or SSor OW'">
        <xsl:value-of select="'US_SS_OW'"/>
      </xsl:when>
      <xsl:when test="$text='US or SS'">
        <xsl:value-of select="'US_SS'"/>
      </xsl:when>
      <xsl:when test="$text='OW or OB'">
        <xsl:value-of select="'OB_OW'"/>
      </xsl:when>
      <xsl:when test="$text='OB or OW'">
        <xsl:value-of select="'OB_OW'"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
  <xsl:template name="process-vm">
    <xsl:param name="text"/>
    <xsl:choose>
      <xsl:when test="$text='1-n or 1'">
        <xsl:value-of select="'1-n'"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$text"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>
</xsl:stylesheet>
