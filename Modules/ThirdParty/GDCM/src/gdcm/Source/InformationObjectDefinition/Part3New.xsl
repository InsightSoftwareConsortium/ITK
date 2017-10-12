<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:dk="http://docbook.org/ns/docbook" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="dk">
<xsl:output method="xml" omit-xml-declaration="no" encoding="UTF-8" indent="no" />

<xsl:template match="text()" />

<xsl:template match="/">
<xsl:comment> to produce output use:
$ xsltproc ma2html.xsl ModuleAttributes.xml
    </xsl:comment>
    <xsl:comment>
  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.
</xsl:comment>
<tables edition="2008">
   <xsl:apply-templates />
</tables>
</xsl:template>

<xsl:template match="dk:caption" mode="module"/>
<xsl:template match="dk:caption" mode="macro"/>
<xsl:template match="dk:caption" mode="iod"/>
<xsl:template match="dk:caption" mode="iod3"/>

<xsl:template match="dk:table">
  <xsl:variable name="caption" select="dk:caption"/>
  <xsl:choose>
  <xsl:when test="contains($caption,'Macro') and not(contains($caption,'Without The Use'))">
  <macro table="{@label}" name="{$caption}">
   <xsl:apply-templates mode="macro"/>
  </macro>
  </xsl:when>
  <xsl:when test="contains($caption,'Module') and not(contains($caption,'IOD'))">
  <module table="{@label}" name="{$caption}">
   <xsl:apply-templates mode="module"/>
  </module>
  </xsl:when>
  <xsl:when test="contains($caption,'IOD Modules') and count(dk:thead/dk:tr/dk:th) = 4">
  <iod table="{@label}" name="{$caption}">
   <xsl:apply-templates mode="iod"/>
  </iod>
  </xsl:when>
  <xsl:when test="contains($caption,'IOD Modules') and count(dk:thead/dk:tr/dk:th) = 3">
  <iod table="{@label}" name="{$caption}">
   <xsl:apply-templates mode="iod3"/>
  </iod>
  </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="dk:thead/dk:tr" mode="macro"/>
<xsl:template match="dk:tbody/dk:tr" mode="macro">
  <xsl:variable name="num_nodes" select="count(*)"/>
<!-- counting the number of element avoid issue with: BASIC CODED ENTRY ATTRIBUTES as seen in Table 8.8-1a -->
  <xsl:if test="$num_nodes = 4">
  <xsl:variable name="name" select="dk:td[1]/dk:para"/>
  <xsl:variable name="tag" select="dk:td[2]/dk:para"/> <!-- keep upper case for now -->
  <xsl:variable name="group" select="substring-after(substring-before($tag,','), '(')"/>
  <xsl:variable name="element" select="substring-after(substring-before($tag,')'), ',')"/>
  <xsl:variable name="type" select="dk:td[3]/dk:para"/>
  <xsl:variable name="description">
<!--
    <xsl:apply-templates select="dk:td[4]/*" mode="module"/>
-->
    <xsl:for-each select="dk:td[4]/*">
      <xsl:apply-templates select="node()" mode="module"/>
      <xsl:if test="position() != last()">
        <xsl:text> </xsl:text>
      </xsl:if>
    </xsl:for-each>
  </xsl:variable>
  <entry group="{$group}" element="{$element}" name="{$name}" type="{$type}">
     <description><xsl:value-of select="$description"/></description>
  </entry>
  </xsl:if>
  <!-- yes a macro can also reference another macro -->
  <xsl:if test="$num_nodes = 2">
  <xsl:variable name="ref">
    <xsl:apply-templates select="dk:td[1]/dk:para/dk:emphasis" mode="module"/>
  </xsl:variable>
  <xsl:variable name="description" >
    <xsl:apply-templates select="dk:td[2]/dk:para[not(dk:emphasis)] | dk:td[2]/dk:para/dk:emphasis" mode="module"/>
  </xsl:variable>
  <include ref="{$ref}">
    <xsl:if test="$description != ''">
      <xsl:attribute name="description"><xsl:value-of select="$description"/></xsl:attribute>
    </xsl:if>
  </include>
  </xsl:if>
</xsl:template>

<xsl:template match="dk:thead/dk:tr" mode="module"/>
<xsl:template match="dk:tbody/dk:tr" mode="module">
  <xsl:variable name="num_nodes" select="count(*)"/>
  <xsl:if test="$num_nodes = 4">
  <xsl:variable name="name" select="dk:td[1]/dk:para"/>
  <xsl:variable name="tag" select="dk:td[2]/dk:para"/> <!-- keep upper case for now -->
  <xsl:variable name="group" select="substring-after(substring-before($tag,','), '(')"/>
  <xsl:variable name="element" select="substring-after(substring-before($tag,')'), ',')"/>
  <xsl:variable name="type" select="dk:td[3]/dk:para"/>
  <xsl:variable name="description">
    <xsl:apply-templates select="dk:td[4]/dk:para" mode="module"/>
  </xsl:variable>
  <entry group="{$group}" element="{$element}" name="{$name}" type="{$type}">
     <description><xsl:value-of select="$description"/></description>
  </entry>
  </xsl:if>
  <xsl:if test="$num_nodes = 2">
  <xsl:variable name="ref">
    <xsl:apply-templates select="dk:td[1]/dk:para/dk:emphasis" mode="module"/>
  </xsl:variable>
  <xsl:variable name="description" select="dk:td[2]/dk:para/dk:emphasis"/>
  <include ref="{$ref}" description="{$description}"/>
  </xsl:if>
</xsl:template>

<xsl:template match="dk:variablelist" mode="module">
<!--
  <xsl:value-of select="dk:title"/>
-->
  <xsl:apply-templates mode="module"/>
</xsl:template>

<xsl:template match="dk:varlistentry" mode="module">
  <xsl:value-of select="dk:term"/>
</xsl:template>

<xsl:key name="tables" match="dk:table" use="@xml:id" />

<xsl:template match="dk:xref" mode="module">
  <xsl:text>'</xsl:text>
  <xsl:value-of select="key('tables', @linkend)/dk:caption" />
  <xsl:text>' Table </xsl:text>
  <xsl:value-of select="translate(@linkend,'table_','')"/>
</xsl:template>

<xsl:template match="dk:olink" mode="module">
  <xsl:value-of select="translate(@targetptr,'sect_','')"/>
</xsl:template>

<xsl:template match="dk:thead/dk:tr" mode="iod"/>
<xsl:template match="dk:tbody/dk:tr" mode="iod">
  <xsl:variable name="num_nodes" select="count(*)"/>
  <xsl:variable name="offset" select="$num_nodes - 3"/>
  <xsl:variable name="ie">
     <xsl:if test="$offset = 1">
       <xsl:value-of select="dk:td[1]/dk:para"/>
     </xsl:if>
     <xsl:if test="$offset = 0">
<!--
       <xsl:value-of select="preceding::dk:td[4]/dk:para"/>
       <xsl:value-of select="(preceding-sibling::*/dk:td[@rowspan != 1])[1]/dk:para"/>
-->
       <xsl:value-of select="preceding-sibling::dk:tr[count(dk:td) = 4 and dk:td[@rowspan != 1]][1]/dk:td[1]/dk:para"/>
     </xsl:if>
  </xsl:variable>
  <xsl:variable name="module" select="dk:td[1 + $offset]/dk:para"/>
  <xsl:variable name="reference" select="dk:td[2 + $offset]/dk:para/dk:xref/@linkend"/>
  <xsl:variable name="ref" select="translate($reference,'sect_','')"/>
<!--
  <xsl:variable name="usage" select="dk:td[3 + $offset]/dk:para"/>
-->
  <xsl:variable name="usage">
    <!-- the following is a hack since we only want to execute when two para and no trailing dot is found: -->
    <xsl:for-each select="dk:td[3 + $offset]/*">
      <xsl:apply-templates select="node()" mode="iod"/>
      <xsl:if test="position() != last()">
        <xsl:text>.</xsl:text>
      </xsl:if>
    </xsl:for-each>
  </xsl:variable>
  <entry ie="{$ie}" name="{$module}" ref="{$ref}" usage="{$usage}"/>
</xsl:template>

<xsl:template match="dk:thead/dk:tr" mode="iod3"/>
<xsl:template match="dk:tbody/dk:tr" mode="iod3">
  <xsl:variable name="module" select="dk:td[1]/dk:para"/>
  <xsl:variable name="reference" select="dk:td[2]/dk:para/dk:xref/@linkend"/>
  <xsl:variable name="ref" select="translate($reference,'sect_','')"/>
  <xsl:variable name="description" select="dk:td[3]/dk:para"/>
  <entry name="{$module}" ref="{$ref}">
    <xsl:if test="$description != ''">
    <xsl:attribute name="description"><xsl:value-of select="$description"/></xsl:attribute>
    </xsl:if>
  </entry>
</xsl:template>

<xsl:template match="dk:para" mode="iod">
  <xsl:apply-templates mode="iod"/>
</xsl:template>

<xsl:key name="sections" match="dk:section" use="@xml:id" />
<xsl:template match="dk:xref" mode="iod">
<!--
  <xsl:message><xsl:text>bla</xsl:text></xsl:message>
-->
<!--
  <xsl:value-of select="key('sections', @linkend)/dk:title" />
-->
  <xsl:text>Section </xsl:text>
  <xsl:value-of select="translate(@linkend,'sect_','')"/>
</xsl:template>

<!--
<xsl:template match="@*|node()">
  <xsl:copy>
   <xsl:apply-templates select="@*|node()" />
  </xsl:copy>
</xsl:template>
-->

</xsl:stylesheet>
