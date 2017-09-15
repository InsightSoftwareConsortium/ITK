<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:dk="http://docbook.org/ns/docbook" xmlns:xsl="http://www.w3.org/1999/XSL/Transform" exclude-result-prefixes="dk">
<xsl:output method="xml" omit-xml-declaration="no" encoding="UTF-8" indent="no" />

<!--
<xsl:template match="/">
<xsl:text>
</xsl:text>

  <xsl:apply-templates select="*/*/*/*/*/dk:table[@xml:id='table_B.3-3']" />
  <xsl:apply-templates select="*/*/*/dk:table[@xml:id='table_B.5-1']" />
  <xsl:apply-templates select="*/*/*/dk:table[@xml:id='table_I.4-1']" />
</xsl:template>
-->
<xsl:template match="text()" />

<!--
<xsl:template match="@*|node()">
  <xsl:copy>
   <xsl:apply-templates select="@*|node()" />
  </xsl:copy>
</xsl:template>
-->
<xsl:template match="dk:caption" />

<xsl:template match="dk:book" >
    <xsl:comment>
  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.
</xsl:comment>

  <sop-classes>
   <xsl:apply-templates />
</sop-classes>
</xsl:template>

<!-- skip those table -->
<!--
<xsl:template match="dk:table[@xml:id='table_B.2-1']"/>
<xsl:template match="dk:table[@xml:id='table_B.3-1']"/>
<xsl:template match="dk:table[@xml:id='table_B.3-2']"/>
<xsl:template match="dk:table[@xml:id='table_B.4-1']"/>
<xsl:template match="dk:table[@xml:id='table_C.1.2-1']"/>
<xsl:template match="dk:table[@xml:id='table_C.3-1']"/>
<xsl:template match="dk:table[@xml:id='table_C.4-1']"/>
<xsl:template match="dk:table[@xml:id='table_C.4-2']"/>
<xsl:template match="dk:table[@xml:id='table_C.4-3']"/>
<xsl:template match="dk:table[@xml:id='table_C.5-1']"/>
<xsl:template match="dk:table[@xml:id='table_C.5-2']"/>
<xsl:template match="dk:table[@xml:id='table_C.5-3']"/>
<xsl:template match="dk:table[@xml:id='table_C.5-4']"/>
<xsl:template match="dk:table[@xml:id='table_C.6.1-1']"/>
<xsl:template match="dk:table[@xml:id='table_C.6-1']"/>
<xsl:template match="dk:table[@xml:id='table_C.6-2']"/>
<xsl:template match="dk:table[@xml:id='table_C.6-3']"/>
<xsl:template match="dk:table[@xml:id='table_C.6-4']"/>
<xsl:template match="dk:table[@xml:id='table_C.6.1.3-1']"/>
<xsl:template match="dk:table[@xml:id='table_C.6.2-1']"/>
<xsl:template match="dk:table[@xml:id='table_C.6-5']"/>
<xsl:template match="dk:table[@xml:id='table_C.6.2.3-1']"/>
<xsl:template match="dk:table[@xml:id='table_F.1-3']"/>
<xsl:template match="dk:table[@xml:id='table_F.1-4']"/>
<xsl:template match="dk:table[@xml:id='table_F.7.1-1']"/>
<xsl:template match="dk:table[@xml:id='table_F.7.2-1']"/>
<xsl:template match="dk:table[@xml:id='table_F.7.2-2']"/>
-->
<xsl:template match="dk:table"/>

<!-- do not skip the following tables -->
<xsl:template match="dk:table[@xml:id='table_B.3-3']">
  <xsl:variable name="caption" select="dk:caption"/>
<xsl:text>
</xsl:text>
  <standard-and-related-general-sop-classes>
   <xsl:apply-templates />
  </standard-and-related-general-sop-classes>
</xsl:template>

<xsl:template match="dk:table[@xml:id='table_B.5-1']">
  <xsl:variable name="caption" select="dk:caption"/>
<xsl:text>
</xsl:text>
  <standard-sop-classes>
   <xsl:apply-templates />
  </standard-sop-classes>
</xsl:template>

<xsl:template match="dk:table[@xml:id='table_B.6-1']">
  <xsl:variable name="caption" select="dk:caption"/>
<xsl:text>
</xsl:text>
  <retired-standard-sop-classes>
   <xsl:apply-templates />
  </retired-standard-sop-classes>
</xsl:template>

<xsl:template match="dk:table[@xml:id='table_C.3.5-1']">
  <xsl:variable name="caption" select="dk:caption"/>
<xsl:text>
</xsl:text>
  <modality-specific-sop-class-conversions>
   <xsl:apply-templates />
  </modality-specific-sop-class-conversions>
</xsl:template>

<xsl:template match="dk:table[@xml:id='table_I.4-1']">
  <xsl:variable name="caption" select="dk:caption"/>
<xsl:text>
</xsl:text>
    <media-storage-standard-sop-classes>
   <xsl:apply-templates />
    </media-storage-standard-sop-classes>
</xsl:template>

<xsl:template match="dk:table[@xml:id='table_GG.3-1']">
  <xsl:variable name="caption" select="dk:caption"/>
<xsl:text>
</xsl:text>
    <standard-sop-classes2>
   <xsl:apply-templates />
    </standard-sop-classes2>
</xsl:template>


<xsl:template match="dk:tr" >
  <xsl:variable name="sopclassname" select="dk:td[1]/dk:para/text()"/>
  <xsl:variable name="classuid" select="translate(dk:td[2]/dk:para/text(),'&#x200B;','')"/>
<!--
  <xsl:variable name="iod" select="dk:td[3]/dk:para/dk:emphasis | dk:td[3]/dk:para[not(dk:emphasis)]"/>
-->
  <xsl:variable name="iodtext" select="dk:td[3]/dk:para[1]/dk:olink/@targetptr"/>
  <xsl:variable name="iodlink" select="dk:td[3]/dk:para[2]/dk:xref/@linkend"/>
<!--
<xsl:choose>
-->
  <xsl:variable name="iod" >
    <xsl:choose>
    <xsl:when test="$iodtext = 'sect_A.26'">
       <xsl:text>DX IOD </xsl:text>
    </xsl:when>
    <xsl:when test="$iodtext = 'sect_A.27'">
       <xsl:text>Digital Mammography IOD </xsl:text>
    </xsl:when>
    <xsl:when test="$iodtext = 'sect_A.28'">
       <xsl:text>Digital Intra-oral X-Ray IOD </xsl:text>
    </xsl:when>
    <xsl:when test="$iodtext = 'sect_A.38.1'">
       <xsl:text>Enhanced CT Image </xsl:text>
    </xsl:when>
    <xsl:when test="$iodtext = 'sect_A.36.2'">
       <xsl:text>Enhanced MR Image </xsl:text>
    </xsl:when>
    <xsl:when test="$iodtext = 'sect_A.36.3'">
       <xsl:text>MR Spectroscopy</xsl:text>
    </xsl:when>
    <xsl:when test="$iodtext = 'sect_A.36.4'">
       <xsl:text>Enhanced MR Color Image</xsl:text>
    </xsl:when>
    <xsl:when test="$iodtext = 'sect_A.59'">
       <xsl:text>Enhanced US Volume</xsl:text>
    </xsl:when>
<!--
    <xsl:otherwise>
       <xsl:value-of select="'BLA'"/>
    </xsl:otherwise>
-->
    </xsl:choose>
    <xsl:if test="$iodlink!= ''">
<xsl:value-of select="concat(translate($iodlink,'sect_','(see '),')')"/>
  </xsl:if>
  </xsl:variable>
<!--
    </xsl:when>
    <xsl:otherwise>
  <xsl:variable name="iod"/>
    </xsl:otherwise>
</xsl:choose>
-->
    <xsl:if test="$sopclassname != ''">
  <mapping sop-class-name="{$sopclassname}" sop-class-uid="{$classuid}" iod="{$iod}"/>
</xsl:if>
</xsl:template>

</xsl:stylesheet>
