<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text" indent="yes"/>
<!-- XSL to convert XML GDCM2 data dictionay into
     David Clunie's dicom3tools data dictionary
Checked against:
     dicom3tools_1.00.snapshot.20061120/libsrc/standard/elmdict/dicom3.tpl
-->
<!--
  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL: https://gdcm.svn.sourceforge.net/svnroot/gdcm/trunk/Source/DataDictionary/Part6tod3t.xsl $

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.
-->

<!--life saver xsl script found at:
http://www.thescripts.com/forum/thread86881.html
-->
  <xsl:template name="upperCase">
    <xsl:param name="textToTransform"/>
    <xsl:variable name="head">
      <xsl:choose>
        <xsl:when test="contains($textToTransform, ' ')">
          <xsl:value-of select="substring-before($textToTransform, ' ')"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$textToTransform"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>
    <xsl:variable name="tail" select="substring-after($textToTransform, ' ')"/>
    <xsl:variable name="firstTransform" select="concat(translate(substring($head, 1, 1), 'abcdefghijklmnopqrstuvwxyz', 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'), substring($head, 2))"/>
    <xsl:choose>
      <xsl:when test="$tail">
        <xsl:value-of select="$firstTransform"/>
        <xsl:call-template name="upperCase">
          <xsl:with-param name="textToTransform" select="$tail"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$firstTransform"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

<!-- The main template that loop over all dict/entry -->
  <xsl:template match="/">
    <xsl:for-each select="dicts/dict/entry">
    <xsl:sort select="@group"/>
    <xsl:sort select="@element"/>
      <xsl:text>(</xsl:text>
      <xsl:value-of select="translate(@group,'abcdef','ABCDEF')"/>
      <xsl:text>,</xsl:text>
      <xsl:value-of select="translate(@element,'abcdef','ABCDEF')"/>
      <xsl:text>) VERS="</xsl:text>
      <xsl:choose>
        <xsl:when test="@retired = 'true'">
          <xsl:text>2</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>3</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:if test="@retired != &quot;false&quot;">
        <xsl:text>RET</xsl:text>
      </xsl:if>
      <xsl:text>" VR="</xsl:text>
        <xsl:value-of select="@vr"/>
      <xsl:text>" VM="</xsl:text>
        <xsl:value-of select="@vm"/>
      <xsl:text>" Keyword="</xsl:text>
      <xsl:variable name="apos">'</xsl:variable>
      <!--translating an apostrophe is a pain ... better solution ? -->
      <xsl:variable name="description_apos">
        <xsl:value-of select="translate(@name, $apos, '')"/>
      </xsl:variable>
      <xsl:variable name="description_dash">
        <!-- the dicom3tools is not always consistant with capitalization.
             Assume that every time there is a - we want capitalization -->
        <xsl:value-of select="translate($description_apos,'-',' ')"/>
      </xsl:variable>
      <xsl:variable name="description_cap">
        <xsl:call-template name="upperCase">
          <xsl:with-param name="textToTransform" select="normalize-space($description_dash)"/>
        </xsl:call-template>
      </xsl:variable>
      <!-- remove remaining extra character -->
      <xsl:value-of select="translate($description_cap,'/(),','')"/>
      <xsl:text>" Name="</xsl:text>
      <xsl:value-of select="@name"/>
      <xsl:text>"</xsl:text>
      <xsl:text>
</xsl:text>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
