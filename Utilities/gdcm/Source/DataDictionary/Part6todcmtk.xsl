<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="2.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:xs="http://www.w3.org/2001/XMLSchema"
                xmlns:fn="http://www.w3.org/2005/xpath-functions"
                xmlns:ics="http://www.icsmed.de/xslt">

<!-- =====================================================================
 |
 |  Copyright (C) 2007, ICSMED AG
 |
 |  This software and supporting documentation were developed by
 |
 |    ICSMED AG
 |    Escherweg 2
 |    26121 Oldenburg
 |    Germany
 |
 |  Purpose: Convert DICOM data dictionary from GDCM to DCMTK format.
 |
 ======================================================================= -->

<!-- GENERAL -->

<xsl:output method="text" indent="no" encoding="ISO-8859-1"/>
<xsl:strip-space elements="*"/>

<xsl:variable name="apos">'</xsl:variable>

<!-- default edition of the DICOM standard (if not specified in the input file) -->
<xsl:param name="dicom-default-edition" select="'2007'"/>

<!-- actually used edition of the DICOM standard -->
<xsl:variable name="dicom-edition" select="if (string(/dict/@edition)) then /dict/@edition else $dicom-default-edition"/>


<!-- MAIN -->

<xsl:template match="/dicts">
  <!--xsl:call-template name="add-copyright-note"/-->
  <!--xsl:call-template name="add-generator-note"/-->
  <!-- iterate over all entries -->
  <xsl:for-each select="dict/entry">
    <xsl:sort select="@group"/>
    <xsl:sort select="@element"/>
    <!--xsl:variable name="attribute-name" select="ics:to-camel-case(ics:convert-chars(description))"/-->
    <xsl:variable name="attribute-name" select="ics:to-camel-case(ics:convert-chars(@name))"/>
    <!-- output tag -->
    <xsl:choose>
      <!-- repeating group -->
      <xsl:when test="contains(@group,'xx')">
        <xsl:value-of select="concat('(',substring(@group,1,2),'00-',substring(@group,1,2),'FF,',upper-case(@element),')&#9;')"/>
      </xsl:when>
      <xsl:when test="contains(@element,'xx')">
        <xsl:value-of select="concat('(',upper-case(@group),',',substring(@element,1,2),'00-',substring(@element,1,2),'FF)&#9;')"/>
      </xsl:when>
      <!-- standard case -->
      <xsl:otherwise>
        <xsl:value-of select="concat('(',upper-case(@group),',',upper-case(@element),')&#9;')"/>
      </xsl:otherwise>
    </xsl:choose>
    <!-- output VR -->
    <xsl:choose>
      <!-- offset attribute for DICOMDIR -->
      <xsl:when test="@group='0004' and contains($attribute-name,'Offset')">
        <xsl:value-of select="'up&#9;'"/>
      </xsl:when>
      <!-- multiple value representations -->
      <xsl:when test="@vr='OB_OW'">
        <xsl:value-of select="'ox&#9;'"/>
      </xsl:when>
      <xsl:when test="@vr='US_SS'">
        <xsl:value-of select="'xs&#9;'"/>
      </xsl:when>
      <xsl:when test="@vr='US_SS_OW'">
        <xsl:value-of select="'lt&#9;'"/>
      </xsl:when>
      <!-- standard case -->
      <xsl:when test="string(@vr)">
        <xsl:value-of select="concat(@vr,'&#9;')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="'na&#9;'"/>
      </xsl:otherwise>
    </xsl:choose>
    <!-- output name -->
    <xsl:choose>
      <xsl:when test="@version='2'">
        <xsl:value-of select="concat('ACR_NEMA_',$attribute-name,'&#9;')"/>
      </xsl:when>
      <xsl:when test="@retired='true'">
        <xsl:value-of select="concat('RETIRED_',$attribute-name,'&#9;')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="concat($attribute-name,'&#9;')"/>
      </xsl:otherwise>
    </xsl:choose>
    <!-- output VM -->
    <xsl:choose>
      <xsl:when test="string(@vm)">
        <xsl:value-of select="concat(@vm,'&#9;')"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="'1&#9;'"/>
      </xsl:otherwise>
    </xsl:choose>
    <!-- output version -->
    <xsl:choose>
      <xsl:when test="@version='2'">
        <xsl:value-of select="'ACR/NEMA2'"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="'DICOM'"/>
      </xsl:otherwise>
    </xsl:choose>
    <!-- output newline -->
    <xsl:value-of select="'&#10;'"/>
  </xsl:for-each>
</xsl:template>


<!-- TEMPLATES -->

<xsl:template name="add-copyright-note">
  <xsl:value-of select="concat('# Copyright 1994-',fn:format-date(fn:current-date(),'[Y0001]'),' by OFFIS e.V. and ICSMED AG, Oldenburg, Germany&#10;')"/>
  <xsl:value-of select="'# Thanks to Mathieu Malaterre for the XML version of the DICOM standard text&#10;&#10;'"/>
</xsl:template>

<xsl:template name="add-generator-note">
  <xsl:value-of select="concat('# Generated automatically from DICOM PS 3.6-',$dicom-edition,' and PS 3.7-',$dicom-edition,'&#10;')"/>
  <xsl:value-of select="concat('# File created on ',fn:format-dateTime(fn:current-dateTime(),'[Y0001]-[M01]-[D01] [H01]:[m01]:[s01]')),'&#10;'"/>
  <xsl:value-of select="'# Last modified on YYYY-MM-DD by NOBODY&#10;&#10;'"/>
</xsl:template>


<!-- FUNCTIONS -->

<xsl:function name="ics:to-camel-case" as="xs:string*">
  <xsl:param name="string" as="xs:string*"/>
  <!-- create camel case string -->
  <xsl:variable name="result">
    <xsl:for-each select="tokenize($string,'-| |/')">
      <xsl:value-of select="concat(upper-case(substring(.,1,1)),substring(.,2))"/>
    </xsl:for-each>
  </xsl:variable>
  <!-- compose result string -->
  <xsl:sequence select="ics:remove-invalid-chars($result)"/>
</xsl:function>

<xsl:function name="ics:remove-invalid-chars" as="xs:string*">
  <xsl:param name="string" as="xs:string*"/>
  <xsl:sequence select="translate($string,concat($apos,' -/%^.,‘’()[]&amp;‑'),'')"/>
</xsl:function>

<xsl:function name="ics:convert-chars" as="xs:string*">
  <xsl:param name="string" as="xs:string*"/>
  <xsl:sequence select="fn:replace($string,'µ|','micro')"/>
</xsl:function>

</xsl:stylesheet>
