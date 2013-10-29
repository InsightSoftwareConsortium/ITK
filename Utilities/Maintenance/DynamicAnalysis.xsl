<xsl:stylesheet
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version='1.0'>

   <xsl:output method="xml" indent="yes"  doctype-public="-//W3C//DTD XHTML 1.0 Transitional//EN"
   doctype-system="http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd" />
   <xsl:template match="/">
      <html>
       <head>
       <title><xsl:value-of select="cdash/title"/></title>
        <meta name="robots" content="noindex,nofollow" />
         <link rel="StyleSheet" type="text/css">
          <xsl:attribute name="href">stylish.css</xsl:attribute>
         </link>
       </head>
       <body bgcolor="#111111">

<br/>

<!-- Main -->
<br/>
<h3>Dynamic analysis started on <xsl:value-of select="Site/DynamicAnalysis/StartDateTime"/></h3>
<table border="0">
  <tr><td align="right"><b>Site Name:</b></td><td><xsl:value-of select="Site/@Hostname"/></td></tr>
  <tr><td align="right"><b>Build Name:</b></td><td><xsl:value-of select="Site/@BuildStamp"/></td></tr>
</table>

<table xmlns:lxslt="http://xml.apache.org/xslt" cellspacing="2" cellpadding="3">
   <tr>
      <th width="20%">Name</th>

      <th>Status</th>
      <th><xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text></th>
      <th><xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text>Memory Leak<xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text></th>
      <th><xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text>Uninitialized Memory Read<xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text></th>
      <th><xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text>Potential Memory Leak<xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text></th>
      <th><xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text>Uninitialized Memory Conditional<xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text></th>
      <th><xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text>Mismatched Deallocate<xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text></th>
      <th><xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text>Freeing Invalid Memory<xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text></th>
      <th><xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text>Invalid Pointer Read<xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text></th>
      <th><xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text>Invalid Pointer Write<xsl:text disable-output-escaping="yes">&amp;nbsp;</xsl:text></th>

      <th>Labels</th>
  </tr>

   <xsl:for-each select="Site/DynamicAnalysis/Test">
   <tr align="center" bgcolor="#333333">
      <td align="left"><a>
        <xsl:attribute name="href"><xsl:value-of select="Name"/>.html</xsl:attribute>
        <xsl:value-of select="Name"/>
      </a></td>
      <td>
      <xsl:attribute name="class">
       <xsl:choose>
          <xsl:when test="./@Status='passed'">
            normal
          </xsl:when>
          <xsl:otherwise>
            warning
           </xsl:otherwise>
        </xsl:choose>
      </xsl:attribute>
      <xsl:value-of select="./@Status"/></td>
      <td></td>
      <!-- Memory Leak -->
      <td>
      <xsl:attribute name="class">
       <xsl:choose>
          <xsl:when test="count(./Results/Defect[@type='Memory Leak'])>0">
            warning
          </xsl:when>
        </xsl:choose>
      </xsl:attribute>
      <xsl:value-of select="./Results/Defect[@type='Memory Leak']"/>
      </td>
      <!-- UMR -->
      <td>
      <xsl:attribute name="class">
       <xsl:choose>
          <xsl:when test="count(./Results/Defect[@type='Uninitialized Memory Read'])>0">
            warning
          </xsl:when>
        </xsl:choose>
      </xsl:attribute>
      <xsl:value-of select="./Results/Defect[@type='Uninitialized Memory Read']"/>
      </td>
      <!-- PML -->
      <td>
      <xsl:attribute name="class">
       <xsl:choose>
          <xsl:when test="count(./Results/Defect[@type='Potential Memory Leak'])>0">
            warning
          </xsl:when>
        </xsl:choose>
      </xsl:attribute>
      <xsl:value-of select="./Results/Defect[@type='Potential Memory Leak']"/>
      </td>
      <!--UMC -->
      <td>
      <xsl:attribute name="class">
       <xsl:choose>
          <xsl:when test="count(./Results/Defect[@type='Uninitialized Memory Conditional'])>0">
            warning
          </xsl:when>
        </xsl:choose>
      </xsl:attribute>
      <xsl:value-of select="./Results/Defect[@type='Uninitialized Memory Conditional']"/>
      </td>
      <!-- Mismatched deallocation -->
      <td>
      <xsl:attribute name="class">
       <xsl:choose>
          <xsl:when test="count(./Results/Defect[@type='Mismatched deallocation'])>0">
            warning
          </xsl:when>
        </xsl:choose>
      </xsl:attribute>
      <xsl:value-of select="./Results/Defect[@type='Mismatched deallocation']"/>
      </td>
      <!--FIM -->
      <td>
      <xsl:attribute name="class">
       <xsl:choose>
          <xsl:when test="count(./Results/Defect[@type='FIM'])>0">
            warning
          </xsl:when>
        </xsl:choose>
      </xsl:attribute>
      <xsl:value-of select="./Results/Defect[@type='FIM']"/>
      </td>
      <!-- IPR -->
      <td>
      <xsl:attribute name="class">
       <xsl:choose>
          <xsl:when test="count(./Results/Defect[@type='IPR'])>0">
            warning
          </xsl:when>
        </xsl:choose>
      </xsl:attribute>
      <xsl:value-of select="./Results/Defect[@type='IPR']"/>
      </td>
      <!-- IPW -->
      <td>
      <xsl:attribute name="class">
       <xsl:choose>
          <xsl:when test="count(./Results/Defect[@type='IPW'])>0">
            warning
          </xsl:when>
        </xsl:choose>
      </xsl:attribute>
      <xsl:value-of select="./Results/Defect[@type='IPW']"/>
      </td>
      <!-- Labels -->
      <td>
        <xsl:for-each select="labels/label">
          <xsl:if test="position() > 1">,
          <xsl:text disable-output-escaping="yes"> </xsl:text>
          </xsl:if>
          <nobr><xsl:value-of select="."/></nobr>
        </xsl:for-each>
      </td>

    </tr>
   </xsl:for-each>
</table>

<!-- FOOTER -->
<br/>
        </body>
      </html>
    </xsl:template>
</xsl:stylesheet>
