<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">

<xsl:output method="xml" indent="yes"/>

<xsl:template match="/Update">

<Update>
<StartDateTime><xsl:value-of select="StartDateTime"/></StartDateTime>
<ChangedFileCount><xsl:value-of select="count(Modified|Conflicting)"/></ChangedFileCount>
<AuthorCount><xsl:value-of select="count(Author)"/></AuthorCount>
<DirectoryCount><xsl:value-of select="count(Directory)"/></DirectoryCount>
</Update>

</xsl:template>
</xsl:stylesheet>