/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIndent.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkIndent.h"
#include "itkObjectFactory.h"

#define ITK_STD_INDENT 2
#define ITK_NUMBER_OF_BLANKS 40

namespace itk
{

static const char blanks[ITK_NUMBER_OF_BLANKS+1] =
    "                                        ";

/**
 * Instance creation.
 */
Indent*
Indent::
New()
{
  return new Self;
}
  
  
/**
 * Determine the next indentation level. Keep indenting by two until the 
 * max of forty.
 */
Indent 
Indent
::GetNextIndent()
{
  int indent = m_Indent + ITK_STD_INDENT;
  if ( indent > ITK_NUMBER_OF_BLANKS )
    {
    indent = ITK_NUMBER_OF_BLANKS;
    }
  return indent;
}
 
/**
 * Print out the indentation. Basically output a bunch of spaces.
 */
std::ostream& 
operator<<(std::ostream& os, Indent& ind)
{
  os << blanks + (ITK_NUMBER_OF_BLANKS-ind.m_Indent) ;
  return os;
}

} // namespace itk
