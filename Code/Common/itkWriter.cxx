/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkWriter.h"

//------------------------------------------------------------------------
itkWriter::Pointer itkWriter::New()
{
  return itkWriter::Pointer(new itkWriter);
}

//----------------------------------------------------------------------------
itkWriter::itkWriter()
{
  m_FileName = 0;
  m_FileType = ITK_ASCII;
}

//----------------------------------------------------------------------------
itkWriter::~itkWriter()
{
  if ( m_FileName )
    {
    delete [] m_FileName;
    m_FileName = NULL;
    }
}

//----------------------------------------------------------------------------
void itkWriter::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkProcessObject::PrintSelf(os,indent);

  os << indent << "File Name: " 
     << (m_FileName ? m_FileName : "(none)") << std::endl;

  if ( m_FileType == VTK_BINARY )
    {
    os << indent << "File Type: BINARY\n";
    }
  else
    {
    os << indent << "File Type: ASCII\n";
    }
}

