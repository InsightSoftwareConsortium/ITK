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

//----------------------------------------------------------------------------
itkWriter::itkWriter()
{
  m_FileName = "";
  m_FileType = ITK_ASCII;
}

//----------------------------------------------------------------------------
itkWriter::~itkWriter()
{
}

void itkWriter::Write()
{
  this->Update();
  this->WriteData();
}

//----------------------------------------------------------------------------
void itkWriter::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkProcessObject::PrintSelf(os,indent);

  os << indent << "File Name: " 
     << (m_FileName.data() ? m_FileName.data() : "(none)") << std::endl;

  if ( m_FileType == ITK_BINARY )
    {
    os << indent << "File Type: BINARY\n";
    }
  else
    {
    os << indent << "File Type: ASCII\n";
    }
}

