/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaArrayReader.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMetaArrayReader_cxx
#define __itkMetaArrayReader_cxx

#include "itkMetaArrayReader.h"

namespace itk
{

/** Constructor */
MetaArrayReader
::MetaArrayReader()
  {
  m_FileName = "";
  m_Buffer = NULL;
  }

/** Destructor */
MetaArrayReader
::~MetaArrayReader()
  {
  }

/** Update the Reader */
void MetaArrayReader
::SetBuffer(void * _buffer)
  {  
  m_Buffer = _buffer;
  }

/** Return a pointer to the metaArray variable */
MetaArray * MetaArrayReader
::GetMetaArrayPointer(void)
  {
  return & m_MetaArray;
  }

/** Update the Reader */
void MetaArrayReader
::Update()
  {  
  m_MetaArray.Read(m_FileName.c_str(), true, m_Buffer);
  }


} // namespace itk

#endif
