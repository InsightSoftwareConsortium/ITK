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
#include "itkDataObject.h"

//----------------------------------------------------------------------------
itkWriter
::itkWriter()
{
  m_FileName = "";
}

//----------------------------------------------------------------------------
itkWriter
::~itkWriter()
{
}

void 
itkWriter
::Write()
{
  // make sure input is available
  if ( !this->GetInput(0) )
    {
    itkErrorMacro(<< "No input!");
    return;
    }

  this->GetInput(0)->Update();
  if ( m_StartMethod )
    {
    (*m_StartMethod)(m_StartMethodArg);
    }

  this->WriteData();

  if ( m_EndMethod )
    {
    (*m_EndMethod)(m_EndMethodArg);
    }

  if ( this->GetInput(0)->ShouldIReleaseData() )
    {
    this->GetInput(0)->ReleaseData();
    }
}

//----------------------------------------------------------------------------
void 
itkWriter
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkProcessObject::PrintSelf(os,indent);

  os << indent << "File Name: " 
     << (m_FileName.data() ? m_FileName.data() : "(none)") << std::endl;

}

