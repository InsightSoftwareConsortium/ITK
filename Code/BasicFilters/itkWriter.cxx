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
#include "itkCommand.h"

namespace itk
{

/**
 * Construct with empty file name
 */
Writer
::Writer()
{
  m_FileName = "";
  m_FileType = Writer::ASCII;
}


Writer
::~Writer()
{
}


/**
 * Actually do the work of writing.
 */
void 
Writer
::Write()
{
  // make sure input is available
  if ( !this->GetInput(0) )
    {
    itkErrorMacro(<< "No input!");
    return;
    }

  // Notify start event observers
  this->InvokeEvent(Command::StartEvent);

  // Actually do something
  this->WriteData();
  
  // Notify end event observers
  this->InvokeEvent(Command::EndEvent);

  // Release upstream data if requested
  if ( this->GetInput(0)->ShouldIReleaseData() )
    {
    this->GetInput(0)->ReleaseData();
    }
}


void 
Writer
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "File Name: " 
     << (m_FileName.data() ? m_FileName.data() : "(none)") << std::endl;

  if ( m_FileType == Writer::Binary )
    {
    os << indent << "File Type: Binary\n";
    }
  else
    {
    os << indent << "File Type: ASCII\n";
    }
}

} // end namespace itk
