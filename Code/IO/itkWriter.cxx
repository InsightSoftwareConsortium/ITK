/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
    itkExceptionMacro(<< "No input!");
    return;
    }

  // Notify start event observers
  this->InvokeEvent( StartEvent() );

  // Actually do something
  this->WriteData();
  
  // Notify end event observers
  this->InvokeEvent( EndEvent() );

  // Release upstream data if requested
  if ( this->GetInput(0)->ShouldIReleaseData() )
    {
    this->GetInput(0)->ReleaseData();
    }
}


void 
Writer
::PrintSelf(std::ostream& os, Indent indent) const
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
