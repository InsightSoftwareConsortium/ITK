/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLightProcessObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkLightProcessObject.h"
#include "itkCommand.h"
#include "itkEventObject.h"

namespace itk
{

/**
   * Instantiate object with no start, end, or progress methods.
   */
LightProcessObject
::LightProcessObject()
{
  m_AbortGenerateData = false;
  m_Progress = 0.0;
}


/**
   * Destructor for the LightProcessObject class. We've got to
   * UnRegister() the use of any input classes.
   */
LightProcessObject
::~LightProcessObject()
{}


/**
   * Update the progress of the process object. If a ProgressMethod exists, 
   * executes it. Then set the Progress ivar to amount. The parameter amount 
   * should range between (0,1).
   */
void 
LightProcessObject
::UpdateProgress(float amount)
{
  m_Progress = amount;
  this->InvokeEvent( ProgressEvent() );
}


/**
   *
   */
void 
LightProcessObject
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "AbortGenerateData: " << (m_AbortGenerateData ? "On\n" : "Off\n");
  os << indent << "Progress: " << m_Progress << "\n";
}


/**
   *
   */
void 
LightProcessObject
::UpdateOutputData()
{
    
  this->InvokeEvent( StartEvent() );

  /**
     * GenerateData this object - we have not aborted yet, and our progress
     * before we start to execute is 0.0.
     */
  m_AbortGenerateData = 0;
  m_Progress = 0.0;

  this->GenerateData();

  /**
     * If we ended due to aborting, push the progress up to 1.0 (since
     * it probably didn't end there)
     */
  if ( !m_AbortGenerateData )
    {
    this->UpdateProgress(1.0);
    }

  // Notify end event observers
  this->InvokeEvent( EndEvent() );
}

} // end namespace itk
