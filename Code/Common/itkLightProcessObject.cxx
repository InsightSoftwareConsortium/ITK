/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLightProcessObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkLightProcessObject.h"
#include "itkCommand.h"

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
    this->InvokeEvent(Command::ProgressEvent);
  }


  /**
   *
   */
  void 
  LightProcessObject
  ::PrintSelf(std::ostream& os, Indent indent)
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
    
    this->InvokeEvent(Command::StartEvent);

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
    this->InvokeEvent(Command::EndEvent);
  }

} // end namespace itk
