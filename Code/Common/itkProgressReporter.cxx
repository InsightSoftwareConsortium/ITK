/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProgressReporter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkProgressReporter.h"

namespace itk
{

//----------------------------------------------------------------------------
ProgressReporter::ProgressReporter(ProcessObject* filter, int threadId,
                                   unsigned long numberOfPixels,
                                   unsigned long numberOfUpdates):
  m_Filter(filter),
  m_ThreadId(threadId)
{
  // Only thread 0 should update progress.
  if(m_ThreadId == 0)
    {
    // Make sure we have at least one pixel.
    if(numberOfPixels == 0)
      {
      numberOfPixels = 1;
      }
    m_NumberOfUpdates = ((numberOfUpdates < numberOfPixels)?
                         numberOfUpdates : numberOfPixels);
    float numPixels = numberOfPixels;
    float numUpdates = m_NumberOfUpdates;
    m_InverseNumberOfPixels = 1.0 / numPixels;
    m_PixelsPerUpdate = static_cast<unsigned long>(numPixels/numUpdates);
    m_PixelsBeforeUpdate = m_PixelsPerUpdate;
    m_CurrentPixel = 0;
    
    // Set the progress to 0.  The filter is just starting.
    m_Filter->UpdateProgress(0);
    }
}

//----------------------------------------------------------------------------
ProgressReporter::~ProgressReporter()
{
  // Only thread 0 should update progress.
  if(m_ThreadId == 0)
    {
    // Set the progress to 1.  The filter has finished.
    m_Filter->UpdateProgress(1);
    }
}  

} // end namespace itk
