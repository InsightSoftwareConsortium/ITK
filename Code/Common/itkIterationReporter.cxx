/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIterationReporter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkIterationReporter.h"
#include "itkNumericTraits.h"

namespace itk
{

//----------------------------------------------------------------------------
IterationReporter::IterationReporter(ProcessObject* filter, int threadId,
                                     unsigned long stepsPerUpdate):
  m_Filter(filter),
  m_ThreadId(threadId),
  m_StepsPerUpdate( stepsPerUpdate )
{
  // Only thread 0 should update progress.
  m_StepsBeforeUpdate = m_StepsPerUpdate;
}

  

} // end namespace itk
