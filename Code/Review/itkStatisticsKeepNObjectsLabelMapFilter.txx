/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsKeepNObjectsLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStatisticsKeepNObjectsLabelMapFilter_txx
#define __itkStatisticsKeepNObjectsLabelMapFilter_txx

#include "itkStatisticsKeepNObjectsLabelMapFilter.h"
#include "itkProgressReporter.h"
#include "itkLabelMapUtilities.h"

namespace itk
{
template< class TImage >
StatisticsKeepNObjectsLabelMapFilter< TImage >
::StatisticsKeepNObjectsLabelMapFilter()
{
  this->m_Attribute = LabelObjectType::MEAN;
  // create the output image for the removed objects
  this->SetNumberOfRequiredOutputs(2);
  this->SetNthOutput( 1, static_cast< TImage * >( this->MakeOutput(1).GetPointer() ) );
}

template< class TImage >
void
StatisticsKeepNObjectsLabelMapFilter< TImage >
::GenerateData()
{
  switch ( this->m_Attribute )
    {
    itkStatisticsLabelMapFilterDispatchMacro()
    default:
      Superclass::GenerateData();
      break;
    }
}
} // end namespace itk
#endif
