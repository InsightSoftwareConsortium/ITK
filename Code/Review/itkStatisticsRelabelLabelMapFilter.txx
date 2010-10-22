/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsRelabelLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStatisticsRelabelLabelMapFilter_txx
#define __itkStatisticsRelabelLabelMapFilter_txx

#include "itkStatisticsRelabelLabelMapFilter.h"
#include "itkProgressReporter.h"
#include "itkLabelMapUtilities.h"

namespace itk
{
template< class TImage >
StatisticsRelabelLabelMapFilter< TImage >
::StatisticsRelabelLabelMapFilter()
{
  this->m_Attribute = LabelObjectType::MEAN;
}

template< class TImage >
void
StatisticsRelabelLabelMapFilter< TImage >
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
