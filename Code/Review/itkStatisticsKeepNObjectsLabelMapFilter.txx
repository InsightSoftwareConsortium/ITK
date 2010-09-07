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
    case LabelObjectType::MINIMUM:
      {
      typedef typename Functor::MinimumLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::MAXIMUM:
      {
      typedef typename Functor::MaximumLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::MEAN:
      {
      typedef typename Functor::MeanLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::SUM:
      {
      typedef typename Functor::SumLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::SIGMA:
      {
      typedef typename Functor::SigmaLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::VARIANCE:
      {
      typedef typename Functor::VarianceLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::MEDIAN:
      {
      typedef typename Functor::MedianLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::KURTOSIS:
      {
      typedef typename Functor::KurtosisLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::SKEWNESS:
      {
      typedef typename Functor::SkewnessLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::ELONGATION:
      {
      typedef typename Functor::ElongationLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    case LabelObjectType::FLATNESS:
      {
      typedef typename Functor::FlatnessLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedGenerateData(accessor);
      break;
      }
    default:
      Superclass::GenerateData();
      break;
    }
}
} // end namespace itk
#endif
