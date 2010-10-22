/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkStatisticsPositionLabelMapFilter.txx,v $
  Language:  C++
  Date:      $Date: 2005/08/23 15:09:03 $
  Version:   $Revision: 1.6 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStatisticsPositionLabelMapFilter_txx
#define __itkStatisticsPositionLabelMapFilter_txx

#include "itkStatisticsPositionLabelMapFilter.h"
#include "itkLabelMapUtilities.h"
#include "itkStatisticsLabelObjectAccessors.h"


namespace itk {

template <class TImage>
StatisticsPositionLabelMapFilter<TImage>
::StatisticsPositionLabelMapFilter()
{
  this->m_Attribute = LabelObjectType::CENTER_OF_GRAVITY;
}


template <class TImage>
void
StatisticsPositionLabelMapFilter<TImage>
::ThreadedProcessLabelObject( LabelObjectType * labelObject )
{
  switch( this->m_Attribute )
    {
    case LabelObjectType::MAXIMUM_INDEX:
      {
      typedef typename Functor::MaximumIndexLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedThreadedProcessLabelObject(accessor, false, labelObject);
      break;
      }
    case LabelObjectType::MINIMUM_INDEX:
      {
      typedef typename Functor::MinimumIndexLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedThreadedProcessLabelObject(accessor, false, labelObject);
      break;
      }
    case LabelObjectType::CENTER_OF_GRAVITY:
      {
      typedef typename Functor::CenterOfGravityLabelObjectAccessor< LabelObjectType > AccessorType;
      AccessorType accessor;
      this->TemplatedThreadedProcessLabelObject(accessor, true, labelObject);
      break;
      }
    default:
      Superclass::ThreadedProcessLabelObject( labelObject );
      break;
    }
}

}// end namespace itk
#endif
