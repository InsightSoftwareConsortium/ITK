/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPointSetToPointSetMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPointSetToPointSetMetric_txx
#define __itkPointSetToPointSetMetric_txx

#include "itkPointSetToPointSetMetric.h"

namespace itk
{
/** Constructor */
template< class TFixedPointSet, class TMovingPointSet >
PointSetToPointSetMetric< TFixedPointSet, TMovingPointSet >
::PointSetToPointSetMetric()
{
  m_FixedPointSet = 0;    // has to be provided by the user.
  m_MovingPointSet   = 0; // has to be provided by the user.
  m_Transform     = 0;    // has to be provided by the user.
}

/** Set the parameters that define a unique transform */
template< class TFixedPointSet, class TMovingPointSet >
void
PointSetToPointSetMetric< TFixedPointSet, TMovingPointSet >
::SetTransformParameters(const ParametersType & parameters) const
{
  if ( !m_Transform )
    {
    itkExceptionMacro(<< "Transform has not been assigned");
    }
  m_Transform->SetParameters(parameters);
}

/** Initialize the metric */
template< class TFixedPointSet, class TMovingPointSet >
void
PointSetToPointSetMetric< TFixedPointSet, TMovingPointSet >
::Initialize(void)
throw ( ExceptionObject )
{
  if ( !m_Transform )
    {
    itkExceptionMacro(<< "Transform is not present");
    }

  if ( !m_MovingPointSet )
    {
    itkExceptionMacro(<< "MovingPointSet is not present");
    }

  if ( !m_FixedPointSet )
    {
    itkExceptionMacro(<< "FixedPointSet is not present");
    }

  // If the PointSet is provided by a source, update the source.
  if ( m_MovingPointSet->GetSource() )
    {
    m_MovingPointSet->GetSource()->Update();
    }

  // If the point set is provided by a source, update the source.
  if ( m_FixedPointSet->GetSource() )
    {
    m_FixedPointSet->GetSource()->Update();
    }
}

/** PrintSelf */
template< class TFixedPointSet, class TMovingPointSet >
void
PointSetToPointSetMetric< TFixedPointSet, TMovingPointSet >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Moving PointSet: " << m_MovingPointSet.GetPointer()  << std::endl;
  os << indent << "Fixed  PointSet: " << m_FixedPointSet.GetPointer()   << std::endl;
  os << indent << "Transform:    " << m_Transform.GetPointer()    << std::endl;
}
} // end namespace itk

#endif
