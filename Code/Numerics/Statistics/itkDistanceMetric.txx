/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDistanceMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDistanceMetric_txx
#define __itkDistanceMetric_txx

#include "itkDistanceMetric.h"

namespace itk
{
namespace Statistics
{
template< class TVector >
DistanceMetric< TVector >
::DistanceMetric()
{
  //If the measurment vector type is non-resizable type,
  //initialize the vector size to it.
  MeasurementVectorType vector;

  if ( !MeasurementVectorTraits::IsResizable(vector) )
    {
    MeasurementVectorSizeType defaultLength =
      MeasurementVectorTraits::GetLength(vector);

    this->m_MeasurementVectorSize = defaultLength;
    this->m_Origin.SetSize(this->m_MeasurementVectorSize);
    }
  else
    {
    //otherwise initialize it to zero
    this->m_MeasurementVectorSize = 0;
    }
  m_Origin.Fill(0.0);
}

template< class TVector >
void
DistanceMetric< TVector >
::SetOrigin(const OriginType & x)
{
  if ( this->m_MeasurementVectorSize != 0 )
    {
    if ( x.Size() != this->m_MeasurementVectorSize )
      {
      itkExceptionMacro(<< "Size of the origin must be same as the length of"
                        << " each measurement vector.");
      }
    }

  this->m_MeasurementVectorSize = x.Size();
  m_Origin.SetSize(this->m_MeasurementVectorSize);
  m_Origin = x;
  this->Modified();
}

template< class TVector >
void
DistanceMetric< TVector >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Origin: " << this->GetOrigin() << std::endl;
  os << indent << "MeasurementVectorSize: " << this->GetMeasurementVectorSize() << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
