/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEuclideanDistanceMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkEuclideanDistanceMetric_txx
#define __itkEuclideanDistanceMetric_txx

namespace itk
{
namespace Statistics
{
template< class TVector >
inline double
EuclideanDistanceMetric< TVector >
::Evaluate(const MeasurementVectorType & x) const
{
  MeasurementVectorSizeType
    measurementVectorSize = this->GetMeasurementVectorSize();

  if ( measurementVectorSize == 0 )
    {
    itkExceptionMacro(<< "Please set the MeasurementVectorSize first");
    }
  MeasurementVectorTraits::Assert(this->GetOrigin(), measurementVectorSize,
                                  "EuclideanDistanceMetric::Evaluate Origin and input vector have different lengths");

  double sumOfSquares = NumericTraits< double >::Zero;

  for ( unsigned int i = 0; i < measurementVectorSize; i++ )
    {
    const double temp = this->GetOrigin()[i] - x[i];
    sumOfSquares += temp * temp;
    }

  const double distance = vcl_sqrt(sumOfSquares);

  return distance;
}

template< class TVector >
inline double
EuclideanDistanceMetric< TVector >
::Evaluate(const MeasurementVectorType & x1, const MeasurementVectorType & x2) const
{
  MeasurementVectorSizeType measurementVectorSize = MeasurementVectorTraits::GetLength(x1);

  if ( measurementVectorSize != MeasurementVectorTraits::GetLength(x2) )
    {
    itkExceptionMacro(<< "The two measurement vectors have unequal size");
    }

  double sumOfSquares = NumericTraits< double >::Zero;

  for ( unsigned int i = 0; i < measurementVectorSize; i++ )
    {
    const double temp = x1[i] - x2[i];
    sumOfSquares += temp * temp;
    }

  const double distance = vcl_sqrt(sumOfSquares);

  return distance;
}

template< class TVector >
inline double
EuclideanDistanceMetric< TVector >
::Evaluate(const ValueType & a, const ValueType & b) const
{
  const double temp = a - b;

  return vcl_abs(temp);
}
} // end of namespace Statistics
} // end of namespace itk

#endif
