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


namespace itk  { 
namespace Statistics  {

template< class TVector >
inline double
EuclideanDistanceMetric< TVector >
::Evaluate(const MeasurementVectorType &x) const
{
  MeasurementVectorSizeType 
    measurementVectorSize = this->GetMeasurementVectorSize();
  if(measurementVectorSize == 0) 
    {
    itkExceptionMacro( << "Please set the MeasurementVectorSize first" );
    }
  MeasurementVectorTraits::Assert( this->GetOrigin(), measurementVectorSize, 
    "EuclideanDistanceMetric::Evaluate Origin and input vector have different lengths");
  
  double temp, distance = NumericTraits< double >::Zero;
  
  for(unsigned int i = 0; i < measurementVectorSize; i++)
    {
    temp = this->GetOrigin()[i] - x[i];
    distance += temp * temp;
    }
  
  return vcl_sqrt(distance);
}

template< class TVector >
inline double
EuclideanDistanceMetric< TVector >
::Evaluate(const MeasurementVectorType &x1, const MeasurementVectorType &x2) const
{
  MeasurementVectorTraits::Assert( x1, x2, 
    "ManhattanDistanceMetric:: The two measurement vectors have unequal size");
  
  MeasurementVectorSizeType measurementVectorSize = MeasurementVectorTraits::GetLength( x1 ); 

  double temp, distance = NumericTraits< double >::Zero;
  for(unsigned int i = 0; i < measurementVectorSize; i++)
    {
    temp = x1[i] - x2[i];
    distance += temp * temp;
    }
  
  return vcl_sqrt(distance);
}

template< class TVector >
inline double
EuclideanDistanceMetric< TVector >
::Evaluate(const ValueType &a, const ValueType &b) const
{
  double temp = a - b;
  return vcl_sqrt(temp * temp);
}

} // end of namespace Statistics 
} // end of namespace itk

#endif
