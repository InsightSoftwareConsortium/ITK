/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEuclideanDistance.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkEuclideanDistance_txx
#define __itkEuclideanDistance_txx


namespace itk{ 
namespace Statistics{

template< class TVector >
inline double
EuclideanDistance< TVector >
::Evaluate(const TVector &x1, const TVector &x2) const
{
  double temp, distance = NumericTraits< double >::Zero ;
  
  for(unsigned int i = 0 ; i < VectorLength ; i++ )
    {
    temp = x1[i] - x2[i] ;
    distance += temp * temp ;
    }
  
  return sqrt(distance) ;
}


template< class TVector >
inline double
EuclideanDistance< TVector >
::Evaluate(const TVector &x) const
{
  double temp, distance = NumericTraits< double >::Zero ;
  
  for(unsigned int i = 0 ; i < VectorLength ; i++ )
    {
    temp = m_Origin[i] - x[i] ;
    distance += temp * temp ;
    }
  
  return sqrt(distance) ;
}

template< class TVector >
inline double
EuclideanDistance< TVector >
::Evaluate(const ValueType &a, const ValueType &b) const
{
  double temp = a - b ;
  return sqrt(temp * temp) ;
}

template< class TVector >
inline bool
EuclideanDistance< TVector >
::IsWithinRange(const TVector &x, const double radius) const 
{
  double squaredRadius = radius * radius ;
  double sum = NumericTraits< double >::Zero ;
  double temp ;
  for ( unsigned int i = VectorLength ; i > 0 ; --i )
    {
    temp = this->Evaluate( m_Origin[i-1], x[i-1] ) ;
    sum += temp * temp ;
    if (sum > squaredRadius)
      {
      return false ;
      }
    }
  return true ;
}

} // end of namespace Statistics 
} // end of namespace itk

#endif







