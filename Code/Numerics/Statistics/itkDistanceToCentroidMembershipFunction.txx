/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDistanceToCentroidMembershipFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDistanceToCentroidMembershipFunction_txx
#define __itkDistanceToCentroidMembershipFunction_txx

#include "itkDistanceToCentroidMembershipFunction.h"

namespace itk{ 
namespace Statistics{

template < class TVector >
DistanceToCentroidMembershipFunction< TVector >
::DistanceToCentroidMembershipFunction():
  m_NumberOfSamples(-1)
{
  m_Centroid.fill( 0.0f );
}

template < class TVector >
void 
DistanceToCentroidMembershipFunction< TVector >
::SetCentroid(const vnl_vector< double > & centroid)
{
  m_Centroid = centroid ;
}

template < class TVector >
const vnl_vector< double > &
DistanceToCentroidMembershipFunction< TVector >
::GetCentroid() const
{
  return m_Centroid ;
}

template < class TVector >
double 
DistanceToCentroidMembershipFunction< TVector >
::Evaluate(const MeasurementVectorType &measurement) const
{ 
  double temp =0;
  double tempDistance;

  // Compute |y - mean |   
  for ( unsigned int i = 0; i < VectorDimension; i++ )
    {
    tempDistance = measurement[i] - m_Centroid[i];
    temp += tempDistance * tempDistance;
    }

  temp = vcl_sqrt( temp );  
  
  return temp ;
}
  
template < class TVector >
void  
DistanceToCentroidMembershipFunction< TVector >
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i;
  Superclass::PrintSelf(os,indent);

  if ( m_Centroid.size() == VectorDimension )
    {
    os << indent << "Centroid: [" ;
    for (i=0; i+1 < VectorDimension; i++)
      {
      os << m_Centroid[i] << ", ";
      }
    os << m_Centroid[i] << "]" << std::endl;
    }
  else
    {
    os <<  indent << "Centorid: not set or size doen't match." << std::endl ;
    }
  
  os << indent << "Number of Samples: " << m_NumberOfSamples << std::endl;
  os << indent << "VectorSize:        " << VectorDimension << std::endl;
}
} // end namespace Statistics
} // end of namespace itk



#endif
