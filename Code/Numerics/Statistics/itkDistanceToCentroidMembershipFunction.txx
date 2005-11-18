/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDistanceToCentroidMembershipFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
  this->m_MeasurementVectorSize = 0;
  m_Centroid.fill( 0.0f );
}

template < class TVector >
void 
DistanceToCentroidMembershipFunction< TVector >
::SetCentroid(const vnl_vector< double > & centroid)
{
  if( this->m_MeasurementVectorSize != 0 )
    {  
    if( centroid.size() != this->m_MeasurementVectorSize )
      {
      itkExceptionMacro( << "Size of the centroid must be same as the length of"
          << " each measurement vector.");
      }
    }
  this->m_MeasurementVectorSize = centroid.size();
  m_Centroid = centroid;
  this->Modified();
}

template< class TVector >
void 
DistanceToCentroidMembershipFunction< TVector >
::SetMeasurementVectorSize( MeasurementVectorSizeType s )
{
  if( s == this->m_MeasurementVectorSize )
    {
    return;
    }
  
  if( this->m_MeasurementVectorSize != 0 )
    {  
    itkWarningMacro( << "Destructively resizing paramters of the DistanceToCentroidMembershipFunction." );
    }
  this->m_MeasurementVectorSize = s;
  m_Centroid.set_size( s );
  this->Modified();
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
  // Assuming that measurement has the same length asthe centroid. 
  
  double temp =0;
  double tempDistance;

  // Compute |y - mean |   
  for ( unsigned int i = 0; i < this->m_MeasurementVectorSize; i++ )
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

  if ( this->m_MeasurementVectorSize && 
       m_Centroid.size() == this->m_MeasurementVectorSize )
    {
    os << indent << "Centroid: [" ;
    for (i=0; i+1 < m_Centroid.size(); i++)
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
}

} // end namespace Statistics
} // end of namespace itk



#endif
