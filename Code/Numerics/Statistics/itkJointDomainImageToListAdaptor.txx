/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJointDomainImageToListAdaptor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkJointDomainImageToListAdaptor_txx
#define _itkJointDomainImageToListAdaptor_txx

#include "itkJointDomainImageToListAdaptor.h"

namespace itk{ 
namespace Statistics{

template < class TImage >
JointDomainImageToListAdaptor< TImage >
::JointDomainImageToListAdaptor()
{
  m_NormalizationFactors.Fill( 1.0f ) ;
}

template < class TImage >
void
JointDomainImageToListAdaptor< TImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

template < class TImage >
void
JointDomainImageToListAdaptor< TImage >
::SetNormalizationFactors(NormalizationFactorsType& factors)
{
  if ( m_NormalizationFactors != factors )
    {
    m_NormalizationFactors = factors ;
    this->Modified() ;
    }
}

template < class TImage >
inline const typename JointDomainImageToListAdaptor< TImage >::MeasurementVectorType &
JointDomainImageToListAdaptor< TImage >
::GetMeasurementVector(const InstanceIdentifier &id) const
{
  m_TempIndex = this->GetImage()->ComputeIndex( id ) ;
  
  this->GetImage()->TransformIndexToPhysicalPoint( m_TempIndex, m_TempPoint ) ;
  
  for ( unsigned int i = 0 ; i < TImage::ImageDimension ; ++i )
    {
    m_TempVector[i] = m_TempPoint[i] / m_NormalizationFactors[i] ;
    }
  
  if( m_UseBuffer )
    {
    m_TempRangeVector =  
      *(reinterpret_cast<const RangeDomainMeasurementVectorType* >
        (&(*m_PixelContainer)[id]))  ;
    }
  else
    {
    m_TempRangeVector = 
      *(reinterpret_cast< const RangeDomainMeasurementVectorType* >
        (&(this->GetImage()->GetPixel( m_TempIndex ) ) ) ) ;
    }
  
  for ( unsigned int i = TImage::ImageDimension ; i < MeasurementVectorType::Length ; ++i )
    {
    m_TempVector[i] = m_TempRangeVector[i - TImage::ImageDimension] 
      / m_NormalizationFactors[i] ;
    }
  
  return m_TempVector ;
}

template < class TImage >
inline void
JointDomainImageToListAdaptor< TImage >
::ComputeRegion(const MeasurementVectorType& mv, 
                const double radius,
                ImageRegionType& region)
{
  ImageIndexType beginIndex ;
  ImageSizeType size ;

  for ( unsigned int i = 0 ; i < TImage::ImageDimension ; ++i )
    {
    m_TempPoint[i] = m_NormalizationFactors[i] * (mv[i] - radius) ;
    size[i] = (unsigned long)(2.0 * m_NormalizationFactors[i] * radius 
                              / this->GetImage()->GetSpacing()[i]) ;
    }

  this->GetImage()->TransformPhysicalPointToIndex(m_TempPoint , beginIndex ) ;

  for ( unsigned int i = 0 ; i < TImage::ImageDimension ; ++i )
    {
    if ( beginIndex[i] < m_ImageBeginIndex[i] )
      {
      beginIndex[i] = m_ImageBeginIndex[i] ;
      size[i] -= (m_ImageBeginIndex[i] - beginIndex[i]) ;
      }

    if ( static_cast<typename ImageIndexType::IndexValueType>(beginIndex[i] + size[i] - 1) > m_ImageEndIndex[i] )
      {
      size[i] = m_ImageEndIndex[i] - beginIndex[i] + 1 ;
      }
    }
  
  region.SetIndex( beginIndex ) ;
  region.SetSize( size ) ;
}

template < class TImage >
inline void
JointDomainImageToListAdaptor< TImage >
::Search(const MeasurementVectorType& mv, 
         const double radius, 
         InstanceIdentifierVectorType& result)
{
  ImageRegionType region ;
  this->ComputeRegion( mv, radius, region ) ;

  InstanceIdentifier id ;
  result.clear() ;
  ImageIteratorType iter( this->GetImage(), region ) ;
  iter.GoToBegin() ;
  bool isWithinRange ;
  double sum ;
  double squaredRadius = radius * radius ;
  double temp ;
  while ( !iter.IsAtEnd() )
    {
    id = this->GetImage()->ComputeOffset(iter.GetIndex()) ;
    m_TempVector = this->GetMeasurementVector( id ) ;
    isWithinRange = true ; 
    sum = 0.0 ;
    for ( unsigned int i = 0 ; i < MeasurementVectorSize ; ++i )
      {
      temp = (m_TempVector[i] - mv[i]) ;
      sum += temp * temp ;
      if ( sum > squaredRadius )
        {
        isWithinRange = false ;
        break ;
        }
      }

    if ( isWithinRange )
      {
      result.push_back(id) ;
      }
    ++iter ;
    }
}

} // end of namespace Statistics 
} // end of namespace itk

#endif



