/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJointDomainImageToListAdaptor.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkJointDomainImageToListAdaptor_txx
#define _itkJointDomainImageToListAdaptor_txx

namespace itk{ 
namespace Statistics{

template < class TImage >
JointDomainImageToListAdaptor< TImage >
::JointDomainImageToListAdaptor()
{
  m_NormalizationFactors.Fill( 1.0f ) ;
  m_DistanceMetric = DistanceMetricType::New() ;
  m_PreviousRadius = 0.0 ;
  m_Cache = CacheType::New() ;
  m_CacheAvailable = false ;
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
void
JointDomainImageToListAdaptor< TImage >
::GenerateCache()
{
//   m_Cache->Resize( this->Size() ) ;

//   for ( unsigned long id = 0 ; id < this->Size() ; ++id )
//     {
//     m_Cache->SetMeasurementVector( id, this->GetMeasurementVector( id ) ) ;
//     }
//   m_CacheAvailable = true ;
}

template < class TImage >
inline typename JointDomainImageToListAdaptor< TImage >::MeasurementVectorType
JointDomainImageToListAdaptor< TImage >
::GetMeasurementVector(const InstanceIdentifier &id)
{
//   if ( m_CacheAvailable )
//     {
//     return m_Cache->GetMeasurementVector( id ) ;
//     }

//  Point<MeasurementType, TImage::ImageDimension> point ;
  ImageIndexType index = 
    this->GetImage()->ComputeIndex( id ) ;
//  this->GetImage()->TransformIndexToPhysicalPoint( index, point ) ;
  
//   for ( unsigned int i = 0 ; i < TImage::ImageDimension ; ++i )
//     {
//     m_TempVector[i] = point[i] / m_NormalizationFactors[i] ;
//     }
  
  for ( unsigned int i = 0 ; i < TImage::ImageDimension ; ++i )
    {
    m_TempVector[i] = index[i] / m_NormalizationFactors[i] ;
    }

  if( m_UseBuffer )
    {
    m_TempRangeVector =  
      *(reinterpret_cast< RangeDomainMeasurementVectorType* >
        (&(*m_PixelContainer)[id]))  ;
    }
  else
    {
    m_TempRangeVector = 
      *(reinterpret_cast< RangeDomainMeasurementVectorType* >
        (&(this->GetImage()->GetPixel( index ) ) ) ) ;
    }

  for ( unsigned int i = TImage::ImageDimension ; i < MeasurementVectorType::Length ; ++i )
    {
    m_TempVector[i] = 
      m_TempRangeVector[i - TImage::ImageDimension] / m_NormalizationFactors[i] ;
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
  ImageIndexType endIndex ;
  ImageSizeType size ;

  for ( unsigned int i = 0 ; i < TImage::ImageDimension ; ++i )
    {
    beginIndex[i] = (ImageIndexValueType) (mv[i] * m_NormalizationFactors[i]
      - m_NormalizationFactors[i] * radius) ;
    if ( beginIndex[i] < m_ImageBeginIndex[i] )
      {
      beginIndex[i] = m_ImageBeginIndex[i] ;
      }
    endIndex[i] = (ImageIndexValueType) (mv[i] * m_NormalizationFactors[i] 
      + m_NormalizationFactors[i] * radius) ;
    if ( endIndex[i] > m_ImageEndIndex[i] )
      {
      endIndex[i] = m_ImageEndIndex[i] ;
      }
    size[i] = endIndex[i] - beginIndex[i] + 1 ;
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

  typename DistanceMetricType::OriginType origin ;
  for ( unsigned int i = 0 ; i < MeasurementVectorSize ; ++i )
    {
    origin[i] = mv[i] ;
    }

  m_DistanceMetric->SetOrigin( origin ) ;

  InstanceIdentifier id ;
  result.clear() ;
  ImageIteratorType iter( this->GetImage(), region ) ;
  iter.GoToBegin() ;
  while ( !iter.IsAtEnd() )
    {
    id = this->GetImage()->ComputeOffset(iter.GetIndex()) ;
     if ( m_DistanceMetric->IsWithinRange( this->GetMeasurementVector( id ),
                                           radius ) ) 
      {
        result.push_back(id) ;
      }
    ++iter ;
    }

//  std::cout << "DEBUG: search done." << std::endl ;
}

} // end of namespace Statistics 
} // end of namespace itk

#endif



