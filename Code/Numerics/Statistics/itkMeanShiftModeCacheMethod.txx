/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanShiftModeCacheMethod.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMeanShiftModeCacheMethod_txx
#define __itkMeanShiftModeCacheMethod_txx

namespace itk{ 
namespace Statistics{

template< class TMeasurementVector >
MeanShiftModeCacheMethod< TMeasurementVector >
::MeanShiftModeCacheMethod()
{
  m_MaximumEntries = 200 ;
  m_MaximumConsecutiveFailures = 5 ;
  m_HitRatioThreshold = 0.75 ;

  m_HitsSuccess = 0 ;
  m_NumberOfRequests = 0 ;
  m_ConsecutiveFailures = 0 ;

  m_TotalHitsSuccess = 0 ;
  m_TotalHitsFailure = 0 ;

  m_TotalTableSize = 0 ;
  m_TimesOfRebuilding = 0 ;
  m_TimesOfRebuildingByHitRatio = 0 ;
  m_TimesOfRebuildingByConsecutiveFailures = 0 ;
}

template< class TMeasurementVector >
MeanShiftModeCacheMethod< TMeasurementVector >
::~MeanShiftModeCacheMethod()
{
}

template< class TMeasurementVector >
void
MeanShiftModeCacheMethod< TMeasurementVector >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Maximum entries: " << m_MaximumEntries << std::endl ;
  os << indent << "Maximum consecutive failures: " 
     << m_MaximumConsecutiveFailures << std::endl ;
  os << indent << "Hit ratio threshold: " 
     << m_HitRatioThreshold << std::endl ;

  os << indent << "Last consecutive failures: " 
     << m_ConsecutiveFailures << std::endl ;
  os << indent << "Last successful hits: " 
     << m_HitsSuccess << std::endl ;
  os << indent << "Last number of requests: " 
     << m_NumberOfRequests << std::endl ;

  os << indent << "Total successful hits: " << m_TotalHitsSuccess << std::endl ;
  os << indent << "Total failed hits: " << m_TotalHitsFailure  << std::endl ;

  os << indent << "Total table size before destuction: " 
     << m_TotalTableSize << std::endl ;
  os << indent << "Number of cache rebuildings: " 
     << m_TimesOfRebuilding << std::endl ;
  os << indent << "Average cache table size: " ;
  if ( m_TimesOfRebuilding > 0 )
    {
    os << (float)(m_TotalTableSize / m_TimesOfRebuilding) << std::endl ;
    }
  else
    {
    os << m_TotalTableSize << std::endl ;
    }

  os << indent << "Number of cache rebuildings caused by hit ratio threshold: " 
     << m_TimesOfRebuildingByHitRatio << std::endl ;
  os << indent << "Number of cache rebuildings caused by consecutive failures: " 
     << m_TimesOfRebuildingByConsecutiveFailures << std::endl ;

  os << indent << "Cache table: " << &m_CacheTable << std::endl ;
}

template< class TMeasurementVector >
bool
MeanShiftModeCacheMethod< TMeasurementVector >
::SetMeasurementVector(MeasurementVectorType& source, 
                       MeasurementVectorType& target)
{
  if ( this->IsFull() )
    {
    return false ;
    }
  else
    {
    m_CacheTable[source] = target ;
    return true ;
    }
}

template< class TMeasurementVector >
bool
MeanShiftModeCacheMethod< TMeasurementVector >
::GetMeasurementVector(MeasurementVectorType& source,
                       MeasurementVectorType& target)
{
  typename CacheTableType::iterator iter = m_CacheTable.find( source ) ;
  ++m_NumberOfRequests ;
  if ( iter != m_CacheTable.end() )
    {
    target = iter->second ;
    ++m_HitsSuccess ;
    ++m_TotalHitsSuccess ;
    m_ConsecutiveFailures = 0 ;
    return true ;
    }
  else
    {
    ++m_TotalHitsFailure ;
    if ( this->IsFull() )
      {
      ++m_ConsecutiveFailures ;
      if ( float(m_HitsSuccess / m_NumberOfRequests) < m_HitRatioThreshold )
        {
        ++m_TimesOfRebuildingByHitRatio ;
        this->DestroyCacheTable() ;
        return false ;
        }
      
      if ( m_ConsecutiveFailures > m_MaximumConsecutiveFailures )
        {
        ++m_TimesOfRebuildingByConsecutiveFailures ;
        this->DestroyCacheTable() ;
        return false ;
        }
      }
    return false ;
    }
}

template< class TMeasurementVector >
bool
MeanShiftModeCacheMethod< TMeasurementVector >
::IsFull()
{
  if ( m_CacheTable.size() < m_MaximumEntries )
    {
    return false ;
    }
  else
    {
    return true ;
    }
}

template< class TMeasurementVector >
void
MeanShiftModeCacheMethod< TMeasurementVector >
::DestroyCacheTable()
{
  ++m_TimesOfRebuilding ;
  m_TotalTableSize += m_CacheTable.size() ;
  m_NumberOfRequests = 0 ;
  m_HitsSuccess = 0 ;
  m_ConsecutiveFailures = 0 ;
  m_CacheTable.clear() ;
}


} // end of namespace Statistics 
} // end of namespace itk

#endif

