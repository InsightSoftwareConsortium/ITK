/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ClusteringOutputEvaluator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __ClusteringOutputEvaluator_txx
#define __ClusteringOutputEvaluator_txx

#include "ClusteringOutputEvaluator.h"

template< class TSample >
ClusteringOutputEvaluator< TSample >
::ClusteringOutputEvaluator()
{
  m_Truth = 0 ;
  m_NumberOfClasses = 0 ;
}

template< class TSample >
ClusteringOutputEvaluator< TSample > 
::~ClusteringOutputEvaluator()
{
}

template< class TSample >
void
ClusteringOutputEvaluator< TSample > 
::SetTruth(TSample* sample)
{
  m_Truth = sample ;
}

template< class TSample >
void
ClusteringOutputEvaluator< TSample > 
::SetClusteringOutput(ClusteringOutputType* labels)
{
  m_ClusteringOutput = labels ;
}

template< class TSample >
const int 
ClusteringOutputEvaluator< TSample > 
::GetSize(const unsigned int classLabel ) const
{
  return m_Sizes[this->GetClassIndex(classLabel)] ;
}

template< class TSample >
const int
ClusteringOutputEvaluator< TSample > 
::GetNumberOfMatches(const unsigned int classLabel ) const
{
  return m_NumberOfMatches[this->GetClassIndex(classLabel)] ;
}
  
template< class TSample >
const std::vector< int >&
ClusteringOutputEvaluator< TSample > 
::GetInclusionErrors(const unsigned int classLabel) const
{
  return m_InclusionErrors[this->GetClassIndex(classLabel)] ;
}

template< class TSample >
const std::vector< int >&
ClusteringOutputEvaluator< TSample > 
::GetExclusionErrors(const unsigned int classLabel) const 
{
  return m_ExclusionErrors[this->GetClassIndex(classLabel)] ;
}

template< class TSample >
void
ClusteringOutputEvaluator< TSample > 
::SetClassLabels(const std::vector< unsigned int >& classLabels)
{
  m_ClassLabels = classLabels ;
  m_NumberOfClasses = m_ClassLabels.size() ;
  m_Sizes.resize(m_NumberOfClasses) ;
  m_NumberOfMatches.resize(m_NumberOfClasses) ;
  m_InclusionErrors.resize(m_NumberOfClasses) ;
  m_ExclusionErrors.resize(m_NumberOfClasses) ;
  for ( int i = 0 ; i < m_NumberOfClasses ; i++ )
    {
      m_InclusionErrors[i].resize(m_NumberOfClasses) ;
      m_ExclusionErrors[i].resize(m_NumberOfClasses) ;
    }
}

template< class TSample >
unsigned int
ClusteringOutputEvaluator< TSample > 
::GetClassIndex(const unsigned int classLabel) const
{
  for ( unsigned int i = 0 ; i < m_NumberOfClasses ; i++ )
    {
      if ( classLabel == m_ClassLabels[i])
        {
          return i ;
        }
    }
 
 return 0 ;
}

template< class TSample >
void
ClusteringOutputEvaluator< TSample > 
::GenerateData()
{

  for ( int i = 0 ; i < m_NumberOfClasses ; i++ )
    {
      m_Sizes[i] = 0 ;
      m_NumberOfMatches[i] = 0 ;
      std::fill(m_InclusionErrors[i].begin(), m_InclusionErrors[i].end(), 0) ;
      std::fill(m_ExclusionErrors[i].begin(), m_ExclusionErrors[i].end(), 0) ;
    }

//   std::copy(m_ClassLabels.begin(), m_ClassLabels.end(), 
//             std::ostream_iterator<std::string>(std::cout, " ") ) ;
//   std::cout << std::endl ;
  typename TSample::Iterator t_iter = m_Truth->Begin() ;
  ClusteringOutputType::iterator o_iter = m_ClusteringOutput->begin() ;
  unsigned int trueLabel ;
  unsigned int estimatedLabel ;
  while ( o_iter != m_ClusteringOutput->end() )
    {
      
      trueLabel = this->GetClassIndex(t_iter.GetMeasurementVector()[0]) ;
      estimatedLabel = this->GetClassIndex(*o_iter) ;
      m_Sizes[estimatedLabel] += 1 ;
      
      if ( estimatedLabel == trueLabel)
        {
          m_NumberOfMatches[trueLabel] += 1 ;
        }
      else
        {
          // error
          m_ExclusionErrors[trueLabel][estimatedLabel] += 1 ;
          m_InclusionErrors[estimatedLabel][trueLabel] += 1 ;
        }
      ++t_iter ;
      ++o_iter ;
    }
}

#endif
