/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarityRegistrationMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkSimilarityRegistrationMetric_txx
#define _itkSimilarityRegistrationMetric_txx



namespace itk
{

/**
 * Constructor
 */
template <class TTarget, class TMapper, 
          class TMeasure, class TDerivative>
SimilarityRegistrationMetric<TTarget,TMapper,TMeasure,TDerivative>
::SimilarityRegistrationMetric()
{
  m_Target = TargetType::New();
  m_Mapper = MapperType::New();
}


/**
 * Set Reference 
 */
template <class TTarget, class TMapper, 
          class TMeasure,class TDerivative>
void
SimilarityRegistrationMetric<TTarget,TMapper,TMeasure,TDerivative>
::SetReference( ReferenceType * reference ) 
{
  this->m_Mapper->SetDomain( reference );
}


/**
 * Get Reference 
 */
template <class TTarget, class TMapper, 
          class TMeasure,class TDerivative>
typename SimilarityRegistrationMetric<TTarget,TMapper,TMeasure,TDerivative>::ReferencePointer
SimilarityRegistrationMetric<TTarget,TMapper,TMeasure,TDerivative>
::GetReference( void )
{
  return this->m_Mapper->GetDomain();
}




/**
 * GenerateData Performs the evaluation of similarity
 */
template <class TTarget, class TMapper, 
          class TMeasure, class TDerivative>
void
SimilarityRegistrationMetric<TTarget,TMapper,TMeasure,TDerivative>
::Compute( void )
{
  // Evaluate similarity here...
  // store the result in m_MatchMeasure
  // and the derivatives in m_MatchMeasureDerivatives
}





} // end namespace itk

#endif
