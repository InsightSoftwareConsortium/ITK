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


namespace itk
{

/**
 * Constructor
 */
template <class TReference, class TTarget, 
          class TMapper, class TMeasure, class TDerivative>
SimilarityRegistrationMetric<TReference,TTarget,TMapper,TMeasure,TDerivative>
::SimilarityRegistrationMetric()
{
}


/**
 * Set Reference 
 */
template <class TReference, class TTarget, 
          class TMapper, class TMeasure,class TDerivative>
void
SimilarityRegistrationMetric<TReference,TTarget,TMapper,TMeasure,TDerivative>
::SetReference( TReference * reference ) 
{
  this->m_Reference = reference;
}



/**
 * Set Target 
 */
template <class TReference, class TTarget, 
          class TMapper, class TMeasure, class TDerivative>
void
SimilarityRegistrationMetric<TReference,TTarget,TMapper,TMeasure,TDerivative>
::SetTarget( TTarget * target ) 
{
  this->m_Target = target;
}


/**
 * Set Mapper
 */
template <class TReference, class TTarget, 
          class TMapper, class TMeasure, class TDerivative>
void
SimilarityRegistrationMetric<TReference,TTarget,TMapper,TMeasure,TDerivative>
::SetMapper( TMapper * mapper ) 
{
  this->m_Mapper = mapper;
}






/**
 * GenerateData Performs the evaluation of similarity
 */
template <class TReference, class TTarget, 
          class TMapper, class TMeasure, class TDerivative>
void
SimilarityRegistrationMetric<TReference,TTarget,TMapper,TMeasure,TDerivative>
::Compute( void )
{
  // Evaluate similarity here...
  // store the result in m_MatchMeasure
  // and the derivatives in m_MatchMeasureDerivatives
}





} // end namespace itk
