/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarityRegistrationMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
::SetReference( const ReferenceType * reference ) 
{
  this->m_Mapper->SetDomain( reference );
}


/**
 * Get Reference 
 */
template <class TTarget, class TMapper, 
          class TMeasure,class TDerivative>
typename SimilarityRegistrationMetric<TTarget,TMapper,TMeasure,TDerivative>::ReferenceConstPointer
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


/**
 * PrintSelf
 */
template <class TTarget, class TMapper, 
          class TMeasure, class TDerivative>
void
SimilarityRegistrationMetric<TTarget,TMapper,TMeasure,TDerivative>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Target: " << m_Target.GetPointer() << std::endl;
  os << indent << "Mapper: " << m_Mapper.GetPointer() << std::endl;

  os << indent << "MatchMeasure: " << m_MatchMeasure << std::endl;
  os << indent << "MatchMeasureDerivatives: ";
  os << m_MatchMeasureDerivatives << std::endl;

}


} // end namespace itk

#endif
