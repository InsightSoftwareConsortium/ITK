/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarityRegistrationMetric.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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





} // end namespace itk

#endif
