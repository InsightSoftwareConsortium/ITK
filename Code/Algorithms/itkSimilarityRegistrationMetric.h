/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarityRegistrationMetric.h
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
#ifndef __itkSimilarityRegistrationMetric_h
#define __itkSimilarityRegistrationMetric_h

#include "itkObject.h"

namespace itk
{
  
/** \class SimilarityRegistrationMetric
 * \brief Computes similarity between two objects to be registered
 *
 * This Class is templated over the type of the objects to be registered and
 * over the type of transformation to be used.
 *
 * SmartPointer to this three objects are received, and using them, this
 * class computes a value(s) that measures the similarity of the target
 * against the reference object once the transformation is applied to it.
 *
 * The class is templated over the kind of value that can be produced as
 * measure of similarity. That allows to cover methods that produce residuals
 * as well as methods that produces just one double as result.
 *
 * \ingroup RegistrationMetrics
 *
 */

template <class TTarget,  class TMapper, 
          class TMeasure, class TDerivative > 
class ITK_EXPORT SimilarityRegistrationMetric : public Object 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef SimilarityRegistrationMetric  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   *  Type of the Reference
   */
  typedef typename TMapper::DomainType          ReferenceType;
  typedef typename ReferenceType::ConstPointer  ReferenceConstPointer;


  /**
   *  Type of the Target
   */
  typedef TTarget               TargetType;
 

  /**
   *  Type of the Mapper
   */
  typedef TMapper               MapperType;
  

  /**
   *  Type of the measure
   */
  typedef TMeasure              MeasureType;
 

  /**
   *  Type of the measure
   */
  typedef TDerivative           DerivativeType;


  /**
   *  Pointer type for the Target 
   */
  typedef typename TargetType::ConstPointer TargetConstPointer;


  /**
   *  Pointer type for the Mapper
   */
  typedef typename MapperType::Pointer MapperPointer;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(SimilarityRegistrationMetric, Object);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Method for execute the algorithm
   */
   virtual void Compute(void);
  

  /**
   * Connect the Reference 
   */
   void SetReference( const ReferenceType * );


  /**
   * Return the Reference 
   */
   ReferenceConstPointer  GetReference( void );


  /**
   * Connect the Target 
   */
   itkSetConstObjectMacro( Target, TargetType );


  /**
   * Get the Target
   */
   itkGetConstObjectMacro( Target, TargetType );


  /**
   * Get the Match Measure Value
   */
   itkGetMacro( MatchMeasure, MeasureType );


  /**
   * Get the Derivatives of the Match Measure
   */
   itkGetMacro( MatchMeasureDerivatives, DerivativeType );

  
  /**
   * Connect the Mapper
   */
   itkSetObjectMacro( Mapper, MapperType );


   /**
    * Get a pointer to the Mapper
    */
   itkGetObjectMacro( Mapper, MapperType );


protected:

  SimilarityRegistrationMetric();
  virtual ~SimilarityRegistrationMetric() {};
  SimilarityRegistrationMetric(const Self&) {}
  void operator=(const Self&) {}

private:

  TargetConstPointer          m_Target;
  MapperPointer               m_Mapper;

protected:

  MeasureType                 m_MatchMeasure;
  DerivativeType              m_MatchMeasureDerivatives;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimilarityRegistrationMetric.txx"
#endif

#endif



