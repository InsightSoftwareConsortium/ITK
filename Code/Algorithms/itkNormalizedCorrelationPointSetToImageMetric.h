/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizedCorrelationPointSetToImageMetric.h
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
#ifndef __itkNormalizedCorrelationPointSetToImageMetric_h
#define __itkNormalizedCorrelationPointSetToImageMetric_h

#include "itkSimilarityRegistrationMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"


namespace itk
{
  
/** \class NormalizedCorrelationPointSetToImageMetric
 * \brief Computes similarity between two objects to be registered
 *
 * This Class is templated over the type of the objects to be registered and
 * over the type of transformation to be used.
 *
 * SmartPointer to this three objects are received, and using them, this
 * class computes a value(s) that measures the similarity of the target
 * against the reference object once the transformation is applied to it.
 *
 *
 * \ingroup RegistrationMetrics
 *
 */

template < class TTarget, class TMapper > 
class ITK_EXPORT NormalizedCorrelationPointSetToImageMetric : 
public SimilarityRegistrationMetric< TTarget, TMapper, double,
                                     CovariantVector<double, TMapper::SpaceDimension > >

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef NormalizedCorrelationPointSetToImageMetric  Self;

  /**
   * Space dimension is the dimension of parameters space
   */
  enum { SpaceDimension = TMapper::SpaceDimension };
  enum { RangeDimension = 9};


  /**
   *  Type of the match measure
   */
  typedef double			        MeasureType;
 

  /**
   *  Type of the derivative of the match measure
   */
  typedef CovariantVector<MeasureType,
                          SpaceDimension >  DerivativeType;


  /**
   * Standard "Superclass" typedef.
   */
  typedef SimilarityRegistrationMetric< 
                       TTarget, TMapper,
                       MeasureType,DerivativeType >  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /**
   *  Type of the Mapper
   */
  typedef TMapper							MapperType;
  
  /**
   *  Type of the Reference
   */
  typedef typename MapperType::DomainType     ReferenceType;


  /**
   *  Type of the Target
   */
  typedef TTarget							TargetType;
 

  /**
   *  Pointer type for the Reference 
   */
  typedef typename ReferenceType::ConstPointer         ReferenceConstPointer;


  /**
   *  Pointer type for the Target 
   */
  typedef typename TargetType::ConstPointer            TargetConstPointer;


  /**
   *  Pointer type for the Mapper
   */
  typedef typename MapperType::Pointer            MapperPointer;


  /**
   *  Parameters type
   */
  typedef typename  TMapper::ParametersType       ParametersType;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(NormalizedCorrelationPointSetToImageMetric, Object);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
 
  /**
   * Get the Derivatives of the Match Measure
   */
  const DerivativeType & GetDerivative( const ParametersType & parameters );

  /**
   *  Get the Value for SingleValue Optimizers
   */
  MeasureType    GetValue( const ParametersType & parameters );


  /**
   *  Get Value and Derivatives for MultipleValuedOptimizers
   */
   void GetValueAndDerivative( const ParametersType & parameters,
       MeasureType & Value, DerivativeType  & Derivative );

 
protected:

  NormalizedCorrelationPointSetToImageMetric();
  virtual ~NormalizedCorrelationPointSetToImageMetric() {};
  NormalizedCorrelationPointSetToImageMetric(const Self&) {}
  void operator=(const Self&) {}

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNormalizedCorrelationPointSetToImageMetric.txx"
#endif

#endif



