/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarityRegistrationMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
  /** Standard class typedefs. */
  typedef SimilarityRegistrationMetric  Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimilarityRegistrationMetric, Object);

  /**  Type of the reference. */
  typedef typename TMapper::DomainType          ReferenceType;
  typedef typename ReferenceType::ConstPointer  ReferenceConstPointer;

  /**  Type of the target. */
  typedef TTarget               TargetType;

  /**  Type of the mapper. */
  typedef TMapper               MapperType;

  /**  Type of the measure. */
  typedef TMeasure              MeasureType;

  /**  Type of the measure. */
  typedef TDerivative           DerivativeType;

  /**  Pointer type for the target.  */
  typedef typename TargetType::ConstPointer TargetConstPointer;

  /**  Pointer type for the mapper. */
  typedef typename MapperType::Pointer MapperPointer;

  /** Method for execute the algorithm. */
  virtual void Compute(void);

  /** Connect the reference.  */
  void SetReference( const ReferenceType * );

  /** Return the reference.  */
  ReferenceConstPointer  GetReference( void );

  /** Connect the target.  */
  itkSetConstObjectMacro( Target, TargetType );

  /** Get the target. */
  itkGetConstObjectMacro( Target, TargetType );

  /** Get the match measure value. */
  itkGetMacro( MatchMeasure, MeasureType );

  /** Get the derivatives of the match measure. */
  itkGetMacro( MatchMeasureDerivatives, DerivativeType );
  
  /** Connect the mapper. */
  itkSetObjectMacro( Mapper, MapperType );

  /** Get a pointer to the mapper.  */
  itkGetObjectMacro( Mapper, MapperType );

protected:
  SimilarityRegistrationMetric();
  virtual ~SimilarityRegistrationMetric() {};
  void PrintSelf(std::ostream& os, Indent indent) const;


  MeasureType                 m_MatchMeasure;
  DerivativeType              m_MatchMeasureDerivatives;

private:
  SimilarityRegistrationMetric(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  TargetConstPointer          m_Target;
  MapperPointer               m_Mapper;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimilarityRegistrationMetric.txx"
#endif

#endif



