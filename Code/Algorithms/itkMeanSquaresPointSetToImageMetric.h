/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanSquaresPointSetToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMeanSquaresPointSetToImageMetric_h
#define __itkMeanSquaresPointSetToImageMetric_h

#ifdef    COMPILE_OLD_REGISTRATION_FRAMEWORK

#include "itkSimilarityRegistrationMetric.h"
#include "itkCovariantVector.h"
#include "itkPoint.h"


namespace itk
{
  
/** \class MeanSquaresPointSetToImageMetric
 * \brief Computes similarity between two objects to be registered
 *
 * This Class is templated over the type of the objects to be registered and
 * over the type of transformation to be used.
 *
 * SmartPointer to this three objects are received, and using them, this
 * class computes a value(s) that measures the similarity of the target
 * against the reference object once the transformation is applied to it.
 *
 * \ingroup RegistrationMetrics
 */
template < class TTarget, class TMapper > 
class ITK_EXPORT MeanSquaresPointSetToImageMetric : 
public SimilarityRegistrationMetric< TTarget, TMapper, double,
                                     CovariantVector<double, TMapper::SpaceDimension > >
{
public:
  /** Space dimension is the dimension of parameters space. */
  itkStaticConstMacro(SpaceDimension, unsigned int,
                      TMapper::SpaceDimension);
  itkStaticConstMacro(RangeDimension, unsigned int, 9);

  /** Standard class typedefs. */
  typedef MeanSquaresPointSetToImageMetric  Self;
  typedef double MeasureType;
  typedef CovariantVector<MeasureType,itkGetStaticConstMacro(SpaceDimension) >  DerivativeType;
  typedef SimilarityRegistrationMetric< TTarget, TMapper,
                       MeasureType,DerivativeType >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
 
  /** Run-time type information (and related methods). */
  itkTypeMacro(MeanSquaresPointSetToImageMetric, Object);

  /**  Type of the mapper. */
  typedef TMapper MapperType;
  
  /**  Type of the reference. */
  typedef typename MapperType::DomainType ReferenceType;

  /**  Type of the target. */
  typedef TTarget TargetType;
 
  /**  Pointer type for the reference.  */
  typedef typename ReferenceType::ConstPointer ReferenceConstPointer;

  /**  Pointer type for the target.  */
  typedef typename TargetType::ConstPointer TargetConstPointer;

  /**  Pointer type for the mapper. */
  typedef typename MapperType::Pointer MapperPointer;

  /**  Parameters type. */
  typedef typename  TMapper::ParametersType       ParametersType;

  /** Get the derivatives of the match measure. */
  const DerivativeType & GetDerivative( const ParametersType & parameters );

  /**  Get the Value for single valued optimizers. */
  MeasureType GetValue( const ParametersType & parameters );

  /**  Get Value and Derivatives for multiple valued optimizers. */
   void GetValueAndDerivative( const ParametersType & parameters,
                               MeasureType& Value, DerivativeType& Derivative);

protected:
  MeanSquaresPointSetToImageMetric();
  virtual ~MeanSquaresPointSetToImageMetric() {};

private:  
  MeanSquaresPointSetToImageMetric(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanSquaresPointSetToImageMetric.txx"
#endif

#endif
#endif



