/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMethod.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegistrationMethod_h
#define __itkRegistrationMethod_h

#include "itkProcessObject.h"

namespace itk
{

/** \class RegistrationMethod
 * \brief Base class for Registration Methods
 *
 * This class defines the generic interface for a registration method.
 * The basic elements of a registration method are defined in a Traits
 * class
 * \deprecated This class has been replaced by the less templated ImageRegistrationMethod class
 * \ingroup RegistrationFilters  Deprecated
 */
template <class TTraits>
class ITK_EXPORT RegistrationMethod : public ProcessObject 
{
public:
  /** Standard class typedefs. */
  typedef RegistrationMethod  Self;
  typedef ProcessObject  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(RegistrationMethod, Object);

  /**  Type of the target. */
  typedef typename TTraits::TargetType              TargetType;
  typedef typename TargetType::ConstPointer              TargetConstPointer;

  /**  Type of the metric. */
  typedef typename TTraits::MetricType              MetricType;

  /**  Type of the mapper. */
  typedef typename TTraits::MapperType              MapperType;

  /**  Type of the transformation. */
  typedef typename TTraits::TransformationType TransformationType;

  /**  Type of the reference. */
  typedef typename TTraits::ReferenceType              ReferenceType;
  typedef typename ReferenceType::ConstPointer         ReferenceConstPointer;

  /**  Type of the optimizer. */
  typedef typename TTraits::OptimizerType         OptimizerType;

  /** Type of the Transformation parameters This is the same type used to
   *  represent the search space of the optimization algorithm */
  typedef typename TTraits::ParametersType         ParametersType;

  /** Method that initiates the registration. */
  void StartRegistration(void);

  /** Set the target. */
  void SetTarget( const TargetType * Target );

  /** Set the reference. */
  void SetReference( const ReferenceType * Reference );

  /** Set the optimizer. */
  itkSetObjectMacro( Optimizer,  OptimizerType );

  /** Set the metric. */
  itkSetObjectMacro( Metric, MetricType );

  /** Get the reference. */
  const ReferenceType * GetReference( void );

  /** Get the target. */
  const TargetType *  GetTarget( void );

  /** Get the optimizer. */
  itkGetObjectMacro( Optimizer, OptimizerType );

  /** Get the metric. */
  itkGetObjectMacro( Metric, MetricType );

protected:
  RegistrationMethod();
  virtual ~RegistrationMethod();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  RegistrationMethod(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  typename MetricType::Pointer              m_Metric;
  typename OptimizerType::Pointer           m_Optimizer;

};


} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationMethod.txx"
#endif

#endif



