/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRegistrationTransform_h
#define __itkRegistrationTransform_h

#include "itkObject.h"

namespace itk
{

/** \class RegistrationTransform
 * \brief Base class for registration methods
 *
 * This Class define the generic interface for a registration method.
 * The basic elements of a registration method are:
 *   - Metric to compare the reference and the target
 *   - Transformation used to register the reference against the target
 *   - Optimization method used to search for the best transformation
 * 
 * Registration is not limited to Images, and for this reason
 * this class is templated over the type of the reference object,
 * the target object and the transformation. This types are obtained
 * from the Metric type, to reduce the number of redundant
 * template parameters
 * 
 * \ingroup Transforms 
 */
template <class TMetric, class TOptimizer >
class ITK_EXPORT RegistrationTransform : public Object 
{
public:
  /** Standard class typedefs. */
  typedef RegistrationTransform  Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(RegistrationTransform, Object);

  /**  Type of the metric. */
  typedef TMetric               MetricType;

  /**  Type of the optimizer. */
  typedef TOptimizer            OptimizerType;

  /**  Type of the reference. */
  typedef typename MetricType::ReferenceType  ReferenceType;

  /**  Type of the metric. */
  typedef typename MetricType::TargetType  TargetType;

  /**  Type of the mapper. */
  typedef typename MetricType::MapperType  MapperType;

  /**  Type of the transformation. */
  typedef typename MapperType::TransformationType  TransformationType;
  
  /**  Pointer type for the reference.  */
  typedef typename ReferenceType::ConstPointer ReferenceConstPointer;
  
  /**  Pointer type for the target.  */
  typedef typename TargetType::ConstPointer TargetConstPointer;

  /**  Pointer type for the transformation. */
  typedef typename TransformationType::Pointer TransformationPointer;

  /**  Pointer type for the metric. */
  typedef typename MetricType::Pointer        MetricPointer;

  /**  Pointer type for the mapper. */
  typedef typename MapperType::Pointer        MapperPointer;

  /**  Pointer type for the optimizer. */
  typedef typename OptimizerType::Pointer     OptimizerPointer;

  /** Method that initiates the registration. */
  void StartRegistration(void);

  /** Set/Get the target, */
  void SetTarget( const TargetType * );
  itkGetConstObjectMacro( Target, TargetType );
   
  /** Set/Get the reference. */
  void SetReference( const  ReferenceType * );
  itkGetConstObjectMacro( Reference, ReferenceType );

  /** Set/Get the transformation. */
  void SetTransformation( TransformationType * );
  itkGetMacro( Transformation, TransformationPointer );

  /** Get the optimizer. */
  itkGetMacro( Optimizer, OptimizerPointer );

protected:
  RegistrationTransform();
  virtual ~RegistrationTransform();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  RegistrationTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  TargetConstPointer         m_Target;
  ReferenceConstPointer      m_Reference;
  TransformationPointer      m_Transformation;
  MapperPointer              m_Mapper;  
  MetricPointer              m_Metric;
  OptimizerPointer           m_Optimizer;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationTransform.txx"
#endif

#endif



