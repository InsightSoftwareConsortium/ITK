/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOptimizer_h
#define __itkOptimizer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkExceptionObject.h"
#include "itkScaleTransform.h"


namespace itk
{
  
/** \class Optimizer
 * \brief Generic representation for an optimization method 
 *
 * \ingroup Numerics Optimizers
 */
template <class TCostFunction>
class ITK_EXPORT Optimizer : public Object 
{
public:
  /** Standard class typedefs. */
  typedef Optimizer  Self;
  typedef   Object  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

 /** Run-time type information (and related methods). */
  itkTypeMacro(Optimizer, Object);

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef typename TCostFunction::ParametersType ParametersType;

  /** Dimension of the search space. */
  enum { SpaceDimension = TCostFunction::SpaceDimension };
 
  /**  Transform type.
   *  It defines a transform to be applied to points before 
   *  being evaluated in the cost function. This allows to 
   *  map to a more convenient space. In particular this is
   *  used to normalize parameter spaces in which some parameters
   *  have a different dynamic range.   */
  typedef     ScaleTransform<double,SpaceDimension>      TransformType;

  /**  Measure type.
   *  It defines a type used to return the cost function value. */
  typedef typename TCostFunction::MeasureType MeasureType;

  /**  Derivative type.
   *  It defines a type used to return the cost function derivative.  */
  typedef typename TCostFunction::DerivativeType DerivativeType;

  /**  Set the position to initialize the optimization. */
  itkSetMacro(InitialPosition, ParametersType);

  /** Get the position to initialize the optimization. */
  itkGetConstMacro(InitialPosition, ParametersType);

  /** Get current position of the optimization. */
  itkGetConstMacro( CurrentPosition, ParametersType );

  /** Set current transform. */
  itkSetObjectMacro( Transform, TransformType );

  /** Get current transform. */
  itkGetObjectMacro( Transform, TransformType );

protected:
  Optimizer() 
    { m_Transform = TransformType::New(); };
  virtual ~Optimizer() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Set the current position. */
  itkSetMacro( CurrentPosition, ParametersType );

private:
  Optimizer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  ParametersType          m_InitialPosition;
  ParametersType          m_CurrentPosition;

  typename TransformType::Pointer  m_Transform; 

};

} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOptimizer.txx"
#endif

#endif



