/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptimizer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkOptimizer_h
#define __itkOptimizer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkExceptionObject.h"


namespace itk
{
  
/** \class Optimizer
 * \brief Generic representation for an optimization method 
 */
template <class TParameters>
class ITK_EXPORT Optimizer : public Object 

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Optimizer  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef   Object  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Optimizer, Object);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);


  /**
   * ParametersType typedef
   * This is the type used to pass points in the
   * search space to the optimizer
   */
  typedef   TParameters    ParametersType;


  /**
   * ParametersPointer typedef
   * This is the type used to pass points in the
   * search space to the optimizer
   */
  typedef   typename ParametersType::Pointer ParametersPointer;


  /**
   * Set the vector of parameters
   */
  itkSetObjectMacro( Parameters, ParametersType );

  /**
   * Get the vector of parameters
   */
  itkGetConstReferenceObjectMacro( Parameters, ParametersType );


protected:

  Optimizer() { m_Parameters = ParametersType::New();  };
  virtual ~Optimizer() {};
  Optimizer(const Self&) {}
  void operator=(const Self&) {}

private:

  ParametersPointer     m_Parameters;

};

} // end namespace itk

#endif



