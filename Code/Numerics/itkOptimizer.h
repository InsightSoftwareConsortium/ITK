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
template <class TCostFunction>
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
   *  Parameters type.
   *  it defines a position in the optimization search space
   */
  typedef typename TCostFunction::ParametersType ParametersType;


  /**
   *  Measure type.
   *  it defines a type used to return the cost function value 
   */
  typedef typename TCostFunction::MeasureType MeasureType;


  /**
   *  Derivative type.
   *  it defines a type used to return the cost function derivative 
   */
  typedef typename TCostFunction::DerivativeType DerivativeType;


  /**
   *   Set the position to initialize the optimization  
   */
  void SetInitialPosition( const ParametersType & initialPosition ) 
    { m_InitialPosition = initialPosition; }

  /**
   *   Get the position to initialize the optimization  
   */
  const ParametersType & GetInitialPosition( void ) const 
    { return m_InitialPosition; }

  /**
   *   Get current position of the optimization  
   */
  const ParametersType & GetCurrentPosition( void ) const 
    { return m_CurrentPosition; }


protected:

  Optimizer() { };
  virtual ~Optimizer() {};
  Optimizer(const Self&) {}
  void operator=(const Self&) {}

  /**
   *   Set the current position 
   */
  void SetCurrentPosition( const ParametersType & currentPosition ) 
    { m_CurrentPosition = currentPosition; }


private:
  
  ParametersType     m_InitialPosition;
  ParametersType     m_CurrentPosition;
  
};

} // end namespace itk

#endif



