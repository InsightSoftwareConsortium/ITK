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
template <class TPoint>
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
   *  Point type.
   *  it defines a position in the optimization search space
   */
  typedef TPoint  PointType;

  /**
   *   Set the position to initialize the optimization  
   */
  void SetInitialPosition( const PointType & initialPosition ) 
    { m_InitialPosition = initialPosition; }

  /**
   *   Get the position to initialize the optimization  
   */
  const PointType & GetInitialPosition( void ) const 
    { return m_InitialPosition; }

  /**
   *   Get current position of the optimization  
   */
  const PointType & GetCurrentPosition( void ) const 
    { return m_CurrentPosition; }


protected:

  Optimizer() { };
  virtual ~Optimizer() {};
  Optimizer(const Self&) {}
  void operator=(const Self&) {}

  /**
   *   Set the current position 
   */
  void SetCurrentPosition( const PointType & currentPosition ) 
    { m_CurrentPosition = currentPosition; }


private:
  
  PointType     m_InitialPosition;
  PointType     m_CurrentPosition;
  
};

} // end namespace itk

#endif



