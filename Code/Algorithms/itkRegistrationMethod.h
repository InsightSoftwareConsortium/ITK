/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationMethod.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationMethod_h
#define __itkRegistrationMethod_h

#include "itkProcessObject.h"

namespace itk
{

/**
 * \class RegistrationMethod
 * \brief Base class for Registration Methods
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
 */

template <class TReference, class TTarget>
class ITK_EXPORT RegistrationMethod : public ProcessObject 
{
public:
  /**
   * Standard "Self" typedef.
   */
   typedef RegistrationMethod  Self;


  /**
   * Standard "Superclass" typedef.
   */
   typedef ProcessObject  Superclass;


  /** 
   * Smart pointer typedef support 
   */
   typedef SmartPointer<Self>   Pointer;
   typedef SmartPointer<const Self>  ConstPointer;

  /**
   *  Type of the Reference
   */
   typedef TReference  ReferenceType;

  /**
   *  Type of the Target
   */
   typedef TTarget TargetType;

  /**
   *  Pointer type for the Reference 
   */
   typedef typename ReferenceType::Pointer ReferencePointer;

  
  /**
   *  Pointer type for the Target 
   */
   typedef typename TargetType::Pointer TargetPointer;


  /** 
   * Run-time type information (and related methods).
   */
   itkTypeMacro(RegistrationMethod, Object);


  /**
   * Method for creation through the object factory.
   */
   itkNewMacro(Self);
  

  /**
   * Method that initiates the registration.
   */
   int StartRegistration(void);


  /**
   * Set the Target
   */
   itkSetObjectMacro( Target, TargetType );

  /**
   * Set the Reference
   */
   itkSetObjectMacro( Reference,  ReferenceType );

  /**
   * Get the Reference
   */
   itkGetObjectMacro( Reference, ReferenceType );
   
  /**
   * Get the Target
   */
   itkGetObjectMacro( Target, TargetType );


protected:

  RegistrationMethod(){};
  virtual ~RegistrationMethod(){};
  RegistrationMethod(const Self&);
  const Self & operator=(const Self&);
 

private:

  TargetPointer              m_Target;
  ReferencePointer           m_Reference;

};


} // end namespace itk


#endif



