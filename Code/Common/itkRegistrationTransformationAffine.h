/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegistrationTransformationAffine.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRegistrationTransformationAffine_h
#define __itkRegistrationTransformationAffine_h

#include "itkObject.h"
#include "itkRegistrationTransformation.h"
#include "itkPoint.h"
#include "itkVector.h"


namespace itk
{
  
/** \class RegistrationTransformationAffine
 * \brief Generic Affine Transformation for a registration method
 *
 * This Class define the generic interface for an Affine Transformation 
 * It provides Transform() and InverseTransform() methods that works 
 * over Points and Vectors.
 *
 */

template <class TParameters,unsigned int NDimensions>
class ITK_EXPORT  RegistrationTransformationAffine : 
        public RegistrationTransformation<TParameters>

{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegistrationTransformationAffine  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;


  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;


  /** 
   * Type of the input parameters
   */
  typedef    TParameters      ParametersType;


 /** 
   * Pointer Type to input parameters
   */
  typedef    typename ParametersType::Pointer  ParametersPointer;


 /** 
   * Point Type
   */
  typedef    Point<NDimensions,double>      PointType;


 /** 
   * Vector Type
   */
  typedef    Vector<double,NDimensions>     VectorType;


 /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegistrationTransformationAffine, RegistrationTransformation);


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Transform a Point using the Affine transformation
   */
   PointType Transform( const PointType & point );


  /**
   * Transform a Vector using the Affine transformation
   */
   VectorType Transform( const VectorType & point );


  /**
   * Inverse Transform a Point using the Affine transformation
   */
   PointType InverseTransform( const PointType & point );


  /**
   * Inverse Transform a Vector using the Affine transformation
   */
   VectorType InverseTransform( const VectorType & point );




protected:

  RegistrationTransformationAffine();
  virtual ~RegistrationTransformationAffine() {};
  RegistrationTransformationAffine(const Self&);
  const Self & operator=(const Self&);


private:


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationTransformationAffine.txx"
#endif

#endif



