/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTranslationRegistrationTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkTranslationRegistrationTransform_h
#define __itkTranslationRegistrationTransform_h

#include "itkObject.h"
#include "itkTransformation.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkTranslationTransform.h"

namespace itk
{
  
/** \class TranslationRegistrationTransform
 * \brief Generic Translation Transformation for a registration method
 *
 * This Class define the generic interface for an Translation Transformation 
 *
 */

template <class TScalarType,unsigned int NDimensions, class TParameters>
class ITK_EXPORT  TranslationRegistrationTransform : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef TranslationRegistrationTransform  Self;


  /**
   * Integer constants
   */
  enum 
  { 
    SpaceDimension = NDimensions,
    ParametersDimension = NDimensions * (NDimensions + 1)
  };


  /**
   * Standard "Superclass" typedef.
   */
  typedef Transformation<TScalarType,NDimensions> Superclass;


  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;


  /** 
   * Type of the input parameters
   */
  typedef  TParameters     ParametersType;


  /** 
   * Translation Transform Type
   */
  typedef  TranslationTransform<TScalarType,NDimensions>    TranslationTransformType;


  /** 
   * Point Type
   */
  typedef  typename TranslationTransformType::PointType     PointType;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(TranslationRegistrationTransform, Transform);


  /** 
   * Run-time type information (and related methods).
   */
  typedef Matrix<TScalarType, SpaceDimension, 
                              ParametersDimension > JacobianType;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Transform a Point using the Translation transformation
   */
  PointType Transform( const PointType & point ) const;

  /**
   * Set the Transformation Parameters
   * and update the internal transformation
   */
  void SetParameters(const ParametersType &);


  /**
   * Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector.
   **/
  const JacobianType & GetJacobian(const PointType  &point ) const;

protected:

  TranslationRegistrationTransform();
  virtual ~TranslationRegistrationTransform() {};
  TranslationRegistrationTransform(const Self&);
  const Self & operator=(const Self&);


private:

  TranslationTransformType                 m_TranslationTransform;
  ParametersType                      m_Parameters;

  mutable JacobianType                m_Jacobian;     

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTranslationRegistrationTransform.txx"
#endif

#endif



