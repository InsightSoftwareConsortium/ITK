/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DPerspectiveRegistrationTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkRigid3DPerspectiveRegistrationTransform_h
#define __itkRigid3DPerspectiveRegistrationTransform_h

#include "itkObject.h"
#include "itkTransform.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkMatrix.h"

namespace itk
{
  
/** \class Rigid3DPerspectiveRegistrationTransform
 * \brief Generic Rigid Transformation for a registration method
 *
 * This Class define the generic interface for an Rigid Transformation .
 * It uses an instance of an Rigid3D transform, and only the Rigid aspects
 * of the transform are used. This is reflected by the fact that the number
 * of parameters is reduced.
 *
 */

template <class TScalarType, class TParameters>
class ITK_EXPORT  Rigid3DPerspectiveRegistrationTransform : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Rigid3DPerspectiveRegistrationTransform  Self;


  /**
   * Integer constants
   */
  enum 
  { 
    SpaceDimension = 3,
    ParametersDimension = 7
  };


  /**
   * Standard "Superclass" typedef.
   */
  typedef Object Superclass;


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
   * Rigid Transform Type
   */
  typedef  Rigid3DPerspectiveTransform<TScalarType>    TransformType;


  /** 
   * Input Point Type
   */
  typedef  typename TransformType::InputPointType     InputPointType;

  
  /** 
   * Output Point Type
   */
  typedef  typename TransformType::OutputPointType     OutputPointType;


  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Rigid3DPerspectiveRegistrationTransform, Object);


  /** 
   * Type for the Jacobian matrix
   */
  typedef Matrix<TScalarType, SpaceDimension, 
                              ParametersDimension > JacobianType;


  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  

  /**
   * Transform a Point using the Rigid transformation
   */
  OutputPointType Transform( const InputPointType & point ) const;

  /**
   * Set the Transformation Parameters
   * and update the internal transformation
   */
  void SetParameters(const ParametersType &);

  /**
   *  Set the Scale for translations
   */
  itkSetMacro( TranslationScale , TScalarType );
   

  /**
   *  Get the Scale for translations
   */
  itkGetMacro( TranslationScale , TScalarType );
  

  /**
   *  Get the transform
   */
  itkGetMacro( Transform , TransformType );
 

  /**
   * Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector.
   **/
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

protected:

  Rigid3DPerspectiveRegistrationTransform();
  virtual ~Rigid3DPerspectiveRegistrationTransform() {};
  Rigid3DPerspectiveRegistrationTransform(const Self&);
  const Self & operator=(const Self&);


private:

  /**
   *  Internal transformation
   */
  TransformType                       m_Transform;
  
  /**
   *  List of parameters that unambiguosly define the transformation
   */  
  ParametersType                      m_Parameters;

  /**
   *  Scale of the translations. It is used to bring translations
   *  and rotations to a similar scale. It should be set to the 
   *  value of the maximum expected translation.
   */  
  TScalarType                         m_TranslationScale;

  /**
   * Jacobian matrix of the transformation. It is used to compute
   * derivatives by using the chain rule.
   */
  mutable JacobianType                m_Jacobian; 

  /**
   * Cached results for calculating the Jacobian
   */
  double                              m_AxisMagnitude;
  double                              m_CosHalfAngle;
  double                              m_SinHalfAngle;
  
  typedef Matrix<double,4,4> MatrixType;
  MatrixType                          m_dQ_dParameters;    

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRigid3DPerspectiveRegistrationTransform.txx"
#endif

#endif



