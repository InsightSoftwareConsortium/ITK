/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuaternionRigidRegistrationTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkQuaternionRigidRegistrationTransform_h
#define __itkQuaternionRigidRegistrationTransform_h

#include "itkObject.h"
#include "itkTransform.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkRigid3DTransform.h"

namespace itk
{
  
/** \class QuaternionRigidRegistrationTransform
 * \brief Quaternion based rigid transform for registration.
 *
 * This class rigidly transform points in 3D vector space. The rigid
 * transform is specified by 7 parameters. The first 4 parameters
 * defines the components of a quaternion and the last 3 parameters
 * the translation in each dimension.
 *
 * To maintain a strictly rigid transform, the first 4 parameters are
 * normalized to have a magnitude of one. The normalized parameters are
 * then used to create the an internal quaternion to represent rotation
 * in 3D vector space.
 */

template <class TScalarType, class TParameters>
class ITK_EXPORT  QuaternionRigidRegistrationTransform : public Object 
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef QuaternionRigidRegistrationTransform  Self;

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
  typedef Transform<TScalarType,SpaceDimension>  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Input parameter type
   */
  typedef TParameters     ParametersType;

  /** 
   * Underlying rigid transform type
   */
  typedef Rigid3DTransform<TScalarType,SpaceDimension>  RigidTransformType;

  /** 
   * Input point type
   */
  typedef typename RigidTransformType::InputPointType  InputPointType;
  typedef typename RigidTransformType::OutputPointType OutputPointType;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(QuaternionRigidRegistrationTransform, Transform);

  /** 
   * Jacobian matrix type.
   */
  typedef Matrix<TScalarType, SpaceDimension, 
                              ParametersDimension > JacobianType;
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Rigidly transform a Point using pre-specified parameters.
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
   * Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation
   * for a given point.
   **/
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

protected:

  QuaternionRigidRegistrationTransform();
  virtual ~QuaternionRigidRegistrationTransform() {};
  QuaternionRigidRegistrationTransform(const Self&);
  const Self & operator=(const Self&);


private:

  /**
   *  Internal transformation
   */
  RigidTransformType                 m_RigidTransform;
  
  /**
   *  Parameters which defines the rigid transformation
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
  double                              m_Magnitude;


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuaternionRigidRegistrationTransform.txx"
#endif

#endif



