/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredAffineRegistrationTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkCenteredAffineRegistrationTransform_h
#define __itkCenteredAffineRegistrationTransform_h

#include "itkObject.h"
#include "itkTransformation.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkAffineTransform.h"

namespace itk
{

/** \class CenteredAffineRegistrationTransform
 * \brief Affine Transformation used for registration.
 *
 * CenteredAffineRegistrationTransform maps a point
 * from one space to another for a given set of affine
 * parameters.
 *
 * The affine parameters is specifed as an itkPoint of size
 * ImageDimension * (ImageDimension + 1 ). This Point is formed
 * by concatenating each row of the matrix (linear) component with
 * the offset (translation) component.
 *
 * In addition the user can specify the transformation center
 * for both the domain and range spaces. For example, setting the
 * centers to the center of mass or image stack center can
 * greatly improve the registration optimization process.
 *
 * \sa AffineRegistrationTransform
 *
 */
template <class TScalarType,unsigned int NDimensions, class TParameters>
class ITK_EXPORT  CenteredAffineRegistrationTransform : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef CenteredAffineRegistrationTransform  Self;

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
   * Affine Transform Type
   */
  typedef  AffineTransform<TScalarType,NDimensions>    AffineTransformType;

  /**
   * Point Type
   */
  typedef typename AffineTransformType::PointType     PointType;

  /**
   * Run-time type information (and related methods).
   */
  itkTypeMacro(CenteredAffineRegistrationTransform, Transform);

  /**
   * Type of the Jacobian matrix
   */
  typedef Matrix<TScalarType, SpaceDimension,
                              ParametersDimension > JacobianType;
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Transform a Point using the Affine transformation
   */
  PointType Transform( const PointType & point ) const;

  /**
   * Set the domain transformation center
   */
  void SetDomainTransformationCenter( const PointType & center )
    {  m_DomainTransformationCenter = center; }

  /**
   * Get the domain transformation center
   */
  const PointType& GetDomainTransformationCenter( void )
    { return m_DomainTransformationCenter; }

  /**
   * Set the range transformation center
   */
  void SetRangeTransformationCenter( const PointType & center )
    {  m_RangeTransformationCenter = center; }

  /**
   * Get the range transformation center
   */
  const PointType& GetRangeTransformationCenter( void )
    { return m_RangeTransformationCenter; }

  /**
   * Set the Transformation Parameters
   * and update the internal transformation
   */
  void SetParameters(const ParametersType &);

  /**
   * Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation
   * at a given point.
   **/
  const JacobianType & GetJacobian(const PointType  &point ) const;

protected:

  CenteredAffineRegistrationTransform();
  virtual ~CenteredAffineRegistrationTransform() {};
  CenteredAffineRegistrationTransform(const Self&);
  const Self & operator=(const Self&);

private:

  AffineTransformType                 m_AffineTransform;
  ParametersType                      m_Parameters;

  mutable JacobianType                m_Jacobian;

  PointType                           m_DomainTransformationCenter;
  PointType                           m_RangeTransformationCenter;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCenteredAffineRegistrationTransform.txx"
#endif

#endif



