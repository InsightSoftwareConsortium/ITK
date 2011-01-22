/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkDeformationFieldTransform.h,v $
  Language:  C++
  Date:      $Date: $
  Version:   $Revision: $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDeformationFieldTransform_h
#define __itkDeformationFieldTransform_h

#include "itkTransform.h"

#include "itkImage.h"
#include "itkVectorInterpolateImageFunction.h"

namespace itk
{

/** \class DeformationFieldTransform
 * \brief TODO
 *
 * \ingroup Transforms
 *
 */
template
  <class TScalar, unsigned int NDimensions>
class ITK_EXPORT DeformationFieldTransform :
  public Transform<TScalar, NDimensions, NDimensions>
{
public:
  /** Standard class typedefs. */
  typedef DeformationFieldTransform                         Self;
  typedef Transform<TScalar, NDimensions, NDimensions>      Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( DeformationFieldTransform, Transform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** InverseTransform type. */
  typedef typename Superclass::InverseTransformBasePointer  InverseTransformBasePointer;

  /** Scalar type. */
  typedef typename Superclass::ScalarType  ScalarType;

  /** Type of the input parameters. */
  typedef  typename Superclass::ParametersType      ParametersType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType  JacobianType;

  /** Standard coordinate point type for this class. */
  typedef typename Superclass::InputPointType   InputPointType;
  typedef typename Superclass::OutputPointType  OutputPointType;

  /** Standard vector type for this class. */
  typedef typename Superclass::InputVectorType      InputVectorType;
  typedef typename Superclass::OutputVectorType     OutputVectorType;

  /** Standard covariant vector type for this class */
  typedef typename Superclass::InputCovariantVectorType
    InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType
    OutputCovariantVectorType;

  /** Standard vnl_vector type for this class. */
  typedef typename Superclass::InputVnlVectorType   InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType  OutputVnlVectorType;

  /** Dimension of the domain spaces. */
  itkStaticConstMacro( Dimension, unsigned int, NDimensions );

  /** Define the deformation field type and corresponding interpolator type. */
  typedef Image<OutputVectorType,
    itkGetStaticConstMacro( Dimension )> DeformationFieldType;
  typedef VectorInterpolateImageFunction
    <DeformationFieldType, ScalarType> InterpolatorType;

  /** Get/Set the deformation field. */
  itkGetObjectMacro( DeformationField, DeformationFieldType );
  itkSetObjectMacro( DeformationField, DeformationFieldType );

  /** Get/Set the inverse deformation field. */
  itkGetObjectMacro( InverseDeformationField, DeformationFieldType );
  itkSetObjectMacro( InverseDeformationField, DeformationFieldType );

  /** Get/Set the interpolator. */
  itkGetObjectMacro( Interpolator, InterpolatorType );
  itkSetObjectMacro( Interpolator, InterpolatorType );

  /**  Method to transform a point. */
  virtual OutputPointType TransformPoint( const InputPointType& thisPoint ) const;

  /**  Method to transform a vector. */
  virtual OutputVectorType TransformVector(const InputVectorType &) const
    { itkExceptionMacro( "TransformVector unimplemented" ); }

  /**  Method to transform a vnl_vector. */
  virtual OutputVnlVectorType TransformVector(const InputVnlVectorType &) const
    { itkExceptionMacro( "TransformVector unimplemented" ); }

  /**  Method to transform a CovariantVector. */
  virtual OutputCovariantVectorType TransformCovariantVector(const InputCovariantVectorType &) const
    { itkExceptionMacro( "TransformCovariantVector unimplemented" ); }

  /** Set the transformation parameters and update internal transformation.
   * NOTE if this is implemented eventually, refer first to itkTransform.h
   * for notes on pass by value vs reference.
   */
  virtual void SetParameters(const ParametersType &)
    { itkExceptionMacro("SetParameters unimplemented."); }

  /** Get the Transformation Parameters. */
  virtual const ParametersType & GetParameters(void) const
    { itkExceptionMacro("GetParameters unimplemented."); }

  /** Set the fixed parameters and update internal transformation. */
  virtual void SetFixedParameters(const ParametersType &)
    { itkExceptionMacro("SetFixedParameters unimplemented."); }

  /** Get the Fixed Parameters. */
  virtual const ParametersType & GetFixedParameters(void) const
    { itkExceptionMacro("GetFixedParameters unimplemented."); }

  /**
   * Compute the jacobian with respect to the parameters.  Since there are
   * no parameters for this transform, the Jacobian shouldn't be requested.
   * Definition in txx file throws an exception.
   */
  virtual JacobianType & GetJacobian( const InputPointType & ) const;

  /** Return an inverse of this transform. */
  bool GetInverse( Self *inverse ) const;

  /** Return an inverse of this transform. */
  virtual InverseTransformBasePointer GetInverseTransform() const;

  /** This transform is not linear. */
  virtual bool IsLinear() const { return false; }


protected:
  DeformationFieldTransform();
  virtual ~DeformationFieldTransform();
  void PrintSelf( std::ostream& os, Indent indent ) const;

  /** The deformation field and its inverse (if it exists). */
  typename DeformationFieldType::Pointer      m_DeformationField;
  typename DeformationFieldType::Pointer      m_InverseDeformationField;

  /** The interpolator. */
  typename InterpolatorType::Pointer          m_Interpolator;

  /** State var for tracking object update time */
  mutable unsigned long                               m_PreviousDeformationFieldMTime;
  mutable unsigned long                               m_PreviousInterpolatorMTime;

private:
  DeformationFieldTransform( const Self& ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

};

} // end namespace itk

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkDeformationFieldTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkDeformationFieldTransform.txx"
#endif

#endif // __itkDeformationFieldTransform_h
