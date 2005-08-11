/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarity2DTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSimilarity2DTransform_h
#define __itkSimilarity2DTransform_h

#include <iostream>
#include "itkRigid2DTransform.h"

namespace itk
{

/** \brief Similarity2DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a homogenous scale and rigid transform in
 * 2D space. The transform is specified as a scale and rotation around
 * a arbitrary center and is followed by a translation.
 * given one angle for rotation, a homogeneous scale and a 2D offset for translation. 
 *
 * The parameters for this transform can be set either using
 * individual Set methods or in serialized form using
 * SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 3 elements
 * ordered as follows:
 * p[0] = scale
 * p[1] = angle
 * p[2] = x component of the translation
 * p[3] = y component of the translation
 *
 * The serialization of the fixed parameters is an array of 2 elements
 * ordered as follows:
 * p[0] = x coordinate of the center
 * p[1] = y coordinate of the center
 *
 * Access methods for the center, translation and underlying matrix
 * offset vectors are documented in the superclass MatrixOffsetTransformBase.
 *
 * Access methods for the angle are documented in superclass Rigid2DTransform.
 *
 * \sa Transform
 * \sa MatrixOffsetTransformBase
 * \sa Rigid2DTransform
 *
 * \ingroup Transforms
 */
template < class TScalarType=double >    // Data type for scalars (float or double)
class ITK_EXPORT Similarity2DTransform : 
            public Rigid2DTransform< TScalarType > 
{
public:
  /** Standard class typedefs. */
  typedef Similarity2DTransform Self;
  typedef Rigid2DTransform< TScalarType >   Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( Similarity2DTransform, Rigid2DTransform );

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension,           unsigned int, 2);
  itkStaticConstMacro(InputSpaceDimension,      unsigned int, 2);
  itkStaticConstMacro(OutputSpaceDimension,     unsigned int, 2);
  itkStaticConstMacro(ParametersDimension,      unsigned int, 4);

  /** Scalar type. */
  typedef typename Superclass::ScalarType  ScalarType;
  typedef          TScalarType             ScaleType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType  ParametersType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType  JacobianType;
  
  /** Offset type. */
  typedef typename Superclass::OffsetType  OffsetType;

  /** Matrix type. */
  typedef typename Superclass::MatrixType MatrixType;

  /** Point type. */
  typedef typename Superclass::InputPointType   InputPointType;
  typedef typename Superclass::OutputPointType  OutputPointType;
  
  /** Vector type. */
  typedef typename Superclass::InputVectorType   InputVectorType;
  typedef typename Superclass::OutputVectorType  OutputVectorType;
  
  /** CovariantVector type. */
  typedef typename Superclass::InputCovariantVectorType   InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType  OutputCovariantVectorType;
  
  /** VnlVector type. */
  typedef typename Superclass::InputVnlVectorType   InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType  OutputVnlVectorType;

  /** Set the Scale part of the transform. */
  void SetScale( ScaleType scale );
  itkGetConstReferenceMacro( Scale, ScaleType );
  
  /** Set the transformation from a container of parameters
    * This is typically used by optimizers.
    * There are 4 parameters. The first one represents the
    * scale, the second represents the angle of rotation
    * and the last two represent the translation.
    * The center of rotation is fixed.
    * 
    * \sa Transform::SetParameters()
    * \sa Transform::SetFixedParameters() */
  void SetParameters( const ParametersType & parameters );

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are 4 parameters. The first one represents the
   * scale, the second represents the angle of rotation,
   * and the last two represent the translation.
   * The center of rotation is fixed.
   *
   * \sa Transform::GetParameters()
   * \sa Transform::GetFixedParameters() */
  const ParametersType & GetParameters( void ) const; 
 
   /** This method computes the Jacobian matrix of the transformation
   * at a given input point.
   *
   * \sa Transform::GetJacobian() */
 const JacobianType & GetJacobian(const InputPointType  &point ) const;

  /** Set the transformation to an identity. */
  virtual void SetIdentity( void );

  /**
   * This method creates and returns a new Similarity2DTransform object
   * which is the inverse of self.
   **/
  void CloneInverseTo( Pointer & newinverse ) const;

  /**
   * This method creates and returns a new Similarity2DTransform object
   * which has the same parameters.
   **/
  void CloneTo( Pointer & clone ) const;

  /**
   * Set the rotation Matrix of a Similarity 2D Transform
   *
   * This method sets the 2x2 matrix representing a similarity
   * transform.  The Matrix is expected to be a valid
   * similarity transform with a certain tolerance.
   *
   * \warning This method will throw an exception if the matrix
   * provided as argument is not valid.
   *
   * \sa MatrixOffsetTransformBase::SetMatrix()
   *
   **/
  virtual void SetMatrix( const MatrixType & matrix );

protected:
  Similarity2DTransform();
  Similarity2DTransform( unsigned int spaceDimension, 
                         unsigned int parametersDimension);

  ~Similarity2DTransform(){};
  void PrintSelf(std::ostream &os, Indent indent) const;

  /** Compute matrix from angle and scale. This is used in Set methods
   * to update the underlying matrix whenever a transform parameter 
   * is changed. */
  virtual void ComputeMatrix(void);

  /** Compute the angle and scale from the matrix. This is used to compute
   * transform parameters from a given matrix. This is used in
   * MatrixOffsetTransformBase::Compose() and 
   * MatrixOffsetTransformBase::GetInverse(). */
  virtual void ComputeMatrixParameters(void);

  /** Set the scale without updating underlying variables. */
  void SetVarScale( ScaleType scale )
    { m_Scale = scale; }

private:
  Similarity2DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  ScaleType     m_Scale; 

}; //class Similarity2DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimilarity2DTransform.txx"
#endif

#endif /* __itkSimilarity2DTransform_h */
