/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuaternionRigidTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuaternionRigidTransform_h
#define __itkQuaternionRigidTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"
#include "vnl/vnl_quaternion.h"

namespace itk
{

/** \brief QuaternionRigidTransform of a vector space (e.g. space coordinates).
 *
 * This transform applies a rotation and translation to the space
 *
 * \ingroup Transforms
 */
template < class TScalarType=double >    // Data type for scalars (float or double)
class ITK_EXPORT QuaternionRigidTransform :
        public Rigid3DTransform< TScalarType > 
{
public:
  /** Standard class typedefs.   */
  typedef QuaternionRigidTransform Self;
  typedef Rigid3DTransform< TScalarType >     Superclass;

  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );

  /** Run-time type information (and related methods).   */
  itkTypeMacro( QuaternionRigidTransform, Rigid3DTransform );

  /** Scalar type.   */
  typedef typename Superclass::ScalarType  ScalarType;

  /** Vector type. */
  typedef typename Superclass::InputVectorType    InputVectorType;
  typedef typename Superclass::OutputVectorType   OutputVectorType;
  typedef typename Superclass::InputVnlVectorType   InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType   OutputVnlVectorType;
  typedef typename Superclass::InputCovariantVectorType InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType OutputCovariantVectorType;

  /** Parameters type.   */
  typedef typename Superclass::ParametersType  ParametersType;
  
  /** Jacobian type.   */
  typedef typename Superclass::JacobianType  JacobianType;

  /** VnlQuaternion type.  */
  typedef vnl_quaternion<TScalarType>           VnlQuaternionType;

  /** Dimension of parameters   */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 7);

  /** Dimension of the domain space. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int,
                      Superclass::InputSpaceDimension);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int,
                      Superclass::OutputSpaceDimension);

  /** Standard matrix type for this class. */
  typedef typename Superclass::MatrixType MatrixType;

  /** Standard vector type for this class. */
  typedef Vector<TScalarType, itkGetStaticConstMacro(InputSpaceDimension)> OffsetType;

  /** Standard coordinate point type for this class. */
  typedef typename Superclass::InputPointType    InputPointType;
  typedef typename Superclass::OutputPointType   OutputPointType;
  

  /** Get the rotation from an QuaternionRigidTransform.
   * This method returns the value of the rotation of the
   * QuaternionRigidTransform.   **/
  const VnlQuaternionType & GetRotation(void) const
    { return m_Rotation; }


  /** Set the parameters to the IdentityTransform */
  virtual void SetIdentity(void);

  /** Compute the Jacobian Matrix of the transformation at one point */
  /** Set the rotation of the rigid transform.
   * This method sets the rotation of a QuaternionRigidTransform to a
   * value specified by the user. */
  void SetRotation(const VnlQuaternionType &rotation);


  /** Set and Get the center of rotation */
  void SetCenter( const InputPointType & center );
  itkGetConstReferenceMacro( Center, InputPointType );

  /** Set and Get the Translation to be applied after rotation */
  void SetTranslation( const OutputVectorType & translation );
  itkGetConstReferenceMacro( Translation, OutputVectorType );
  
  /** Compute the offset using the rotation center, the matrix
   *  and the final translation. This method MUST be called before
   *  using the transform for any mapping. */
  virtual void ComputeOffset(void);

  /** Set the transformation from a container of parameters.
   * This is typically used by optimizers.
   * There are 7 parameters. The first four represents the
   * quaternion and the last three represents the
   * offset. */
  void SetParameters( const ParametersType & parameters );
  virtual const ParametersType & GetParameters() const;

  /** Compute the Jacobian of the transformation.
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

protected:
  QuaternionRigidTransform();
  ~QuaternionRigidTransform(){};
  void PrintSelf(std::ostream &os, Indent indent) const;

private:
  QuaternionRigidTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Rotation of the transformation. */
  VnlQuaternionType   m_Rotation;

  /** Center of rotation */
  InputPointType      m_Center;

  /** Translation */
  OutputVectorType    m_Translation;


}; //class QuaternionRigidTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuaternionRigidTransform.txx"
#endif

#endif /* __itkQuaternionRigidTransform_h */
