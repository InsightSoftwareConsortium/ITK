/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkVersorTransform_h
#define __itkVersorTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"
#include "vnl/vnl_quaternion.h"
#include "itkVersor.h"

namespace itk
{

/**
 *
 * VersorTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation to the space.
 *
 * \ingroup Transforms
 *
 **/
template < class TScalarType=double >//Data type for scalars (float or double)
class ITK_EXPORT VersorTransform : public Rigid3DTransform< TScalarType > 
{
public:

  /** Standard Self Typedef */
  typedef VersorTransform                   Self;
  typedef Rigid3DTransform< TScalarType >   Superclass;
  typedef SmartPointer<Self>                Pointer;
  typedef SmartPointer<const Self>          ConstPointer;

  /** Run-time type information (and related methods).  */
  itkTypeMacro( VersorTransform, Rigid3DTransform );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Dimension of parameters */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 3);

  /** Parameters Type   */
  typedef typename Superclass::ParametersType         ParametersType;
  typedef typename Superclass::JacobianType           JacobianType;
  typedef typename Superclass::ScalarType             ScalarType;
  typedef typename Superclass::InputPointType         InputPointType;
  typedef typename Superclass::OutputPointType        OutputPointType;
  typedef typename Superclass::InputVectorType        InputVectorType;
  typedef typename Superclass::OutputVectorType       OutputVectorType;
  typedef typename Superclass::InputVnlVectorType     InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType    OutputVnlVectorType;
  typedef typename Superclass::InputCovariantVectorType 
                                                      InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType
                                                      OutputCovariantVectorType;
  typedef typename Superclass::MatrixType             MatrixType;
  typedef typename Superclass::InverseMatrixType      InverseMatrixType;
  typedef typename Superclass::CenterType             CenterType;
  typedef typename Superclass::OffsetType             OffsetType;

  /** VnlQuaternion Type */
  typedef vnl_quaternion<TScalarType>                 VnlQuaternionType;

  /** Versor Type */
  typedef Versor<TScalarType>                   VersorType;
  typedef typename VersorType::VectorType       AxisType;
  typedef typename VersorType::ValueType        AngleType;

  /**
   * Set the transformation from a container of parameters
   * This is typically used by optimizers.
   *
   * There are 3 parameters. They represent the components
   * of the right part of the versor. This can be seen
   * as the components of the vector parallel to the rotation
   * axis and multiplied by vcl_sin( angle / 2 ). */
  void SetParameters( const ParametersType & parameters );

  /** Get the Transformation Parameters. */
  const ParametersType& GetParameters(void) const;

  /** Set the rotational part of the transform */
  void SetRotation( const VersorType & versor );
  void SetRotation( const AxisType & axis, AngleType angle );
  itkGetConstReferenceMacro(Versor, VersorType);

  /** Set the parameters to the IdentityTransform */
  virtual void SetIdentity(void);

  /** Compute the Jacobian of the transformation
   *  This method computes the Jacobian matrix of the transformation.
   *  given point or vector, returning the transformed point or
   *  vector. The rank of the Jacobian will also indicate if the 
   *  transform is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

protected:

  /** Construct an VersorTransform object */
  VersorTransform(const MatrixType &matrix,
                  const OutputVectorType &offset);
  VersorTransform(unsigned int outputDims,
                  unsigned int paramDims);
  VersorTransform();

  /** Destroy an VersorTransform object */
  ~VersorTransform(){};

  /** This method must be made protected here because it is not a safe way of
   * initializing the Versor */
  virtual void SetRotationMatrix(const MatrixType & matrix)
    { this->Superclass::SetRotationMatrix( matrix ); }

  void SetVarVersor(const VersorType & newVersor)
    { m_Versor = newVersor; }

  /** Print contents of a VersorTransform */
  void PrintSelf(std::ostream &os, Indent indent) const;

  /** Compute Matrix
   *  Compute the components of the rotation matrix in the superclass */
  void ComputeMatrix(void);
  void ComputeMatrixParameters(void);

private:
  /** Copy a VersorTransform object */
  VersorTransform(const Self & other); // Not implemented

  /** Assignment operator */
  const Self & operator=( const Self & ); // Not implemented

  /** Versor containing the rotation */
  VersorType    m_Versor;

}; //class VersorTransform


}  // namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_VersorTransform(_, EXPORT, x, y) namespace itk { \
  _(1(class EXPORT VersorTransform< ITK_TEMPLATE_1 x >)) \
  namespace Templates { typedef VersorTransform< ITK_TEMPLATE_1 x > VersorTransform##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkVersorTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkVersorTransform.txx"
#endif

#endif /* __itkVersorTransform_h */
