/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorRigid3DTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkVersorRigid3DTransform_h
#define __itkVersorRigid3DTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"

namespace itk
{

/** \brief VersorRigid3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the space
 *
 * \ingroup Transforms
 */
template < class TScalarType=double >    // Data type for scalars (float or double)
class ITK_EXPORT VersorRigid3DTransform : 
            public Rigid3DTransform< TScalarType > 
{
public:
  /** Standard class typedefs. */
  typedef VersorRigid3DTransform Self;
  typedef Rigid3DTransform< TScalarType >   Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( VersorRigid3DTransform, Rigid3DTransform );

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 9);

  /** Scalar type. */
  typedef typename Superclass::ScalarType  ScalarType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType  ParametersType;

  /** Matrix type. */
  typedef typename Superclass::MatrixType      MatrixType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType  JacobianType;

  /** VnlQuaternion type. */
  typedef typename Superclass::VnlQuaternionType  VnlQuaternionType;

  /** Versor type. */
  typedef typename Superclass::VersorType  VersorType;
  typedef typename VersorType::VectorType  AxisType;
  typedef typename VersorType::ValueType   AngleType;
  
  /** Offset type. */
  typedef typename Superclass::OffsetType  OffsetType;

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
  
  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 9 parameters. The first three represent the
   * versor, the following three represent the center of rotation
   * and the last three represent the offset. */
  void SetParameters( const ParametersType & parameters );
  virtual const ParametersType& GetParameters(void) const;

  /** Set the rotational part of the transform. */
  void SetRotation( const VersorType & versor );
  void SetRotation( const AxisType & axis, AngleType angle );

   /** Set and Get the center of rotation */
  void SetCenter( const InputPointType & center );
  itkGetConstReferenceMacro( Center, InputPointType );
 
  /** Set and Get the Translation to be applied after rotation */
  void SetTranslation( const OutputVectorType & translation );
  itkGetConstReferenceMacro( Translation, OutputVectorType );
  
  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the 
   * transform is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

protected:
  VersorRigid3DTransform();
  ~VersorRigid3DTransform(){};
  void PrintSelf(std::ostream &os, Indent indent) const;

  VersorRigid3DTransform(unsigned int outputSpaceDimension, unsigned int parametersDimension);

  /** Compute the components of the rotation matrix in the superclass. */
  void ComputeMatrixAndOffset(void);

private:
  VersorRigid3DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /**  Versor containing the rotation. */
  VersorType          m_Versor;

  /**  Center of rotation */
  InputPointType      m_Center;

  /**  Translation defined in addittion to the rotation around the center. */
  OutputVectorType    m_Translation;


}; //class VersorRigid3DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVersorRigid3DTransform.txx"
#endif

#endif /* __itkVersorRigid3DTransform_h */
