/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScaleSkewVersor3DTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkScaleSkewVersor3DTransform_h
#define __itkScaleSkewVersor3DTransform_h

#include <iostream>
#include "itkVersorRigid3DTransform.h"

namespace itk
{

/** \brief ScaleSkewVersor3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a versor rotation and translation & scale/skew
 * to the space
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 15 elements.
 * The first 3 elements are the components of the versor representation
 * of 3D rotation. The next 3 parameters defines the translation in each
 * dimension. The next 3 parameters defines scaling in each dimension.
 * The last 6 parameters defines the skew.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation.
 *
 *
 * \ingroup Transforms
 */
template < class TScalarType=double >  // Data type for scalars:float or double
class ITK_EXPORT ScaleSkewVersor3DTransform : 
            public VersorRigid3DTransform< TScalarType > 
{
public:
  /** Standard class typedefs. */
  typedef ScaleSkewVersor3DTransform              Self;
  typedef VersorRigid3DTransform< TScalarType >   Superclass;
  typedef SmartPointer<Self>                      Pointer;
  typedef SmartPointer<const Self>                ConstPointer;
      
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( ScaleSkewVersor3DTransform, VersorRigid3DTransform );

  /** Dimension of parameters. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 15);

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
  typedef typename Superclass::TranslationType        TranslationType;

  typedef typename Superclass::VersorType             VersorType;
  typedef typename Superclass::AxisType               AxisType;
  typedef typename Superclass::AngleType              AngleType;

  /** Scale & Skew Vector Type. */
  typedef Vector<TScalarType, 3> 
                                                      ScaleVectorType;
  typedef Vector<TScalarType, 6 >                     SkewVectorType;

 /** Directly set the matrix of the transform.
  *
  * \sa MatrixOffsetTransformBase::SetMatrix() */
  virtual void SetMatrix(const MatrixType &matrix);

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 15 parameters:
   *   0-2   versor
   *   3-5   translation
   *   6-8   Scale
   *   9-14  Skew
   **  */
  virtual void SetParameters( const ParametersType & parameters );
  virtual const ParametersType& GetParameters(void) const;

  void SetScale( const ScaleVectorType & scale );
  itkGetConstReferenceMacro( Scale, ScaleVectorType );

  void SetSkew( const SkewVectorType & skew );
  itkGetConstReferenceMacro( Skew, SkewVectorType );

  void SetIdentity();

  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the 
   * transform is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

protected:
  ScaleSkewVersor3DTransform();
  ScaleSkewVersor3DTransform(const MatrixType &matrix,
                             const OutputVectorType &offset);
  ScaleSkewVersor3DTransform(unsigned int outputDims,
                             unsigned int paramDims);
  ~ScaleSkewVersor3DTransform(){};

  void PrintSelf(std::ostream &os, Indent indent) const;

  void SetVarScale(const ScaleVectorType & scale)
    { m_Scale = scale; };

  void SetVarSkew(const SkewVectorType & skew)
    { m_Skew = skew; };

  /** Compute the components of the rotation matrix in the superclass. */
  void ComputeMatrix(void);
  void ComputeMatrixParameters(void);

private:
  ScaleSkewVersor3DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /**  Vector containing the scale. */
  ScaleVectorType          m_Scale;

  /**  Vector containing the skew */
  SkewVectorType           m_Skew;

}; //class ScaleSkewVersor3DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScaleSkewVersor3DTransform.txx"
#endif

#endif /* __ScaleSkewVersor3DTransform_h */
