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
 * \ingroup Transforms
 */
template < class TScalarType=double >  // Data type for scalars:float or double
class ITK_EXPORT ScaleSkewVersor3DTransform : 
            public VersorRigid3DTransform< TScalarType > 
{
public:
  /** Standard class typedefs. */
  typedef ScaleSkewVersor3DTransform Self;
  typedef VersorRigid3DTransform< TScalarType >   Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
      
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( ScaleSkewVersor3DTransform, VersorRigid3DTransform );

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 15);

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

  /** Scale & Skew Vector Type. */
  typedef Vector<TScalarType, itkGetStaticConstMacro(SpaceDimension)> 
          ScaleVectorType;
  typedef Vector<TScalarType, 6 >                                           
          SkewVectorType;

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
  typedef typename Superclass::InputCovariantVectorType   
          InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType  
          OutputCovariantVectorType;
  
  /** VnlVector type. */
  typedef typename Superclass::InputVnlVectorType   InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType  OutputVnlVectorType;
  
  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 15 parameters:
   *   0-2   versor
   *   3-5   translation
   *   6-8   Scale
   *   9-14  Skew
   **  */
  void SetParameters( const ParametersType & parameters );
  virtual const ParametersType& GetParameters(void) const;

  virtual const MatrixType & GetMatrix(void) const;

  void SetScale( const ScaleVectorType & scale );
  itkGetConstReferenceMacro( Scale, ScaleVectorType );

  void SetSkew( const SkewVectorType & skew );
  itkGetConstReferenceMacro( Skew, SkewVectorType );

  void SetIdentity();

protected:
  ScaleSkewVersor3DTransform();
  ~ScaleSkewVersor3DTransform(){};
  void PrintSelf(std::ostream &os, Indent indent) const;

  ScaleSkewVersor3DTransform(unsigned int outputSpaceDimension,
                             unsigned int parametersDimension);

  /** Compute the components of the rotation matrix in the superclass. */
  void ComputeMatrixAndOffset(void);

private:
  ScaleSkewVersor3DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /**  Vector containing the scale. */
  ScaleVectorType          m_Scale;

  /**  Vector containing the skew */
  SkewVectorType      m_Skew;

}; //class ScaleSkewVersor3DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScaleSkewVersor3DTransform.txx"
#endif

#endif /* __ScaleSkewVersor3DTransform_h */
