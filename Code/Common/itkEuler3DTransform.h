/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEuler3DTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkEuler3DTransform_h
#define __itkEuler3DTransform_h

#include <iostream>
#include "itkRigid3DTransform.h"

namespace itk
{

/** \brief Euler3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the space given 3 euler
 * angles and a 3D offset.
 *
 * \ingroup Transforms
 */
template < class TScalarType=double >    // Data type for scalars (float or double)
class ITK_EXPORT Euler3DTransform : 
            public Rigid3DTransform< TScalarType > 
{
public:
  /** Standard class typedefs. */
  typedef Euler3DTransform Self;
  typedef Rigid3DTransform< TScalarType >   Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( Euler3DTransform, Rigid3DTransform );

  /** Dimension of parameters. */
  itkStaticConstMacro( SpaceDimension, unsigned int, 3 ); 
  itkStaticConstMacro( ParametersDimension, unsigned int, 6 ); 

  /** Scalar type. */
  typedef typename Superclass::ScalarType  ScalarType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType  ParametersType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType  JacobianType;

  typedef typename Superclass::ScalarType   AngleType;
  
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
  typedef typename Superclass::MatrixType           MatrixType;

  /** Set/Get the transformation from a container of parameters
   * This is typically used by optimizers.  There are 6 parameters. The first
   * three represent the angles to rotate around the coordinate axis, and the
   * last three represents the offset. */
  void SetParameters( const ParametersType & parameters );
  const ParametersType& GetParameters(void) const;

  /** Set the rotational part of the transform. */
  void SetRotation(ScalarType angleX,ScalarType angleY,ScalarType angleZ);
  itkGetMacro(AngleX, ScalarType);
  itkGetMacro(AngleY, ScalarType);
  itkGetMacro(AngleZ, ScalarType);

  void SetCenter( const InputPointType & center );
  itkGetConstReferenceMacro( Center, InputPointType );

  void SetTranslation( const OutputVectorType & translation );
  itkGetConstReferenceMacro( Translation, OutputVectorType );
  
  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the 
   * transform is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

  void ComputeZYX(bool compute) {m_ComputeZYX = compute;}

  virtual void SetRotationMatrix(const MatrixType &matrix);

  virtual void Compose(const Superclass *other, bool pre=false);
  
  virtual void SetIdentity(void);


protected:
  Euler3DTransform();
  Euler3DTransform(unsigned int OutputSpaceDimension,
                   unsigned int ParametersDimension);
  ~Euler3DTransform(){};
  void PrintSelf(std::ostream &os, Indent indent) const;

  /** Compute the components of the rotation matrix in the superclass. */
  void ComputeMatrixAndOffset(void);

  void ComputeMatrix(void);
  void ComputeAnglesFromMatrix(void);

private:
  Euler3DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  ScalarType  m_AngleX; 
  ScalarType  m_AngleY; 
  ScalarType  m_AngleZ;
  bool        m_ComputeZYX;

  InputPointType m_Center;
  OutputVectorType m_Translation;

}; //class Euler3DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEuler3DTransform.txx"
#endif

#endif /* __itkEuler3DTransform_h */
