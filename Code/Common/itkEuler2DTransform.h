/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEuler2DTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkEuler2DTransform_h
#define __itkEuler2DTransform_h

#include <iostream>
#include "itkRigid2DTransform.h"

namespace itk
{

/** \brief Euler2DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the space
 * given one angle for rotation and a 2D offset for translation. 
 *
 * \ingroup Transforms
 */
template < class TScalarType=double >    // Data type for scalars (float or double)
class ITK_EXPORT Euler2DTransform : 
            public Rigid2DTransform< TScalarType > 
{
public:
  /** Standard class typedefs. */
  typedef Euler2DTransform Self;
  typedef Rigid2DTransform< TScalarType >   Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( Euler2DTransform, Rigid2DTransform );

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 2);
  itkStaticConstMacro(ParametersDimension, unsigned int, 3);

  /** Scalar type. */
  typedef typename Superclass::ScalarType  ScalarType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType  ParametersType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType  JacobianType;
  
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
   * There are 3 parameters. The first one represents the
   * angle of rotation in radians and the last two represents the offset. */
  void SetParameters( const ParametersType & parameters );

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are 3 parameters. The first one represents the
   * angle or rotation in radians and the last two represents the offset. */
  const ParametersType & GetParameters( void ) const;


  /** Set the rotational part of the transform. */
  void SetRotation(TScalarType angle);
  
  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the 
   * transform is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

  /** Set the transformation to an Identity
   * This sets the matrix to identity and the Offset to null. */
  virtual void SetIdentity( void );


protected:
  Euler2DTransform();
  ~Euler2DTransform(){};
  void PrintSelf(std::ostream &os, Indent indent) const;

  /** Compute the components of the rotation matrix in the superclass. */
  void ComputeMatrix(void);

private:
  Euler2DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  TScalarType m_Angle; 

}; //class Euler2DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEuler2DTransform.txx"
#endif

#endif /* __itkEuler2DTransform_h */
