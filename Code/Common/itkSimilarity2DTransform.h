/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarity2DTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSimilarity2DTransform_h
#define __itkSimilarity2DTransform_h

#include <iostream>
#include "itkCenteredRigid2DTransform.h"

namespace itk
{

/** \brief Similarity2DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation, scale and translation to the space
 * given one angle for rotation, a homogeneous scale and a 2D offset for translation. 
 *
 * \ingroup Transforms
 */
template < class TScalarType=double >    // Data type for scalars (float or double)
class ITK_EXPORT Similarity2DTransform : 
            public CenteredRigid2DTransform< TScalarType > 
{
public:
  /** Standard class typedefs. */
  typedef Similarity2DTransform Self;
  typedef CenteredRigid2DTransform< TScalarType >   Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( Similarity2DTransform, CenteredRigid2DTransform );

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension,           unsigned int, 2);
  itkStaticConstMacro(InputSpaceDimension,      unsigned int, 2);
  itkStaticConstMacro(OutputSpaceDimension,     unsigned int, 2);
  itkStaticConstMacro(ParametersDimension,      unsigned int, 6);

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
   * rotation and the last two represents the offset. */
  void SetParameters( const ParametersType & parameters );

  /** Set the Scale part of the transform. */
  void SetScale(TScalarType scale);

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are 4 parameters. The first one represents the
   * rotation, the second one the scale and the last 
   * two represent the offset. */
  const ParametersType & GetParameters( void ) const;

 
  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the 
   * transform is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

  /** Set the transformation to an Identity
   * This sets the matrix to identity and the Offset to null. */
  virtual void SetIdentity( void );


protected:
  Similarity2DTransform();
  ~Similarity2DTransform(){};
  void PrintSelf(std::ostream &os, Indent indent) const;

  /** Compute the components of the rotation matrix and offset in the superclass. */
  virtual void ComputeMatrixAndOffset(void);

private:
  Similarity2DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  TScalarType m_Scale; 

}; //class Similarity2DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSimilarity2DTransform.txx"
#endif

#endif /* __itkSimilarity2DTransform_h */
