/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRigid3DTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRigid3DTransform_h
#define __itkRigid3DTransform_h

#include <iostream>
#include "itkMatrixOffsetTransformBase.h"
#include "itkExceptionObject.h"
#include "itkMatrix.h"
#include "itkVersor.h"

namespace itk
{

/** \brief Rigid3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation and translation to the space
 *
 * \ingroup Transforms
 */
template < class TScalarType=double >    // type for scalars (float or double)
class ITK_EXPORT Rigid3DTransform :
   public MatrixOffsetTransformBase< TScalarType, 3, 3> 
{
public:
  /** Standard class typedefs. */
  typedef Rigid3DTransform                                 Self;
  typedef MatrixOffsetTransformBase< TScalarType, 3, 3 >   Superclass;
  typedef SmartPointer<Self>                               Pointer;
  typedef SmartPointer<const Self>                         ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro( Rigid3DTransform, MatrixOffsetTransformBase );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  /** Dimension of the space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 12);

  typedef typename Superclass::ParametersType             ParametersType;
  typedef typename Superclass::JacobianType               JacobianType;
  typedef typename Superclass::ScalarType                 ScalarType;
  typedef typename Superclass::InputVectorType            InputVectorType;
  typedef typename Superclass::OutputVectorType           OutputVectorType;
  typedef typename Superclass::InputCovariantVectorType  
                                                     InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType  
                                                     OutputCovariantVectorType;
  typedef typename Superclass::InputVnlVectorType         InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType        OutputVnlVectorType;
  typedef typename Superclass::InputPointType             InputPointType;
  typedef typename Superclass::OutputPointType            OutputPointType;
  typedef typename Superclass::MatrixType                 MatrixType;
  typedef typename Superclass::InverseMatrixType          InverseMatrixType;
  typedef typename Superclass::CenterType                 CenterType;
  typedef typename Superclass::TranslationType            TranslationType;
  typedef typename Superclass::OffsetType                 OffsetType;

  /**
   * Get rotation Matrix from an Rigid3DTransform
   *
   * This method returns the value of the rotation of the
   * Rigid3DTransform.
   *
   * \deprecated Use GetMatrix instead
   **/
   const MatrixType & GetRotationMatrix()
     { return this->GetMatrix(); }

  /**
   * Set the rotation Matrix of a Rigid3D Transform
   *
   * This method sets the 3x3 matrix representing a rotation
   * in the transform.  The Matrix is expected to be orthogonal
   * with a certain tolerance.
   *
   * \deprecated Use SetMatrix instead
   * 
   **/
  virtual void SetRotationMatrix(const MatrixType & matrix)
      { this->SetMatrix(matrix); }

  /**
   * Compose the transformation with a translation
   *
   * This method modifies self to include a translation of the
   * origin.  The translation is precomposed with self if pre is
   * true, and postcomposed otherwise.
   **/
  void Translate(const OffsetType & offset, bool pre=false);

  /**
   * TransformCovariantVector can be simplified if the matrix is orthogonal
   * as is the case for rigid transforms.
   *
   * This function call is specialization for rigid transforms.
   **/
  OutputCovariantVectorType TransformCovariantVector(
                                const InputCovariantVectorType &vector) const;

  /**
   * Back transform by an affine transformation
   *
   * This method finds the point or vector that maps to a given
   * point or vector under the affine transformation defined by
   * self.  If no such point exists, an exception is thrown.
   *
   * \deprecated Please use GetInverseTransform and then call the forward
   *   transform using the result.
   *
   **/
  inline InputPointType      BackTransform(const OutputPointType 
                                                   &point ) const;
  inline InputVectorType     BackTransform(const OutputVectorType 
                                                   &vector) const;
  inline InputVnlVectorType  BackTransform( const OutputVnlVectorType
                                                   &vector) const;
  inline InputCovariantVectorType BackTransform(const OutputCovariantVectorType
                                                   &vector) const;

protected:
  Rigid3DTransform(unsigned int spaceDim,
                   unsigned int paramDim);
  Rigid3DTransform(const MatrixType & matrix,
                   const OutputVectorType & offset);
  Rigid3DTransform();
  ~Rigid3DTransform();
  
  /**
   * Print contents of an Rigid3DTransform
   **/
  void PrintSelf(std::ostream &os, Indent indent) const;

private:
  Rigid3DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
}; //class Rigid3DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRigid3DTransform.txx"
#endif

#endif /* __itkRigid3DTransform_h */
