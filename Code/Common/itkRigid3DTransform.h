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
 * This transform applies a rotation and translation in 3D space.
 * The transform is specified as a rotation matrix around a arbitrary center
 * and is followed by a translation.
 *
 * The parameters for this transform can be set either using individual Set
 * methods or in serialized form using SetParameters() and SetFixedParameters().
 *
 * The serialization of the optimizable parameters is an array of 12 elements.
 * The first 9 parameters represents the rotation matrix in column-major order
 * (where the column index varies the fastest). The last 3 parameters defines
 * the translation in each dimension.
 *
 * The serialization of the fixed parameters is an array of 3 elements defining
 * the center of rotation in each dimension.
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

  /** Set the transformation from a container of parameters
   * This is typically used by optimizers.
   * There are 12 parameters. The first 9 represents the rotation
   * matrix is column-major order and the last 3 represents the translation.
   *
   * \warning The rotation matrix must be orthogonal to within a specified tolerance,
   * else an exception is thrown.
   * 
   * \sa Transform::SetParameters()
   * \sa Transform::SetFixedParameters() */
   virtual void SetParameters( const ParametersType & parameters );

 /** Directly set the rotation matrix of the transform.
  * \warning The input matrix must be orthogonal to within a specified tolerance,
  * else an exception is thrown.
  *
  * \sa MatrixOffsetTransformBase::SetMatrix() */
  virtual void SetMatrix(const MatrixType &matrix);

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

   /**
    * Utility function to test if a matrix is orthogonal within a specified 
    * tolerance
    */
  bool MatrixIsOrthogonal( const MatrixType & matrix, double tol = 1e-10 );

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

// Define instantiation macro for this template.
#define ITK_TEMPLATE_Rigid3DTransform(_, EXPORT, x, y) namespace itk { \
  _(1(class EXPORT Rigid3DTransform< ITK_TEMPLATE_1 x >)) \
  namespace Templates { typedef Rigid3DTransform< ITK_TEMPLATE_1 x > Rigid3DTransform##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkRigid3DTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkRigid3DTransform.txx"
#endif

#endif /* __itkRigid3DTransform_h */
