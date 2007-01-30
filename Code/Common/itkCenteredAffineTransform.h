/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredAffineTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkCenteredAffineTransform_h
#define __itkCenteredAffineTransform_h

#include "itkAffineTransform.h"

namespace itk
{


/** \class CenteredAffineTransform
 * \brief Affine transformation with a specified center of rotation.
 *
 * This class implements an Affine transform in which the rotation center 
 * can be explicitly selected.
 * Note that the method "ComputeOffset()" must be called just before using 
 * the transform for mapping points, vectors or covariantvectors. 
 * This is necessary for updating the offset of the transform taking into 
 * account the center of rotation.
 *
 * \ingroup Transforms
 *
 */
template <
 class TScalarType=double,         // Data type for scalars
 unsigned int NDimensions=3>       // Number of dimensions in the input space
class ITK_EXPORT CenteredAffineTransform : public AffineTransform< TScalarType, 
                                                                  NDimensions >
{
public:
  /** Standard typedefs   */
  typedef CenteredAffineTransform                      Self;
  typedef AffineTransform< TScalarType, NDimensions >  Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;
  
  /** Run-time type information (and related methods).   */
  itkTypeMacro( CenteredAffineTransform, AffineTransform );

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(ParametersDimension, unsigned int,
                      NDimensions*(NDimensions+2));

  
  /** Types taken from the Superclass */
  typedef typename Superclass::ParametersType               ParametersType;
  typedef typename Superclass::JacobianType                 JacobianType;
  typedef typename Superclass::ScalarType                   ScalarType;
  typedef typename Superclass::InputVectorType              InputVectorType;
  typedef typename Superclass::OutputVectorType             OutputVectorType;
  typedef typename Superclass::InputCovariantVectorType
                                                     InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType
                                                     OutputCovariantVectorType;

  typedef typename Superclass::InputVnlVectorType           InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType          OutputVnlVectorType;
  typedef typename Superclass::InputPointType               InputPointType;
  typedef typename Superclass::OutputPointType              OutputPointType;
  typedef typename Superclass::MatrixType                   MatrixType;
  typedef typename Superclass::OffsetType                   OffsetType;


  
  /** Set/Get the transformation from a container of parameters.
   * The first (NDimension x NDimension) parameters define the
   * matrix, the next N parameters define the center of rotation
   * and the last N parameters define the translation to be applied
   * after the coordinate system has been restored to the rotation center.
   * Note that the Offset of the superclass is no longer in the 
   * parameters array since it is fully dependent on the rotation
   * center and the translation parameters. */
  void SetParameters( const ParametersType & parameters );
  const ParametersType& GetParameters(void) const;

  /** Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

protected:
  /** Construct an CenteredAffineTransform object **/
  CenteredAffineTransform();
  
  /** Destroy an CenteredAffineTransform object   **/
  virtual ~CenteredAffineTransform();

private:
  CenteredAffineTransform(const Self & other);
  const Self & operator=( const Self & );

}; //class CenteredAffineTransform

}  // namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_CenteredAffineTransform(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT CenteredAffineTransform< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef CenteredAffineTransform< ITK_TEMPLATE_2 x > \
                                            CenteredAffineTransform##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkCenteredAffineTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkCenteredAffineTransform.txx"
#endif


#endif /* __itkCenteredAffineTransform_h */
