/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredAffineTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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


/**
 * \brief Affine transformation with a specified center of rotation.
 *
 * This class implements an Affine transform in which the rotation center can be explicitly selected.
 *
 * 
 * \ingroup Transforms
 *
 *
 */

template <
 class TScalarType=double,         // Data type for scalars (e.g. float or double)
 unsigned int NDimensions=3>       // Number of dimensions in the input space
class ITK_EXPORT CenteredAffineTransform : public AffineTransform< TScalarType, NDimensions >
{
public:
  /** Standard typedefs   */
  typedef CenteredAffineTransform  Self;
  typedef AffineTransform< TScalarType, NDimensions >  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
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
  typedef typename Superclass::InputCovariantVectorType     InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType    OutputCovariantVectorType;
  typedef typename Superclass::InputVnlVectorType           InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType          OutputVnlVectorType;
  typedef typename Superclass::InputPointType               InputPointType;
  typedef typename Superclass::OutputPointType              OutputPointType;
  typedef typename Superclass::MatrixType                   MatrixType;
  typedef typename Superclass::OffsetType                   OffsetType;


  
  /** Set the transformation to an Identity
   *
   * This sets the matrix to identity and the Offset to null. */
  void SetIdentity( void );

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

 
  /** Find inverse of an affine transformation
   *
   * This method creates and returns a new CenteredAffineTransform object
   * which is the inverse of self.  If self is not invertible,
   * an exception is thrown.   **/
  CenteredAffineTransform::Pointer Inverse(void) const;

  /** Print contents of an CenteredAffineTransform */
  void PrintSelf(std::ostream &s, Indent indent) const;

  /** Compute the Jacobian of the transformation
   *
   * This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the transform
   * is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

  /** Set and Get the center of rotation */
  void SetCenter( const InputPointType & center );
  itkGetConstReferenceMacro( Center, InputPointType );

  /** Set and Get the Translation to be applied after rotation */
  void SetTranslation( const OutputVectorType & translation );
  itkGetConstReferenceMacro( Translation, OutputVectorType );
 
  /** Compute the offset using the rotation center, the matrix
   *  and the final translation. */
  virtual void ComputeOffset(void);

protected:
  /** Construct an CenteredAffineTransform object **/
     CenteredAffineTransform();      
  
  /** Destroy an CenteredAffineTransform object   **/
  virtual ~CenteredAffineTransform();

  /** Recompute inverse of the transformation matrix   **/
  void RecomputeInverse();

private:
  CenteredAffineTransform(const Self & other);
  const Self & operator=( const Self & );


  InputPointType      m_Center;

  OutputVectorType    m_Translation;


}; //class CenteredAffineTransform

}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCenteredAffineTransform.txx"
#endif

#endif /* __itkCenteredAffineTransform_h */





