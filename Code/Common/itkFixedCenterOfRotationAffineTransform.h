/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFixedCenterOfRotationAffineTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkFixedCenterOfRotationAffineTransform_h
#define __itkFixedCenterOfRotationAffineTransform_h

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
 class TScalarType=double,      // Data type for scalars (e.g. float or double)
 unsigned int NDimensions=3>    // Number of dimensions in the input space
class ITK_EXPORT FixedCenterOfRotationAffineTransform 
: public AffineTransform< TScalarType, NDimensions >
  {
  public:
    /** Standard typedefs   */
    typedef FixedCenterOfRotationAffineTransform  Self;
    typedef AffineTransform< TScalarType, NDimensions >  Superclass;
    typedef SmartPointer<Self>        Pointer;
    typedef SmartPointer<const Self>  ConstPointer;
    
    /** Run-time type information (and related methods).   */
    itkTypeMacro( FixedCenterOfRotationAffineTransform, AffineTransform );
  
    /** New macro for creation of through a Smart Pointer   */
    itkNewMacro( Self );
  
    /** Dimension of the domain space. */
    itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);
    itkStaticConstMacro(ParametersDimension, unsigned int,
                        NDimensions*(NDimensions+1));
  
    
    /** Types taken from the Superclass */
    typedef typename Superclass::ParametersType            ParametersType;
    typedef typename Superclass::JacobianType              JacobianType;
    typedef typename Superclass::ScalarType                ScalarType;
    typedef typename Superclass::InputVectorType           InputVectorType;
    typedef typename Superclass::OutputVectorType          OutputVectorType;
    typedef typename Superclass::InputCovariantVectorType     
                                              InputCovariantVectorType;
    typedef typename Superclass::OutputCovariantVectorType    
                                              OutputCovariantVectorType;
    typedef typename Superclass::InputVnlVectorType        InputVnlVectorType;
    typedef typename Superclass::OutputVnlVectorType       OutputVnlVectorType;
    typedef typename Superclass::InputPointType            InputPointType;
    typedef typename Superclass::OutputPointType           OutputPointType;
    typedef typename Superclass::MatrixType                MatrixType;
    typedef typename Superclass::OffsetType                OffsetType;
  
  
    /** Set the transformation to an Identity
     *
     * This sets the matrix to identity and the Offset to null. */
    void SetIdentity( void );
  
    /** Find inverse of an affine transformation
     *
     * This method creates and returns a new 
     * FixedCenterOfRotationAffineTransform object
     * which is the inverse of self.  If self is not invertible,
     * an exception is thrown.   **/
    FixedCenterOfRotationAffineTransform::Pointer Inverse(void) const;
  
    /** Print contents of an FixedCenterOfRotationAffineTransform */
    void PrintSelf(std::ostream &s, Indent indent) const;
  
    /** Compute the Jacobian of the transformation
     *
     * This method computes the Jacobian matrix of the transformation.
     * given point or vector, returning the transformed point or
     * vector. The rank of the Jacobian will also indicate if the transform
     * is invertible at this point. */
    const JacobianType & GetJacobian(const InputPointType  &point ) const;
  
    /** Set and Get the center of rotation */
    itkSetMacro( CenterOfRotation, InputPointType );
    //void SetCenterOfRotation( const InputPointType & centerOfRotation );
    itkGetConstReferenceMacro( CenterOfRotation, InputPointType );
  
    OutputPointType TransformPoint( const InputPointType & point ) const;
  
  protected:
    /** Construct an FixedCenterOfRotationAffineTransform object **/
    FixedCenterOfRotationAffineTransform();      
    
    /** Destroy an FixedCenterOfRotationAffineTransform object   **/
    virtual ~FixedCenterOfRotationAffineTransform();
  
    /** Recompute inverse of the transformation matrix   **/
    void RecomputeInverse();
  
  private:
    FixedCenterOfRotationAffineTransform(const Self & other);
    const Self & operator=( const Self & );
  
    InputPointType      m_CenterOfRotation;
  
  }; //class FixedCenterOfRotationAffineTransform
  
}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFixedCenterOfRotationAffineTransform.txx"
#endif

#endif /* __itkFixedCenterOfRotationAffineTransform_h */





