/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalableAffineTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkScalableAffineTransform_h
#define __itkScalableAffineTransform_h

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
class ITK_EXPORT ScalableAffineTransform 
: public AffineTransform< TScalarType, NDimensions >
{
public:
  /** Standard typedefs   */
  typedef ScalableAffineTransform                      Self;
  typedef AffineTransform< TScalarType, NDimensions >  Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;
    
  /** Run-time type information (and related methods).   */
  itkTypeMacro( ScalableAffineTransform, AffineTransform );
  
  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );
  
  /** Dimension of the domain space. */
  itkStaticConstMacro(InputSpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, NDimensions);
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
  typedef typename Superclass::InverseMatrixType         InverseMatrixType;
  typedef typename Superclass::CenterType                CenterType;
  typedef typename Superclass::OffsetType                OffsetType;
  typedef typename Superclass::TranslationType           TranslationType;
    
  /** Set the transformation to an Identity
   *
   * This sets the matrix to identity and the Offset to null. */
  void SetIdentity( void );
  
  /** Set the scale of the transform */
  virtual void SetScale( const InputVectorType & scale );
  virtual void SetScaleComponent( const InputVectorType & scale )
    { this->SetScale(scale); };

  /** Set the scale of the transform */
  virtual void SetScale( const double scale[NDimensions] );
  virtual void SetScaleComponent( const double scale[NDimensions] )
    { this->SetScale(scale); };

  /** Get the scale of the transform*/
  virtual const double * GetScale() const 
    { return m_Scale; };
  virtual const double * GetScaleComponent() const 
    { return m_Scale; };

  /** Set the matrix of the transform. The matrix should not include
   *  scale.
   *
   *  \deprecated use SetMatrix instead */
  void SetMatrixComponent(const MatrixType &matrix)
    { this->SetMatrix( matrix ); };
  /** Get matrix of the transform.
   *
   * \deprecated use GetMatrix instead  */
  const MatrixType & GetMatrixComponent() const 
    { return this->GetMatrix(); }

  /** Set offset (origin) of the Transform.
   *
   * \deprecated use SetTranslation instead. */
  void SetOffsetComponent(const OffsetType &offset)
    { this->SetTranslation( offset ); };

  /** Get offset of the transform
   *
   * \deprecated use GetTranslation instead. */
  const OffsetType & GetOffsetComponent(void) const 
    { return this->GetTranslation(); }


protected:
  /** Construct an ScalableAffineTransform object 
   *
   * This method constructs a new AffineTransform object and
   * initializes the matrix and offset parts of the transformation
   * to values specified by the caller.  If the arguments are
   * omitted, then the AffineTransform is initialized to an identity
   * transformation in the appropriate number of dimensions.   **/
  ScalableAffineTransform(const MatrixType &matrix,
                          const OutputVectorType &offset);
  ScalableAffineTransform(unsigned int outputSpaceDimension,
                          unsigned int parametersDimension);
  ScalableAffineTransform();      
   
  void ComputeMatrix();

  /** Destroy an ScalableAffineTransform object   */
  virtual ~ScalableAffineTransform();

  /** Print contents of an ScalableAffineTransform */
  void PrintSelf(std::ostream &s, Indent indent) const;

  void Set_M_Scale(const double * scale)
    { for(int i=0; i<InputSpaceDimension; i++) { m_Scale[i] = scale[i]; } };

private:

  ScalableAffineTransform(const Self & other);
  const Self & operator=( const Self & );
  
  double               m_Scale[NDimensions];
  InputVectorType      m_MatrixScale;

}; //class ScalableAffineTransform
  
}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalableAffineTransform.txx"
#endif

#endif /* __itkScalableAffineTransform_h */

