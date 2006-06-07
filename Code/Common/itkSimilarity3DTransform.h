/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimilarity3DTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSimilarity3DTransform_h
#define __itkSimilarity3DTransform_h

#include <iostream>
#include "itkVersorRigid3DTransform.h"

namespace itk
{

/** \brief Similarity3DTransform of a vector space (e.g. space coordinates)
 *
 * This transform applies a rotation, translation and isotropic scaling to the space.
 *
 * \ingroup Transforms
 *
 * \sa VersorRigid3DTransform
 */
template < class TScalarType=double >    // Data type for scalars (float or double)
class ITK_EXPORT Similarity3DTransform : 
      public VersorRigid3DTransform< TScalarType > 
{
public:
  /** Standard class typedefs. */
  typedef Similarity3DTransform                  Self;
  typedef VersorRigid3DTransform< TScalarType >  Superclass;
  typedef SmartPointer<Self>                     Pointer;
  typedef SmartPointer<const Self>               ConstPointer;
    
  /** New macro for creation of through a Smart Pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( Similarity3DTransform, VersorRigid3DTransform );

  /** Dimension of parameters. */
  itkStaticConstMacro(SpaceDimension, unsigned int, 3);
  itkStaticConstMacro(InputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(OutputSpaceDimension, unsigned int, 3);
  itkStaticConstMacro(ParametersDimension, unsigned int, 7);

    /** Parameters Type   */
  typedef typename Superclass::ParametersType         ParametersType;
  typedef typename Superclass::JacobianType           JacobianType;
  typedef typename Superclass::ScalarType             ScalarType;
  typedef typename Superclass::InputPointType         InputPointType;
  typedef typename Superclass::OutputPointType        OutputPointType;
  typedef typename Superclass::InputVectorType        InputVectorType;
  typedef typename Superclass::OutputVectorType       OutputVectorType;
  typedef typename Superclass::InputVnlVectorType     InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType    OutputVnlVectorType;
  typedef typename Superclass::InputCovariantVectorType 
                                                      InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType      
                                                      OutputCovariantVectorType;
  typedef typename Superclass::MatrixType             MatrixType;
  typedef typename Superclass::InverseMatrixType      InverseMatrixType;
  typedef typename Superclass::CenterType             CenterType;
  typedef typename Superclass::OffsetType             OffsetType;
  typedef typename Superclass::TranslationType        TranslationType;

  /** Versor type. */
  typedef typename Superclass::VersorType             VersorType;
  typedef typename Superclass::AxisType               AxisType;
  typedef typename Superclass::AngleType              AngleType;
  typedef          TScalarType                        ScaleType;
  
  /** Set the transformation from a container of parameters This is typically
   * used by optimizers.  There are 7 parameters. The first three represent the
   * versor, the next three represent the translation and the last one
   * represents the scaling factor. */
  void SetParameters( const ParametersType & parameters );
  virtual const ParametersType& GetParameters(void) const;

  /** Set/Get the value of the isotropic scaling factor */
  void SetScale( ScaleType scale );
  itkGetConstReferenceMacro( Scale, ScaleType );
  
  /** This method computes the Jacobian matrix of the transformation.
   * given point or vector, returning the transformed point or
   * vector. The rank of the Jacobian will also indicate if the 
   * transform is invertible at this point. */
  const JacobianType & GetJacobian(const InputPointType  &point ) const;

protected:
  Similarity3DTransform(unsigned int outputSpaceDim,
                         unsigned int paramDim);
  Similarity3DTransform(const MatrixType & matrix,
                         const OutputVectorType & offset);
  Similarity3DTransform();
  ~Similarity3DTransform(){};

  void PrintSelf(std::ostream &os, Indent indent) const;

  /** Recomputes the matrix by calling the Superclass::ComputeMatrix() and then
   * applying the scale factor. */
  void ComputeMatrix();
  
private:
  Similarity3DTransform(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  ScaleType    m_Scale;

}; //class Similarity3DTransform


}  // namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_Similarity3DTransform(_, EXPORT, x, y) namespace itk { \
  _(1(class EXPORT Similarity3DTransform< ITK_TEMPLATE_1 x >)) \
  namespace Templates { typedef Similarity3DTransform< ITK_TEMPLATE_1 x > Similarity3DTransform##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkSimilarity3DTransform+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkSimilarity3DTransform.txx"
#endif

#endif /* __itkSimilarity3DTransform_h */
