/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScaleLogarithmicTransform.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkScaleLogarithmicTransform_h
#define __itkScaleLogarithmicTransform_h

#include "itkScaleTransform.h"

namespace itk
{

/** \brief Logarithmic Scale transformation of a vector space (e.g. space coordinates)
 *
 * The only difference between this class and its superclass the ScaleTransform 
 * is that here the parameters of the transformation are the logarithms of the
 * scales. This facilitates to linearize the expressions used for optimization.
 *
 * \ingroup Transforms
 */
template <
    class TScalarType=float, // Type for cordinate representation type (float or double)
    unsigned int NDimensions=3  >  // Number of dimensions
class ITK_EXPORT ScaleLogarithmicTransform : 
                            public ScaleTransform< TScalarType, 
                                                   NDimensions >
{
public:
  /** Standard class typedefs.   */
  typedef ScaleLogarithmicTransform Self;
  typedef ScaleTransform< TScalarType, NDimensions >  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** New macro for creation of through a smart pointer. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( ScaleLogarithmicTransform, ScaleTransform );

  /** Dimension of the domain space. */
  itkStaticConstMacro(SpaceDimension, unsigned int, NDimensions);
  itkStaticConstMacro(ParametersDimension, unsigned int, NDimensions);

  /** Scalar type. */
  typedef typename Superclass::ScalarType  ScalarType;

  /** Parameters type. */
  typedef typename Superclass::ParametersType  ParametersType;

  /** Jacobian type. */
  typedef typename Superclass::JacobianType  JacobianType;

  /** Standard vector type for this class. */
  typedef typename Superclass::ScaleType              ScaleType;

  /** Standard vector type for this class. */
  typedef typename Superclass::InputVectorType          InputVectorType;
  typedef typename Superclass::OutputVectorType         OutputVectorType;
  
  /** Standard covariant vector type for this class. */
  typedef typename Superclass::InputCovariantVectorType   InputCovariantVectorType;
  typedef typename Superclass::OutputCovariantVectorType  OutputCovariantVectorType;
  
  /** Standard vnl_vector type for this class. */
  typedef typename Superclass::InputVnlVectorType         InputVnlVectorType;
  typedef typename Superclass::OutputVnlVectorType        OutputVnlVectorType;
  
  /** Standard coordinate point type for this class. */
  typedef typename Superclass::InputPointType             InputPointType;
  typedef typename Superclass::OutputPointType            OutputPointType;
  
  /** Set parameters.
   * This method sets the parameters for the transform
   * value specified by the user. */
  void SetParameters(const ParametersType & parameters);

  /** Get the parameters that uniquely define the transform
   * This is typically used by optimizers.
   * There are 4 parameters. The first one represents the
   * rotation, the second one the scale and the last 
   * two represent the offset. */
  const ParametersType & GetParameters( void ) const;

  /** Get the Jacobian matrix. */
  const JacobianType & GetJacobian( const InputPointType & point ) const;

protected:
  /** Construct an ScaleLogarithmicTransform object. */
  ScaleLogarithmicTransform();

  /** Destroy an ScaleLogarithmicTransform object. */
  ~ScaleLogarithmicTransform();

  /** Print contents of an ScaleLogarithmicTransform */
  void PrintSelf(std::ostream &os, Indent indent) const;

private:
  ScaleLogarithmicTransform(const Self & other); //purposely not implemented
  const Self & operator=( const Self & ); //purposely not implemented

}; //class ScaleLogarithmicTransform

}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScaleLogarithmicTransform.txx"
#endif

#endif /* __itkScaleLogarithmicTransform_h */
