/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDerivativeSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianDerivativeSpatialFunction_h
#define __itkGaussianDerivativeSpatialFunction_h

#include "itkSpatialFunction.h"
#include "itkFixedArray.h"
#include "itkPoint.h"

namespace itk
{

/** \class GaussianDerivativeSpatialFunction
 * \brief N-dimensional gaussian spatial function class
 *
 * GaussianDerivativeSpatialFunction implements a standard derivative of gaussian 
 * curve in N-d.
 * m_Normalized determines whether or not the Derivative of the Gaussian 
 * is normalized (whether or not the sum over infinite space is 1.0)
 *
 * m_Scale scales the output of the Gaussian to span a range
 * larger than 0->1, and is often set to the maximum value
 * of the output data type (for instance, 255 for uchars)
 *
 * \ingroup SpatialFunctions
 */
template <typename TOutput=double, 
          unsigned int VImageDimension=3,
          typename TInput=Point<double, VImageDimension> >
class ITK_EXPORT GaussianDerivativeSpatialFunction : public SpatialFunction<TOutput, VImageDimension, TInput>
{
public:
  /** Standard class typedefs. */
  typedef GaussianDerivativeSpatialFunction Self;
  typedef SpatialFunction<TOutput, VImageDimension, TInput> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianDerivativeSpatialFunction, SpatialFunction);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType OutputType;

  /** Type used to store derivatives parameters. */
  typedef FixedArray<double, VImageDimension> ArrayType;

  /** Type used to return the derivatives in each direction */
  typedef Vector<double, VImageDimension> VectorType;

  /** Evaluate the function at a given position and return the 
   *  value in the specific direction. SetDirection() should be used
   *  to set the direction.*/
  OutputType Evaluate(const TInput& position) const;

  /** Evaluate the function at a given position and return a vector */
  VectorType EvaluateVector(const TInput& position) const;

  /** Gets and sets for gaussian parameters */
  itkSetMacro(Scale, double);
  itkGetMacro(Scale, double);
  itkSetMacro(Normalized, bool);
  itkGetMacro(Normalized, bool);
  itkSetMacro(Sigma, ArrayType);
  itkGetMacro(Sigma, ArrayType);
  itkSetMacro(Mean, ArrayType);
  itkGetMacro(Mean, ArrayType);
  itkSetMacro(Direction, unsigned int);
  itkGetMacro(Direction, unsigned int);

protected:
  GaussianDerivativeSpatialFunction();
  virtual ~GaussianDerivativeSpatialFunction();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  GaussianDerivativeSpatialFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Current direction */
  unsigned int m_Direction;

  /** The standard deviation in each direction. */
  ArrayType m_Sigma;

  /** The mean in each direction. */
  ArrayType m_Mean;

  /** A scale factor multiplied by the true value of the Gaussian. */
  double m_Scale;

  /** Whether or not to normalize the Gaussian. */
  bool m_Normalized;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianDerivativeSpatialFunction.txx"
#endif

#endif
