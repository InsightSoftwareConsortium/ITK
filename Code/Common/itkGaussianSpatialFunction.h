/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianSpatialFunction_h
#define __itkGaussianSpatialFunction_h

#include "itkSpatialFunction.h"
#include "itkFixedArray.h"
#include "itkPoint.h"

namespace itk
{

/** \class GaussianSpatialFunction
 * \brief N-dimensional gaussian spatial function class
 *
 * GaussianSpatialFunction implements a standard gaussian curve in N-d.
 * m_Normalized determines whether or not the Gaussian is normalized
 * (whether or not the sum over infinite space is 1.0)
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
class ITK_EXPORT GaussianSpatialFunction : public SpatialFunction<TOutput, VImageDimension, TInput>
{
public:
  /** Standard class typedefs. */
  typedef GaussianSpatialFunction Self;
  typedef SpatialFunction<TOutput, VImageDimension, TInput> Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianSpatialFunction, SpatialFunction);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType OutputType;

  /** Type used to store gaussian parameters. */
  typedef FixedArray<double, VImageDimension> ArrayType;

  /** Evaluate the function at a given position. */
  OutputType Evaluate(const TInput& position) const;

  /** Gets and sets for gaussian parameters */
  itkSetMacro(Scale, double);
  itkGetMacro(Scale, double);
  itkSetMacro(Normalized, bool);
  itkGetMacro(Normalized, bool);
  itkSetMacro(Sigma, ArrayType);
  itkGetMacro(Sigma, ArrayType);
  itkSetMacro(Mean, ArrayType);
  itkGetMacro(Mean, ArrayType);

protected:
  GaussianSpatialFunction();
  virtual ~GaussianSpatialFunction();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  GaussianSpatialFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

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
#include "itkGaussianSpatialFunction.txx"
#endif

#endif
