/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkGaussianSpatialFunction_h
#define __itkGaussianSpatialFunction_h

#include "itkSpatialFunction.h"
#include "itkArray.h"
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
  typedef Array<double, VImageDimension> TArrayType;

  /** Evaluate the function at a given position. */
  OutputType Evaluate(const TInput& position) const;

  /** Gets and sets for gaussian parameters */
  itkSetMacro(Scale, double);
  itkGetMacro(Scale, double);
  itkSetMacro(Normalized, bool);
  itkGetMacro(Normalized, bool);
  itkSetMacro(Sigma, TArrayType);
  itkGetMacro(Sigma, TArrayType);
  itkSetMacro(Mean, TArrayType);
  itkGetMacro(Mean, TArrayType);

protected:
  GaussianSpatialFunction();
  virtual ~GaussianSpatialFunction();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  GaussianSpatialFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The standard deviation in each direction. */
  TArrayType m_Sigma;

  /** The mean in each direction. */
  TArrayType m_Mean;

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
