/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkGaborKernelFunction_h
#define __itkGaborKernelFunction_h

#include "itkKernelFunction.h"
#include "vnl/vnl_math.h"
#include <math.h>

namespace itk
{
/** \class GaborKernelFunction
 * \brief Gabor kernel used for various computer vision tasks.
 *
 * This class encapsulates a complex Gabor kernel used for
 * various computer vision tasks such as texture segmentation,
 * motion analysis, and object recognition.  It is essentially
 * a complex sinusoid enveloped within a gaussian.
 * See the discussion in
 *
 *   Andreas Klein, Forester Lee, and Amir A. Amini, "Quantitative
 *   Coronary Angiography with Deformable Spline Models", IEEE-TMI
 *   16(5):468-482, October 1997.
 *
 * for a basic discussion including additional references.
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://hdl.handle.net/1926/500
 *
 * \sa KernelFunction
 *
 * \ingroup Functions
 * \ingroup ITKReview
 */
class ITK_EXPORT GaborKernelFunction:public KernelFunction
{
public:
  /** Standard class typedefs. */
  typedef GaborKernelFunction  Self;
  typedef KernelFunction       Superclass;
  typedef SmartPointer< Self > Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaborKernelFunction, KernelFunction);

  /** Evaluate the function. */
  inline double Evaluate(const double & u) const
  {
    double parameter = vnl_math_sqr(u / this->m_Sigma);
    double envelope = vcl_exp(-0.5 * parameter);
    double phase = 2.0 * vnl_math::pi * this->m_Frequency * u
                   + this->m_PhaseOffset;

    if ( this->m_CalculateImaginaryPart )
      {
      return envelope * vcl_sin(phase);
      }
    else
      {
      return envelope * vcl_cos(phase);
      }
  }

  itkSetMacro(Sigma, double);
  itkGetConstMacro(Sigma, double);

  itkSetMacro(Frequency, double);
  itkGetConstMacro(Frequency, double);

  itkSetMacro(PhaseOffset, double);
  itkGetConstMacro(PhaseOffset, double);

  itkSetMacro(CalculateImaginaryPart, bool);
  itkGetConstMacro(CalculateImaginaryPart, bool);
  itkBooleanMacro(CalculateImaginaryPart);
protected:
  GaborKernelFunction();
  ~GaborKernelFunction();
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  GaborKernelFunction(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented

  /** Standard deviation of the Gaussian envelope */
  double m_Sigma;

  /** Modulation frequency of the sine or cosine component */
  double m_Frequency;

  /** Phase offset of the sine or cosine component */
  double m_PhaseOffset;

  /** Evaluate using the complex part */
  bool m_CalculateImaginaryPart;
};
} // end namespace itk

#endif
