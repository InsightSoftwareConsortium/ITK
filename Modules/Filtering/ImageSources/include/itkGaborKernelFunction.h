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

#include "itkKernelFunctionBase.h"
#include <cmath>

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
 * \sa KernelFunctionBase
 *
 * \ingroup Functions
 * \ingroup ITKImageSources
 */
template< typename TRealValueType>
class GaborKernelFunction:public KernelFunctionBase<TRealValueType>
{
public:
  /** Standard class typedefs. */
  typedef GaborKernelFunction                Self;
  typedef KernelFunctionBase<TRealValueType> Superclass;
  typedef SmartPointer< Self >               Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaborKernelFunction, KernelFunctionBase);

  /** Evaluate the function. */
  inline TRealValueType Evaluate(const TRealValueType & u) const
  {
    TRealValueType parameter = vnl_math_sqr(u / this->m_Sigma);
    TRealValueType envelope = std::exp(static_cast< TRealValueType >(-0.5) * parameter);
    TRealValueType phase = static_cast< TRealValueType >(2.0 * vnl_math::pi) * this->m_Frequency * u
                   + this->m_PhaseOffset;

    if ( this->m_CalculateImaginaryPart )
      {
      return envelope * std::sin(phase);
      }
    else
      {
      return envelope * std::cos(phase);
      }
  }

  itkSetMacro(Sigma, TRealValueType);
  itkGetConstMacro(Sigma, TRealValueType);

  itkSetMacro(Frequency, TRealValueType);
  itkGetConstMacro(Frequency, TRealValueType);

  itkSetMacro(PhaseOffset, TRealValueType);
  itkGetConstMacro(PhaseOffset, TRealValueType);

  itkSetMacro(CalculateImaginaryPart, bool);
  itkGetConstMacro(CalculateImaginaryPart, bool);
  itkBooleanMacro(CalculateImaginaryPart);

protected:
  GaborKernelFunction()
    {
    this->m_CalculateImaginaryPart = false;
    this->m_Sigma = NumericTraits< TRealValueType >::One;
    this->m_Frequency = static_cast<TRealValueType>(0.4);
    this->m_PhaseOffset = NumericTraits< TRealValueType >::Zero;
    }
  ~GaborKernelFunction() {};
  void PrintSelf(std::ostream & os, Indent indent) const
    {
    Superclass::PrintSelf(os, indent);

    os << indent << "Sigma: " << this->GetSigma() << std::endl;
    os << indent << "Frequency: " << this->GetFrequency() << std::endl;
    os << indent << "PhaseOffset: " << this->GetPhaseOffset() << std::endl;
    os << indent << "CalculateImaginaryPart: " << this->GetCalculateImaginaryPart() << std::endl;
    }

private:
  GaborKernelFunction(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented

  /** Standard deviation of the Gaussian envelope */
  TRealValueType m_Sigma;

  /** Modulation frequency of the sine or cosine component */
  TRealValueType m_Frequency;

  /** Phase offset of the sine or cosine component */
  TRealValueType m_PhaseOffset;

  /** Evaluate using the complex part */
  bool m_CalculateImaginaryPart;
};
} // end namespace itk

#endif
