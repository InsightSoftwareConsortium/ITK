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
#ifndef itkNoiseBaseImageFilter_h
#define itkNoiseBaseImageFilter_h

#include "itkInPlaceImageFilter.h"
#if !defined( ITK_LEGACY_FUTURE_REMOVE )
# include <vcl_ctime.h>
#endif
#include <ctime>

namespace itk
{

/** \class NoiseBaseImageFilter
 *
 * \brief An Abstract Base class for Noise image filters
 *
 * This class add common methods for setting a seed for the random
 * generators used to generate the noise.
 *
 * \sa InPlaceImageFilter
 * \ingroup ITKImageNoise
 */
template <class TInputImage, class TOutputImage=TInputImage>
class ITK_TEMPLATE_EXPORT NoiseBaseImageFilter :
  public InPlaceImageFilter<TInputImage,TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef NoiseBaseImageFilter                          Self;
  typedef InPlaceImageFilter<TInputImage,TOutputImage > Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  typedef typename Superclass::OutputImagePixelType OutputImagePixelType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(NoiseBaseImageFilter, InPlaceImageFilter);

  /** Set/Get the seed for random initialization  */
  itkGetConstMacro(Seed, uint32_t);
  itkSetMacro(Seed, uint32_t);

  /** Set the seed to a value initialized with the current time and
   * process clock. */
  virtual void SetSeed();

protected:
  NoiseBaseImageFilter();

  virtual ~NoiseBaseImageFilter() ITK_OVERRIDE = 0;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  inline static uint32_t Hash(uint32_t a, uint32_t b)
  {
    //  Knuth's Multiplicative Method for hashing
    return (a+b)*2654435761u;
  }

  // Clamp and round the input value to the output
  static OutputImagePixelType ClampCast(const double &value);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NoiseBaseImageFilter);

  uint32_t m_Seed;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNoiseBaseImageFilter.hxx"
#endif

#endif
