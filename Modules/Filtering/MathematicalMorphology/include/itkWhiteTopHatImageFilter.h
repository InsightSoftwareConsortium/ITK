/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkWhiteTopHatImageFilter_h
#define itkWhiteTopHatImageFilter_h

#include "itkKernelImageFilter.h"

namespace itk
{
/** \class WhiteTopHatImageFilter
 * \brief White top hat extracts local maxima that are larger than the structuring element
 *
 * Top-hats are described in Chapter 4.5 of Pierre Soille's book
 * "Morphological Image Analysis: Principles and Applications",
 * Second Edition, Springer, 2003.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */
template <typename TInputImage, typename TOutputImage, typename TKernel>
class ITK_TEMPLATE_EXPORT WhiteTopHatImageFilter : public KernelImageFilter<TInputImage, TOutputImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(WhiteTopHatImageFilter);

  /** Standard class type aliases. */
  using Self = WhiteTopHatImageFilter;
  using Superclass = KernelImageFilter<TInputImage, TOutputImage, TKernel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** Kernel type alias. */
  using KernelType = TKernel;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(WhiteTopHatImageFilter, KernelImageFilter);

  /** A safe border is added to input image to avoid borders effects
   * and remove it once the closing is done */
  itkSetMacro(SafeBorder, bool);
  itkGetConstReferenceMacro(SafeBorder, bool);
  itkBooleanMacro(SafeBorder);

  /** define values used to determine which algorithm to use */
  enum AlgorithmType
  {
    BASIC = 0,
    HISTO = 1,
    ANCHOR = 2,
    VHGW = 3
  };

  /** Set/Get the backend filter class. */
  itkSetMacro(Algorithm, int);
  itkGetConstMacro(Algorithm, int);

  itkSetMacro(ForceAlgorithm, bool);
  itkGetConstReferenceMacro(ForceAlgorithm, bool);
  itkBooleanMacro(ForceAlgorithm);

protected:
  WhiteTopHatImageFilter();
  ~WhiteTopHatImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

private:
  bool m_SafeBorder;

  int m_Algorithm;

  bool m_ForceAlgorithm;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkWhiteTopHatImageFilter.hxx"
#endif

#endif
