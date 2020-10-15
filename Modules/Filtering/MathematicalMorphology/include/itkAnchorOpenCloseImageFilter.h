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
#ifndef itkAnchorOpenCloseImageFilter_h
#define itkAnchorOpenCloseImageFilter_h

#include "itkKernelImageFilter.h"
#include "itkProgressReporter.h"
#include "itkAnchorOpenCloseLine.h"
#include "itkAnchorErodeDilateLine.h"
#include "itkBresenhamLine.h"

namespace itk
{
/**
 * \class AnchorOpenCloseImageFilter
 * \brief class to implement openings and closings using anchor
 * methods.
 *
 * Anchor methods directly implement opening/closing by line structuring elements, and
 * erosion/dilation is slightly more complicated. This class used line
 * structuring elements to produce more complex shaped SEs, and must
 * be instantiated with a decomposable structuring element type such
 * as FlatStructuringElement. The direct implementation of openings by
 * lines gives a mechanism to short cut the decomposition slightly -
 * e.g. in the case of a rectangle the basic decomposition is Ex Ey Ez
 * Dz Dy Dz, which can be changed to Ex Ey Oz Dy Dx, where Ex, Dx and Ox
 * indicate erosions, dilations and openings along the x
 * direction. Because anchor operations do openings directly, this is
 * a saving of one pass through the filter. Unfortunately it results
 * in more complex template parameters because the appropriate
 * comparison operations need to be passed in. The less
 *
 * \ingroup ITKMathematicalMorphology
 */
template <typename TImage, typename TKernel, typename TCompare1, typename TCompare2>
class ITK_TEMPLATE_EXPORT AnchorOpenCloseImageFilter : public KernelImageFilter<TImage, TImage, TKernel>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AnchorOpenCloseImageFilter);

  /** Standard class type aliases. */
  using Self = AnchorOpenCloseImageFilter;
  using Superclass = KernelImageFilter<TImage, TImage, TKernel>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  /** Kernel type alias. */
  using KernelType = TKernel;
  using KernelLType = typename KernelType::LType;

  using InputImageType = TImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AnchorOpenCloseImageFilter, KernelImageFilter);

protected:
  AnchorOpenCloseImageFilter();
  ~AnchorOpenCloseImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Multi-thread version GenerateData. */
  void
  DynamicThreadedGenerateData(const InputImageRegionType & outputRegionForThread) override;


  InputImagePixelType m_Boundary1;
  InputImagePixelType m_Boundary2;

private:
  using BresType = BresenhamLine<Self::InputImageDimension>;
  using BresOffsetArray = typename BresType::OffsetArray;

  // the class that operates on lines -- does the opening in one
  // operation. The classes following are named on the assumption that
  // we are doing an opening

  //  using AnchorLineOpenType = AnchorOpenCloseLine<InputImagePixelType, THistogramCompare,
  // TFunction1, TFunction2>;
  using AnchorLineOpenType = AnchorOpenCloseLine<InputImagePixelType, TCompare1>;

  using AnchorLineErodeType = AnchorErodeDilateLine<InputImagePixelType, TCompare1>;

  // the class that does the dilation
  using AnchorLineDilateType = AnchorErodeDilateLine<InputImagePixelType, TCompare2>;

  void
  DoFaceOpen(InputImageConstPointer             input,
             InputImagePointer                  output,
             InputImagePixelType                border,
             KernelLType                        line,
             AnchorLineOpenType &               AnchorLineOpen,
             const BresOffsetArray              LineOffsets,
             std::vector<InputImagePixelType> & outbuffer,
             const InputImageRegionType         AllImage,
             const InputImageRegionType         face);
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAnchorOpenCloseImageFilter.hxx"
#endif

#endif
