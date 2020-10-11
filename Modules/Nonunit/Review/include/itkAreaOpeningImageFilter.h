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
#ifndef itkAreaOpeningImageFilter_h
#define itkAreaOpeningImageFilter_h

#include "itkAttributeMorphologyBaseImageFilter.h"
#include <functional>

namespace itk
{
/**
 * \class AreaOpeningImageFilter
 * \brief Morphological opening by attributes
 *
 * This is the base class for morphology attribute
 * operations. Attribute openings remove blobs according to criteria
 * such as area. When applied to grayscale images it has the effect of
 * trimming peaks based on area while leaving the rest of the image
 * unchanged. It is possible to use attributes besides area, but no
 * others are implemented yet. This filter uses some dodgy coding
 * practices - most notably copying the image data to a linear buffer
 * to allow direct implementation of the published algorithm. It
 * should therefore be quite a good candidate to carry out tests of
 * itk iterator performance with randomish access patterns.
 *
 * This filter is implemented using the method of Wilkinson, "A
 * comparison of algorithms for Connected set openings and Closings",
 * A. Meijster and M. H. Wilkinson, PAMI, vol 24, no. 4, April 2002.
 * Attempts at implementing the method from ISMM 2000 are also
 * included, but operation appears incorrect. Check the ifdefs if you
 * are interested.
 *
 * This code was contributed in the Insight Journal paper
 *
 * "Grayscale morphological attribute operations"
 * by Beare R.
 * https://hdl.handle.net/1926/1316
 * http://www.insight-journal.org/browse/publication/203
 *
 *
 * \author Richard Beare. Department of Medicine, Monash University, Melbourne, Australia.
 *
 * \ingroup ITKReview
 */
template <typename TInputImage,
          typename TOutputImage,
          typename TAttribute = typename TInputImage::SpacingType::ValueType>
class AreaOpeningImageFilter
  : public AttributeMorphologyBaseImageFilter<TInputImage,
                                              TOutputImage,
                                              TAttribute,
                                              std::greater<typename TInputImage::PixelType>>

{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AreaOpeningImageFilter);

  using Self = AreaOpeningImageFilter;
  using Superclass = AttributeMorphologyBaseImageFilter<TInputImage,
                                                        TOutputImage,
                                                        TAttribute,
                                                        std::greater<typename TInputImage::PixelType>>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /**
   * Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.
   */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;
  using IndexType = typename TInputImage::IndexType;
  using OffsetType = typename TInputImage::OffsetType;
  using SizeType = typename TInputImage::SizeType;
  using AttributeType = TAttribute;

  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AreaOpeningImageFilter, AttributeMorphologyBaseImageFilter);

  /**
   * Set/Get whether the image spacing is used or not - defaults to true.
   */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstReferenceMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);

protected:
  AreaOpeningImageFilter() { m_UseImageSpacing = true; }

  ~AreaOpeningImageFilter() override = default;

  void
  GenerateData() override
  {
    this->m_AttributeValuePerPixel = 1;
    if (m_UseImageSpacing)
    {
      // compute pixel size
      double psize = 1.0;
      for (unsigned i = 0; i < ImageDimension; i++)
      {
        psize *= this->GetInput()->GetSpacing()[i];
      }
      this->m_AttributeValuePerPixel = static_cast<AttributeType>(psize);
      // std::cout << "m_AttributeValuePerPixel: " <<
      // this->m_AttributeValuePerPixel << std::endl;
      // and call superclass implementation of GenerateData()
    }
    Superclass::GenerateData();
  }

  void
  PrintSelf(std::ostream & os, Indent indent) const override
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "UseImageSpacing: " << m_UseImageSpacing << std::endl;
  }

private:
  bool m_UseImageSpacing;
};
} // namespace itk
#endif
