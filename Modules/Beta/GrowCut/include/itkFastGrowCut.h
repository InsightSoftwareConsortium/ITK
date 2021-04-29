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

#ifndef itkFastGrowCut_h
#define itkFastGrowCut_h

#include <memory>

#include "FastGrowCut.h"

#include "itkImageToImageFilter.h"

namespace itk
{

/** \class FastGrowCut
 *
 * \brief Segments a 3D image from user-provided seeds.
 *
 * Seeds could be just foreground and background, or multiple seeds for multiple objects.
 *
 * Computational complexity is V*log(V), where V is number of voxels in the region to be segmented.
 *
 * Based on the method first introduced by:
 * 
 * Liangjia Zhu, Ivan Kolesov, Yi Gao, Ron Kikinis, Allen Tannenbaum.
 * An Effective Interactive Medical Image Segmentation Method Using Fast GrowCut
 * International Conference on Medical Image Computing and Computer Assisted Intervention (MICCAI),
 * Interactive Medical Image Computing Workshop, 2014
 * https://robobees.seas.harvard.edu/files/nac/files/zhu-miccai2014.pdf
 *
 * Original code was located at: https://github.com/ljzhu/FastGrowCut
 *
 * \ingroup GrowCut
 *
 */
template <typename TInputImage, typename TLabelImage>
class ITK_TEMPLATE_EXPORT FastGrowCut : public ImageToImageFilter<TInputImage, TLabelImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FastGrowCut);

  /** Standard class type aliases. */
  using Self = FastGrowCut;
  using Superclass = ImageToImageFilter<TInputImage, TLabelImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods).  */
  itkTypeMacro(FastGrowCut, ImageToImageFilter);

  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using IndexType = typename InputImageType::IndexType;
  using SizeType = typename InputImageType::SizeType;

  using LabelImageType = TLabelImage;
  using LabelPixelType = typename LabelImageType::PixelType;
  using LabelImagePointer = typename LabelImageType::Pointer;
  using LabelImageRegionType = typename LabelImageType::RegionType;
  using LabelImagePixelType = typename LabelImageType::PixelType;

  using SeedsContainerType = std::vector<IndexType>;

  using InputRealType = typename NumericTraits<InputImagePixelType>::RealType;

  itkSetMacro(InitializationFlag, bool);
  itkGetMacro(InitializationFlag, bool);

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  SetSeedImage(const LabelImageType * seedImage)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput(1, const_cast<LabelImageType *>(seedImage));
  }
  const LabelImageType *
  GetSeedImage()
  {
    return static_cast<const LabelImageType *>(this->ProcessObject::GetInput(1));
  }

  void
  GenerateData() override;


#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  static_assert(TInputImage::ImageDimension == 3, "FastGrowCut only works with 3D images");
  static_assert(TLabelImage::ImageDimension == 3, "FastGrowCut only works with 3D images");
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputImagePixelType>));
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<LabelImagePixelType>));
  // End concept checking
#endif

protected:
  FastGrowCut() = default;
  ~FastGrowCut() = default;

  // Override since the filter needs all the data for the algorithm
  void
  GenerateInputRequestedRegion() override;

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  using InternalFGCType = FGC::FastGrowCut<InputImagePixelType, LabelPixelType>;

private:
  std::vector<LabelPixelType>      m_imSeedVec;
  std::vector<LabelPixelType>      m_imLabVec;
  std::vector<InputImagePixelType> m_imSrcVec;
  std::vector<long>                m_imROI;

  std::shared_ptr<InternalFGCType> m_fastGC = std::make_shared<InternalFGCType>();

  bool m_InitializationFlag = false;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFastGrowCut.hxx"
#endif

#endif // itkFastGrowCut
