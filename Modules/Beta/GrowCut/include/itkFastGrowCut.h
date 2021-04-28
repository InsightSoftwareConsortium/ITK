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

#include <limits>
#include <memory>

#include "FibHeap.h"

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
 * Updated code was adapted from Slicer:
 * https://github.com/Slicer/Slicer/blob/1a692bf36e9c99c47661fbf5fdba0fd3c3e72f95/Modules/Loadable/Segmentations/Logic/vtkImageGrowCutSegment.cxx
 *
 * \ingroup GrowCut
 *
 */
template <typename TInputImage, typename TLabelImage, typename TMaskImage = TLabelImage>
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
  using IntensityPixelType = typename InputImageType::PixelType;

  using LabelImageType = TLabelImage;
  using LabelPixelType = typename LabelImageType::PixelType;

  using MaskImageType = TMaskImage;
  using MaskPixelType = typename MaskImageType::PixelType;

  using RegionType = typename InputImageType::RegionType;
  using IndexType = typename InputImageType::IndexType;
  using SizeType = typename InputImageType::SizeType;
  using SpacingType = typename InputImageType::SpacingType;

  //using SeedsContainerType = std::vector<IndexType>;

  //using InputRealType = typename NumericTraits<IntensityPixelType>::RealType;


  /** Reset to initial state. This forces full recomputation of the result label volume.
   * This method has to be called if intensity volume changes or if seeds are deleted after initial computation.
   */
  void
  Reset();

  /** Spatial regularization factor, which can force growing in nearby regions.
   * For each physical unit distance, this much intensity level difference is simulated.
   * Zero by default, meaning spatial distance does not play a role in the region growing,
   * only intensity value similarity.
   */
  itkSetMacro(DistancePenalty, double);
  itkGetMacro(DistancePenalty, double);

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

  /** Set mask volume (input 2). Optional.
   *
   * If this volume is specified then only those regions outside the mask (where mask has zero value)
   * will be included in the segmentation result. Regions outside the mask will not be used
   * for region growing either (growing will not start from or cross through masked region).
   */
  void
  SetMaskImage(const MaskImageType * maskImage)
  {
    this->SetNthInput(2, const_cast<MaskImageType *>(maskImage));
  }
  const MaskImageType *
  GetMaskImage()
  {
    return static_cast<const MaskImageType *>(this->ProcessObject::GetInput(2));
  }

  void
  GenerateData() override;


#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  static_assert(TInputImage::ImageDimension == 3, "FastGrowCut only works with 3D images");
  static_assert(TLabelImage::ImageDimension == 3, "FastGrowCut only works with 3D images");
  static_assert(TMaskImage::ImageDimension == 3, "FastGrowCut only works with 3D images");
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<IntensityPixelType>));
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<LabelPixelType>));
  itkConceptMacro(MaskHasNumericTraitsCheck, (Concept::HasNumericTraits<MaskPixelType>));
  // End concept checking
#endif

protected:
  FastGrowCut() = default;
  ~FastGrowCut() = default;

  static constexpr NodeKeyValueType DIST_INF = std::numeric_limits<float>::max();
  static constexpr NodeKeyValueType DIST_EPSILON = 1e-3f;
  static constexpr unsigned char    NNGBH = 26;
  using DistancePixelType = float;
  using DistanceImageType = itk::Image<DistancePixelType, 3>;

  // Override since the filter needs all the data for the algorithm
  void
  GenerateInputRequestedRegion() override;

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  bool
  InitializationAHP();

  void
  DijkstraBasedClassificationAHP();

private:
  typename DistanceImageType::Pointer m_DistanceVolume = DistanceImageType::New();

  NodeIndexType m_DimX;
  NodeIndexType m_DimY;
  NodeIndexType m_DimZ;

  std::vector<NodeIndexType> m_NeighborIndexOffsets;
  std::vector<double>        m_NeighborDistancePenalties;
  std::vector<unsigned char> m_NumberOfNeighbors; // same everywhere except at the image boundary

  FibHeap *     m_Heap{ nullptr };
  FibHeapNode * m_HeapNodes{ nullptr }; // a node is stored for each voxel
  bool          m_bSegInitialized{ false };
  double        m_DistancePenalty{ 0.0 };
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFastGrowCut.hxx"
#endif

#endif // itkFastGrowCut
