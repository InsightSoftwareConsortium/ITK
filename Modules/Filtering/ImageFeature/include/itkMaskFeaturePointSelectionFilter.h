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
#ifndef itkMaskFeaturePointSelectionFilter_h
#define itkMaskFeaturePointSelectionFilter_h

#include "itkImageToMeshFilter.h"
#include "itkDefaultDynamicMeshTraits.h"
#include "itkImage.h"
#include "itkPointSet.h"
#include "itkImageRegionConstIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkMatrix.h"
#include "itkConceptChecking.h"
#include <vector>


namespace itk
{
/**
 *\class MaskFeaturePointSelectionFilter
 * \brief Generate a PointSet containing the feature points
 * selected from a masked 3D input image.
 *
 * MaskFeaturePointSelectionFilter takes 3D image and 3D mask as inputs
 * and generates a PointSet of feature points as output.
 *
 * This filter is intended to be used for initializing the process of
 * Physics-Based Non-Rigid Registration. It selects a fraction of non-masked
 * points with highest variance. Optionally, tensors are computed for each
 * point and stored as pixel values. [ M. Bierling, Displacement estimation
 * by hierarchical block matching, Proc. SPIE Vis. Comm. and Image Proc.,
 * vol. 1001, pp. 942-951, 1988. ].
 *
 * The filter is templated over input image and mask and output pointset.
 * \author Andriy Kot, Center for Real-Time Computing, Old Dominion University,
 * Norfolk, VA
 *
 * \sa BlockMatchingImageFilter
 *
 * \ingroup ImageFeatureExtraction
 * \ingroup ITKImageFeature
 */

template <typename TImage,
          typename TMask = TImage,
          typename TFeatures = PointSet<Matrix<SpacePrecisionType, TImage::ImageDimension, TImage::ImageDimension>,
                                        TImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT MaskFeaturePointSelectionFilter : public ImageToMeshFilter<TImage, TFeatures>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MaskFeaturePointSelectionFilter);

  /** Standard class type aliases. */
  using Superclass = ImageToMeshFilter<TImage, TFeatures>;
  using Self = MaskFeaturePointSelectionFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MaskFeaturePointSelectionFilter, ImageToMeshFilter);

  static constexpr unsigned ImageDimension = 3u;

  /** Not input specific type alias */
  using RegionType = ImageRegion<ImageDimension>;
  using SizeType = Size<ImageDimension>;
  using IndexType = Index<ImageDimension>;
  using OffsetType = Offset<ImageDimension>;

  /** Image type alias */
  using ImageType = TImage;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using ImagePixelType = typename ImageType::PixelType;

  /** Mask image type alias */
  using MaskType = TMask;
  using MaskConstPointer = typename MaskType::ConstPointer;
  using MaskPixelType = typename MaskType::PixelType;

  /** Feature points pointset type alias */
  using FeaturePointsType = TFeatures;
  using FeaturePointsPointer = typename FeaturePointsType::Pointer;
  using StructureTensorType = typename FeaturePointsType::PixelType;
  using PointType = typename FeaturePointsType::PointType;

  /** connectivity constants */
  enum
  {
    VERTEX_CONNECTIVITY = 0,
    EDGE_CONNECTIVITY = 1,
    FACE_CONNECTIVITY = 2
  };

  /** set/get non-connectivity with radius == 1 of dimension connect,
   * 0 <= connect < ImageDimension; 0 is vertex connectivity (e.g., 26 in 3D),
   * 1 is edge connectivity (e.g., 18 in 3D), 2 is face connectivity
   * (e.g., 6 in 3D), etc */
  itkSetMacro(NonConnectivity, unsigned);
  itkGetMacro(NonConnectivity, unsigned);

  /** set/get mask */
  itkSetInputMacro(MaskImage, MaskType);
  itkGetInputMacro(MaskImage, MaskType);

  /** set/get half size of the block for calculating variance */
  itkSetMacro(BlockRadius, SizeType);
  itkGetConstReferenceMacro(BlockRadius, SizeType);

  /** enable/disable tensor computations */
  itkSetMacro(ComputeStructureTensors, bool);
  itkGetMacro(ComputeStructureTensors, bool);
  itkBooleanMacro(ComputeStructureTensors);

  /** set fraction of eligible points to select */
  itkSetClampMacro(SelectFraction, double, 0, 1);
  itkGetMacro(SelectFraction, double);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(ImageDimensionShouldBe3, (Concept::SameDimension<TImage::ImageDimension, 3u>));
  itkConceptMacro(MaskDimensionShouldBe3, (Concept::SameDimension<TMask::ImageDimension, 3u>));
  itkConceptMacro(PointDimensionShouldBe3, (Concept::SameDimension<TFeatures::PointType::PointDimension, 3u>));
  // End concept checking
#endif

protected:
  MaskFeaturePointSelectionFilter();
  ~MaskFeaturePointSelectionFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

  /** Compute the connectivity offsets so that points can be excluded during
   * the execution of the filter. This method must be called after invoking
   * SetNonConnectivity().
   */
  void
  ComputeConnectivityOffsets();

private:
  unsigned                m_NonConnectivity;
  std::vector<OffsetType> m_NonConnectivityOffsets;
  SizeType                m_BlockRadius;
  double                  m_SelectFraction;
  bool                    m_ComputeStructureTensors;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkMaskFeaturePointSelectionFilter.hxx"
#endif

#endif
