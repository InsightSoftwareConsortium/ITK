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
#ifndef itkBlockMatchingImageFilter_h
#define itkBlockMatchingImageFilter_h

#include "itkMeshToMeshFilter.h"
#include "itkImage.h"
#include "itkPointSet.h"
#include "itkVector.h"
#include "itkDefaultDynamicMeshTraits.h"


namespace itk
{
/** \class BlockMatchingImageFilter
 * \brief Computes displacements of given points from a fixed image in a
 * floating image.
 *
 * BlockMatchingImageFilter takes fixed and moving Images as well as
 * PointSet of feature points as inputs. Physical coordinates of feature
 * points are stored as point coordinates. Points of the input point set
 * must have unique identifiers within range 0..N-1, where N is the number
 * of points. Pixels (pointData) of input point set are not used.
 * Additionally, by default, feature points are expected to lie at least
 * (SearchRadius + BlockRadius) voxels from a boundary. This is usually
 * achieved by using an appropriate mask during selection of feature points.
 * If you are unsure whether feature points satisfy the above condition set
 * CheckBoundary flag to true which turns on boundary checks.
 * The default output(0) is a PointSet with displacements stored as vectors.
 * Additional output(1) is a PointSet containing similarities. Similarities
 * are needed to compute displacements and are always computed. The number
 * of points in the output PointSet is equal to the number of points in the
 * input PointSet.
 *
 * The filter is templated over fixed Image, moving Image, input PointSet,
 * output displacements PointSet and output similarities PointSet.
 *
 * This filter is intended to be used in the process of Physics-Based
 * Non-Rigid Registration. It computes displacement for selected points based
 * on similarity [M. Bierling, Displacement estimation by hierarchical block
 * matching, Proc. SPIE Vis. Comm. and Image Proc., vol. 1001, pp. 942-951,
 * 1988.].
 *
 * \author Andriy Kot, Center for Real-Time Computing, Old Dominion University,
 * Norfolk, VA
 *
 * \sa MaskFeaturePointSelectionFilter
 *
 * \ingroup ITKRegistrationCommon
 *
 * \sphinx
 * \sphinxexample{Registration/Common/MatchFeaturePoints,Match Feature Points}
 * \endsphinx
 */

template <
  typename TFixedImage,
  typename TMovingImage = TFixedImage,
  typename TFeatures = PointSet<Matrix<SpacePrecisionType, TFixedImage::ImageDimension, TFixedImage::ImageDimension>,
                                TFixedImage::ImageDimension>,
  class TDisplacements =
    PointSet<Vector<typename TFeatures::PointType::ValueType, TFeatures::PointDimension>, TFeatures::PointDimension>,
  class TSimilarities = PointSet<SpacePrecisionType, TDisplacements::PointDimension>>
class ITK_TEMPLATE_EXPORT BlockMatchingImageFilter : public MeshToMeshFilter<TFeatures, TDisplacements>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BlockMatchingImageFilter);

  static constexpr unsigned ImageDimension = TFixedImage::ImageDimension;

  /** Not input specific type alias */
  using ImageRegionType = ImageRegion<ImageDimension>;
  using ImageSizeType = Size<ImageDimension>;
  using ImageIndexType = Index<ImageDimension>;

  /** Fixed image type alias. */
  using FixedImageType = TFixedImage;
  using FixedImageConstPointer = typename FixedImageType::ConstPointer;
  using FixedImagePixelType = typename FixedImageType::PixelType;

  /** Moving image type alias. */
  using MovingImageType = TMovingImage;
  using MovingImageConstPointer = typename MovingImageType::ConstPointer;

  /** Feature points pointset type alias. */
  using FeaturePointsType = TFeatures;
  using FeaturePointsPointer = typename FeaturePointsType::Pointer;
  using FeaturePointsConstPointer = typename FeaturePointsType::ConstPointer;
  using FeaturePointsPhysicalCoordinates = typename FeaturePointsType::PointType;

  /** Displacement vectors type alias. */
  using DisplacementsType = TDisplacements;
  using DisplacementsPointer = typename DisplacementsType::Pointer;
  using DisplacementsConstPointer = typename DisplacementsType::ConstPointer;
  using DisplacementsVector = typename DisplacementsType::PixelType;

  /** Displacement similarities type alias. */
  using SimilaritiesType = TSimilarities;
  using SimilaritiesPointer = typename SimilaritiesType::Pointer;
  using SimilaritiesConstPointer = typename SimilaritiesType::ConstPointer;
  using SimilaritiesValue = typename SimilaritiesType::PixelType;

  /** Standard class type aliases. */
  using Superclass = MeshToMeshFilter<FeaturePointsType, DisplacementsType>;
  using Self = BlockMatchingImageFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BlockMatchingImageFilter, MeshToMeshFilter);

  /** set/get half size */
  itkSetMacro(BlockRadius, ImageSizeType);
  itkGetConstMacro(BlockRadius, ImageSizeType);

  /** set/get half window */
  itkSetMacro(SearchRadius, ImageSizeType);
  itkGetConstMacro(SearchRadius, ImageSizeType);

  /** set/get fixed image */
  itkSetInputMacro(FixedImage, FixedImageType);
  itkGetInputMacro(FixedImage, FixedImageType);

  /** set/get floating image */
  itkSetInputMacro(MovingImage, MovingImageType);
  itkGetInputMacro(MovingImage, MovingImageType);

  /** set/get point list */
  itkSetInputMacro(FeaturePoints, FeaturePointsType);
  itkGetInputMacro(FeaturePoints, FeaturePointsType);

  inline DisplacementsType *
  GetDisplacements()
  {
    return dynamic_cast<DisplacementsType *>(this->ProcessObject::GetOutput(0));
  }

  inline SimilaritiesType *
  GetSimilarities()
  {
    return dynamic_cast<SimilaritiesType *>(this->ProcessObject::GetOutput(1));
  }

protected:
  /** MakeOutput is provided for handling multiple outputs */
  using Superclass::MakeOutput;
  DataObject::Pointer
  MakeOutput(ProcessObject::DataObjectPointerArraySizeType idx) override;

  /** We need to create our own GenerateOutputInformation because the the
   * default version from ProcessObject result in a dynamic_cast of the input
   * pointer to the output pointer type in PointSet::CopyInformation.  This does
   * not work since they are different types. */
  void
  GenerateOutputInformation() override;

  /** We cannot stream (see comments in GenerateOutputInformation). */
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  /** Generate temporary containers to be used by individual threads exclusively */
  virtual void
  BeforeThreadedGenerateData();

  virtual void
  ThreadedGenerateData(ThreadIdType threadId);

  /** Compose pieces computed by each thread into a single output */
  virtual void
  AfterThreadedGenerateData();

  /** Start multithreader here since MeshToMesh filter does not provide multithreaded support */
  void
  GenerateData() override;

  BlockMatchingImageFilter();
  ~BlockMatchingImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Static function used as a "callback" by the MultiThreaderBase.  The threading
   * library will call this routine for each thread, which will delegate the
   * control to DynamicThreadedGenerateData(). */
  static ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
  ThreaderCallback(void * arg);

  /** Internal structure used for passing image data into the threading library
   */
  struct ThreadStruct
  {
    Pointer Filter;
  };

private:
  // algorithm parameters
  ImageSizeType m_BlockRadius;
  ImageSizeType m_SearchRadius;

  // temporary dynamic arrays for storing threads outputs
  SizeValueType         m_PointsCount;
  DisplacementsVector * m_DisplacementsVectorsArray;
  SimilaritiesValue *   m_SimilaritiesValuesArray;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBlockMatchingImageFilter.hxx"
#endif

#endif
