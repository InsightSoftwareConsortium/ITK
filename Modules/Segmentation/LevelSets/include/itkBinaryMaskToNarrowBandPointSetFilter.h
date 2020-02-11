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
#ifndef itkBinaryMaskToNarrowBandPointSetFilter_h
#define itkBinaryMaskToNarrowBandPointSetFilter_h

#include "itkImageToMeshFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkReinitializeLevelSetImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"

namespace itk
{
/** \class BinaryMaskToNarrowBandPointSetFilter
 * \brief Generate a PointSet containing the narrow band around the edges of a
 * input binary image.
 *
 * BinaryMaskToNarrowBandPointSetFilter takes a binary image as input
 * and generates a PointSet as output. The point set contains
 * points around the contours of the binary mask in the image.
 * The pixel values of the point set are obtained as the distances
 * from the point to the edge of the binary mask.
 *
 * This filter is intended to be used for initializing the process
 * of NarrowBand-to-Image Registration.
 *
 * The filter is templated over the input image type and the
 * output mesh type. The only restriction is that the dimension
 * of points in the mesh should be equal to the input image dimension.
 * The PixelType in the mesh should be capable to represent distance
 * values.
 *
 * \sa ReinitializeImageFilter
 * \sa PointSetToImageRegistrationMethod
 *
 * \ingroup ImageFilters  MeshFilters
 * \ingroup ITKLevelSets
 */
template <typename TInputImage, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT BinaryMaskToNarrowBandPointSetFilter : public ImageToMeshFilter<TInputImage, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryMaskToNarrowBandPointSetFilter);

  /** Standard class type aliases. */
  using Self = BinaryMaskToNarrowBandPointSetFilter;

  using Superclass = ImageToMeshFilter<TInputImage, TOutputMesh>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryMaskToNarrowBandPointSetFilter, ImageToMeshFilter);

  /** Some type alias associated with the input images. */
  using InputImageType = TInputImage;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  using InputImageIterator = ImageRegionConstIteratorWithIndex<InputImageType>;

  /** Some type alias associated with the output mesh. */
  using OutputMeshType = TOutputMesh;
  using PointType = typename OutputMeshType::PointType;
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using OutputMeshConstPointer = typename OutputMeshType::ConstPointer;
  using PointsContainer = typename OutputMeshType::PointsContainer;
  using PointIdentifier = typename OutputMeshType::PointIdentifier;
  using PointsContainerPointer = typename PointsContainer::Pointer;
  using PointsContainerIterator = typename PointsContainer::Iterator;
  using PointDataContainer = typename OutputMeshType::PointDataContainer;
  using PointDataContainerPointer = typename PointDataContainer::Pointer;
  using PointDataContainerIterator = typename PointDataContainer::Iterator;

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Float image type to be used by the ReinitializeLevelSet image filter */
  using RealImageType = itk::Image<float, Self::ImageDimension>;

  /** The ReinitializeLevelSet filter is used to evaluate the distance from
      every pixel to the border of the binary mask. It uses internally a
      FastMarching filter for propagating a from from the edges of the binary
      mask.  */
  using DistanceFilterType = ReinitializeLevelSetImageFilter<RealImageType>;
  using DistanceFilterPointer = typename DistanceFilterType::Pointer;
  using NodeContainerPointer = typename DistanceFilterType::NodeContainerPointer;
  using NodeContainer = typename DistanceFilterType::NodeContainer;
  using NodeType = typename NodeContainer::Element;

  /** The ReinitializeLevelSetImageFilter expect the input to be binary
      within the range [-0.5:0.5]. This filte will scale the input to
      fit in this range. */
  using RescaleFilterType = RescaleIntensityImageFilter<InputImageType, RealImageType>;

  using RescaleFilterPointer = typename RescaleFilterType::Pointer;

  /** The dimension of the output mesh. */
  static constexpr unsigned int PointDimension = TOutputMesh::PointDimension;

  /** accept the input image */
  using Superclass::SetInput;
  void
  SetInput(const InputImageType * inputImage);

  /** Set/Get the width of the narrowband. This is the
      maximum distance from the binary mask edges to
      the points in the narrow band. The full width of
      the full narrow band will be double of this value. */
  itkSetMacro(BandWidth, float);
  itkGetConstMacro(BandWidth, float);

protected:
  BinaryMaskToNarrowBandPointSetFilter();
  ~BinaryMaskToNarrowBandPointSetFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Some type alias associated with the output mesh. */
  void
  GenerateData() override;

  /** Some type alias associated with the output mesh. */
  void
  GenerateOutputInformation() override;

private:
  DistanceFilterPointer m_DistanceFilter;
  RescaleFilterPointer  m_RescaleFilter;

  float m_BandWidth;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryMaskToNarrowBandPointSetFilter.hxx"
#endif

#endif
