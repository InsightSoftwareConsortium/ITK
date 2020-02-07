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
#ifndef itkImageToParametricSpaceFilter_h
#define itkImageToParametricSpaceFilter_h

#include "itkImageToMeshFilter.h"

namespace itk
{
/** \class ImageToParametricSpaceFilter
 * \brief Generate a mesh of parametric space from input images.
 *
 * ImageToParametricSpaceFilter takes a three Images of equal dimension and
 * size and generates from them a Mesh.
 *
 * The mesh contains one point for every pixel on the images. The
 * coordinate of the point being equal to the gray level of the
 * associated input pixels.
 *
 * This class is intended to produce the population of points that
 * represent samples in a parametric space. In this particular case
 * the parameters are the gray levels of the input images
 *
 * The dimension of the mesh points should be equal to the number
 * of input images to this filter.
 *
 * \ingroup ImageFilters
 * \ingroup ITKMesh
 */
template <typename TInputImage, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT ImageToParametricSpaceFilter : public ImageToMeshFilter<TInputImage, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToParametricSpaceFilter);

  /** Standard class type aliases. */
  using Self = ImageToParametricSpaceFilter;
  using Superclass = ImageToMeshFilter<TInputImage, TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToParametricSpaceFilter, ImageToMeshFilter);

  /** Some type alias associated with the input images. */
  using InputImageType = TInputImage;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  /** Some type alias associated with the output mesh. */
  using OutputMeshType = TOutputMesh;
  using PointType = typename OutputMeshType::PointType;
  using OutputMeshPointer = typename OutputMeshType::Pointer;
  using PointsContainer = typename OutputMeshType::PointsContainer;
  using PointIdentifier = typename OutputMeshType::PointIdentifier;
  using PointsContainerPointer = typename PointsContainer::Pointer;
  using PointsContainerIterator = typename PointsContainer::Iterator;
  using PointDataContainer = typename OutputMeshType::PointDataContainer;
  using PointDataContainerPointer = typename PointDataContainer::Pointer;
  using PointDataContainerIterator = typename PointDataContainer::Iterator;

  /** The dimension of the output mesh. */
  static constexpr unsigned int PointDimension = TOutputMesh::PointDimension;

  /** Some type alias associated with the output mesh. */
  void
  GenerateData() override;

  /** Prepare the output. */
  void
  GenerateOutputInformation() override;

  /** Select if the indices of input image pixels will be
   * stored as data at each one of the mesh points.
   * That assumes that the type of PointData in the output
   * mesh is capable of accepting an itk::Index through
   * an operator=(). Default value = true */
  itkSetMacro(ComputeIndices, bool);

protected:
  ImageToParametricSpaceFilter();
  ~ImageToParametricSpaceFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** This variable defines if the indices of input image pixels
   * will be stored as Data at each one of the mesh points. */
  bool m_ComputeIndices;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageToParametricSpaceFilter.hxx"
#endif

#endif
