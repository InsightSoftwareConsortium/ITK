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
#ifndef itkImageToPointSetFilter_h
#define itkImageToPointSetFilter_h

#include "itkImageToMeshFilter.h"

namespace itk
{
/** \class ImageToPointSetFilter
 *
 * \brief Convert an Image to a PointSet.
 *
 * This class provides default where every pixel in an Image is converted to a
 * point in the output PointSet.
 *
 * This class inherits from ImageToMeshFilter, but no topological information is
 * added to the mesh.
 *
 * \ingroup MeshToPolyData
 */
template< typename TInputImage, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT ImageToPointSetFilter: public ImageToMeshFilter< TInputImage, TOutputMesh >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToPointSetFilter);

  /** Standard class type alias. */
  using Self = ImageToPointSetFilter;
  using Superclass = ImageToMeshFilter< TInputImage, TOutputMesh >;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToPointSetFilter, ImageToMeshFilter);

  /** Some convenient type alias. */
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
  itkStaticConstMacro(PointDimension, unsigned int, TOutputMesh::PointDimension);

  /** ImageDimension constant */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

protected:
  ImageToPointSetFilter() {}
  virtual ~ImageToPointSetFilter() {}

  void GenerateData() override;

private:
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToPointSetFilter.hxx"
#endif

#endif
