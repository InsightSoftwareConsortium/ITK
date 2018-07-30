/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
 * \ingroup BSplineGradient
 */
template <typename TInputImage, typename TOutputMesh>
class ITK_TEMPLATE_EXPORT ImageToPointSetFilter : public ImageToMeshFilter<TInputImage, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToPointSetFilter);

  /** Standard class typedefs. */
  typedef ImageToPointSetFilter                       Self;
  typedef ImageToMeshFilter<TInputImage, TOutputMesh> Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToPointSetFilter, ImageToMeshFilter);

  /** Some convenient typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  /** Some typedefs associated with the output mesh. */
  typedef TOutputMesh                                 OutputMeshType;
  typedef typename OutputMeshType::PointType          PointType;
  typedef typename OutputMeshType::Pointer            OutputMeshPointer;
  typedef typename OutputMeshType::PointsContainer    PointsContainer;
  typedef typename OutputMeshType::PointIdentifier    PointIdentifier;
  typedef typename PointsContainer::Pointer           PointsContainerPointer;
  typedef typename PointsContainer::Iterator          PointsContainerIterator;
  typedef typename OutputMeshType::PointDataContainer PointDataContainer;
  typedef typename PointDataContainer::Pointer        PointDataContainerPointer;
  typedef typename PointDataContainer::Iterator       PointDataContainerIterator;

  /** The dimension of the output mesh. */
  itkStaticConstMacro(PointDimension, unsigned int, TOutputMesh::PointDimension);

  /** ImageDimension constant */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

protected:
  ImageToPointSetFilter() {}
  virtual ~ImageToPointSetFilter() {}

  void
  GenerateData() override;

private:
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageToPointSetFilter.hxx"
#endif

#endif
