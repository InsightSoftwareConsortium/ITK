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
template< typename TInputImage, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT BinaryMaskToNarrowBandPointSetFilter:
  public ImageToMeshFilter< TInputImage, TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef BinaryMaskToNarrowBandPointSetFilter Self;

  typedef ImageToMeshFilter< TInputImage, TOutputMesh >  Superclass;

  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryMaskToNarrowBandPointSetFilter, ImageToMeshFilter);

  /** Some typedefs associated with the input images. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  typedef ImageRegionConstIteratorWithIndex< InputImageType > InputImageIterator;

  /** Some typedefs associated with the output mesh. */
  typedef TOutputMesh                                 OutputMeshType;
  typedef typename OutputMeshType::PointType          PointType;
  typedef typename OutputMeshType::Pointer            OutputMeshPointer;
  typedef typename OutputMeshType::ConstPointer       OutputMeshConstPointer;
  typedef typename OutputMeshType::PointsContainer    PointsContainer;
  typedef typename OutputMeshType::PointIdentifier    PointIdentifier;
  typedef typename PointsContainer::Pointer           PointsContainerPointer;
  typedef typename PointsContainer::Iterator          PointsContainerIterator;
  typedef typename OutputMeshType::PointDataContainer PointDataContainer;
  typedef typename PointDataContainer::Pointer        PointDataContainerPointer;
  typedef typename PointDataContainer::Iterator       PointDataContainerIterator;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Float image type to be used by the ReinitializeLevelSet image filter */
  typedef itk::Image< float,
                      itkGetStaticConstMacro(ImageDimension) >   RealImageType;

  /** The ReinitializeLevelSet filter is used to evaluate the distance from
      every pixel to the border of the binary mask. It uses internally a
      FastMarching filter for propagating a from from the edges of the binary
      mask.  */
  typedef ReinitializeLevelSetImageFilter< RealImageType >  DistanceFilterType;
  typedef typename DistanceFilterType::Pointer              DistanceFilterPointer;
  typedef typename DistanceFilterType::NodeContainerPointer NodeContainerPointer;
  typedef typename DistanceFilterType::NodeContainer        NodeContainer;
  typedef typename NodeContainer::Element                   NodeType;

  /** The ReinitializeLevelSetImageFilter expect the input to be binary
      within the range [-0.5:0.5]. This filte will scale the input to
      fit in this range. */
  typedef RescaleIntensityImageFilter< InputImageType, RealImageType > RescaleFilterType;

  typedef typename RescaleFilterType::Pointer RescaleFilterPointer;

  /** The dimension of the output mesh. */
  itkStaticConstMacro(PointDimension, unsigned int,
                      TOutputMesh::PointDimension);

  /** Some typedefs associated with the output mesh. */
  void GenerateData(void) ITK_OVERRIDE;

  /** Some typedefs associated with the output mesh. */
  void GenerateOutputInformation(void) ITK_OVERRIDE;

  /** accept the input image */
  using Superclass::SetInput;
  void SetInput(const InputImageType *inputImage);

  /** Set/Get the width of the narrowband. This is the
      maximum distance from the binary mask edges to
      the points in the narrow band. The full width of
      the full narrow band will be double of this value. */
  itkSetMacro(BandWidth, float);
  itkGetConstMacro(BandWidth, float);

protected:
  BinaryMaskToNarrowBandPointSetFilter();
  ~BinaryMaskToNarrowBandPointSetFilter() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryMaskToNarrowBandPointSetFilter);

  DistanceFilterPointer m_DistanceFilter;
  RescaleFilterPointer  m_RescaleFilter;

  float m_BandWidth;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryMaskToNarrowBandPointSetFilter.hxx"
#endif

#endif
