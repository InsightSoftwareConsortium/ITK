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
template< typename TInputImage, typename TOutputMesh >
class ITK_TEMPLATE_EXPORT ImageToParametricSpaceFilter:
  public ImageToMeshFilter< TInputImage, TOutputMesh >
{
public:
  /** Standard class typedefs. */
  typedef ImageToParametricSpaceFilter                  Self;
  typedef ImageToMeshFilter< TInputImage, TOutputMesh > Superclass;
  typedef SmartPointer< Self >                          Pointer;
  typedef SmartPointer< const Self >                    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToParametricSpaceFilter, ImageToMeshFilter);

  /** Some typedefs associated with the input images. */
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
  itkStaticConstMacro(PointDimension, unsigned int,
                      TOutputMesh::PointDimension);

  /** Some typedefs associated with the output mesh. */
  void GenerateData() ITK_OVERRIDE;

  /** Prepare the output. */
  void GenerateOutputInformation(void) ITK_OVERRIDE;

  /** Select if the indices of input image pixels will be
   * stored as data at each one of the mesh points.
   * That assumes that the type of PointData in the output
   * mesh is capable of accepting an itk::Index through
   * an operator=(). Default value = true */
  itkSetMacro(ComputeIndices, bool);

protected:
  ImageToParametricSpaceFilter();
  ~ImageToParametricSpaceFilter() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ImageToParametricSpaceFilter);

  /** This variable defines if the indices of input image pixels
   * will be stored as Data at each one of the mesh points. */
  bool m_ComputeIndices;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToParametricSpaceFilter.hxx"
#endif

#endif
