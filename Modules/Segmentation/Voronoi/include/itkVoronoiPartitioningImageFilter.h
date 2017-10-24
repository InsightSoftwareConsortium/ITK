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
#ifndef itkVoronoiPartitioningImageFilter_h
#define itkVoronoiPartitioningImageFilter_h

#include "itkVoronoiSegmentationImageFilterBase.h"

namespace itk
{
/** \class VoronoiPartitioningImageFilter
 *
 * Perform a partitioning of 2D images (single channel) by Voronoi Diagram.
 *
 * \ingroup HybridSegmentation
 * \ingroup ITKVoronoi
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT VoronoiPartitioningImageFilter:
  public VoronoiSegmentationImageFilterBase< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef VoronoiPartitioningImageFilter                                  Self;
  typedef VoronoiSegmentationImageFilterBase< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                            Pointer;
  typedef SmartPointer< const Self >                                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VoronoiPartitioningImageFilter,
               VoronoiSegmentationImageFilterBase);

  /** Convenient typedefs. */
  typedef typename Superclass::BinaryObjectImage BinaryObjectImage;
  typedef typename Superclass::IndexList         IndexList;
  typedef typename Superclass::IndexType         IndexType;
  typedef typename Superclass::RegionType        RegionType;
  typedef typename Superclass::InputImageType    InputImageType;
  typedef typename Superclass::OutputImageType   OutputImageType;
  typedef typename Superclass::OutputPixelType   OutputPixelType;

  typedef typename Superclass::PointType          PointType;
  typedef typename Superclass::PointTypeDeque     PointTypeDeque;
  typedef typename Superclass::PointIdIterator    PointIdIterator;
  typedef typename Superclass::CellAutoPointer    CellAutoPointer;
  typedef typename Superclass::EdgeIterator       EdgeIterator;
  typedef typename Superclass::NeighborIdIterator NeighborIdIterator;

  /** Create the output results.  */
  virtual void MakeSegmentBoundary(void) ITK_OVERRIDE;

  virtual void MakeSegmentObject(void) ITK_OVERRIDE;

  /** Set/Get the threshold used to determine if a Voronoi region is
   * homogeneous. If the standard deviation of the intensities in the
   * Voronoi region is below this threshold, then the region is
   * considered homogeneous. */
  itkSetMacro(SigmaThreshold, double);
  itkGetConstMacro(SigmaThreshold, double);

  /** ImageDimension enumeration   */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< InputImageDimension, OutputImageDimension > ) );
  itkConceptMacro( IntConvertibleToOutputCheck,
                   ( Concept::Convertible< int, OutputPixelType > ) );
  // End concept checking
#endif

protected:
  VoronoiPartitioningImageFilter();
  ~VoronoiPartitioningImageFilter() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  // Classify all the voronoi cells as interior , exterior or boundary.
  virtual void ClassifyDiagram(void) ITK_OVERRIDE;

  // Generate the seeds to be added by dividing the boundary cells.
  virtual void GenerateAddingSeeds(void) ITK_OVERRIDE;

  // Are the pixels specified in the index list homogeneous?
  virtual bool TestHomogeneity(IndexList & Plist) ITK_OVERRIDE;

  // Threshold for homogeneity criterion
  double m_SigmaThreshold;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VoronoiPartitioningImageFilter);
};
} //end namespace

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVoronoiPartitioningImageFilter.hxx"
#endif

#endif
