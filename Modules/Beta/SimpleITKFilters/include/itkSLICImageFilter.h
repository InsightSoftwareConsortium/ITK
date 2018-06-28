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
#ifndef itkSLICImageFilter_h
#define itkSLICImageFilter_h

#include "itkImageToImageFilter.h"

#include "itkBarrier.h"


namespace itk
{

/** \class SLICImageFilter
 * \brief Simple Linear Iterative Clustering (SLIC) super-pixel segmentation
 *
 * The Simple Linear Iterative Clustering (SLIC) algorithm groups
 * pixels into a set of labeled regions or super-pixels. Super-pixels
 * follow natural image boundaries, are compact, and are nearly uniform
 * regions which can be used as a larger primitive for more efficient
 * computation. The SLIC algorithm can be viewed as a spatially
 * constrained iterative k-means method.
 *
 * The original algorithm was designed to cluster on the joint
 * domain of the images index space and it's CIELAB color space. This
 * implementation works with images of arbitrary dimension
 * as well as scalar, single channel, images and most multi-component image
 * types including ITK's arbitrary length VectorImage.
 *
 * The distance between a pixel and a cluster is the sum of squares of
 * the difference between their joint range and domains ( index and
 * value ). The computation is done in index space with scales
 * provided by the SpatialProximityWeight parameters.
 *
 * The output is a label image with each label representing a
 * superpixel cluster. Every pixel in the output is labeled, and the
 * starting label id is zero.
 *
 * This code was contributed in the Insight Journal paper:
 * "Scalable Simple Linear Iterative Clustering (SSLIC) Using a
 * Generic and Parallel Approach" by Lowekamp B. C., Chen D. T., Yaniv Z.
 *
 * \ingroup Segmentation SimpleITKFilters MultiThreading
 */
template < typename TInputImage, typename TOutputImage, typename TDistancePixel = float>
class SLICImageFilter:
    public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SLICImageFilter);

  /** Standard class type aliases. */
  typedef SLICImageFilter                                 Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SLICImageFilter, ImageToImageFilter);

  /** ImageDimension constants */
  static const unsigned int ImageDimension = TInputImage::ImageDimension;


  /** Image type information. */
  typedef TInputImage                         InputImageType;
  typedef typename InputImageType::PixelType  InputPixelType;
  typedef TOutputImage                        OutputImageType;
  typedef typename OutputImageType::PixelType OutputPixelType;
  typedef TDistancePixel                      DistanceType;
  typedef Image<DistanceType, ImageDimension> DistanceImageType;

  typedef typename InputImageType::IndexType IndexType;
  typedef typename InputImageType::PointType PointType;

  typedef ContinuousIndex<typename PointType::ValueType, ImageDimension> ContinuousIndexType;

  typedef double                               ClusterComponentType;
  typedef vnl_vector_ref<ClusterComponentType> ClusterType;

  typedef typename OutputImageType::RegionType OutputImageRegionType;

  typedef FixedArray< unsigned int, ImageDimension > SuperGridSizeType;

  /** \brief The spatial weight for the distance function.
   *
   * Increasing this value makes the superpixel shape more regular,
   * but more varied in image values. The range of the pixel values
   * and image dimension can effect the appropriate value.
   */
  itkSetMacro( SpatialProximityWeight, double );
  itkGetConstMacro( SpatialProximityWeight, double );

  /** \brief Number of iterations to run
   *
   * Specify the number of iterations to run when optimizing the clusters.
   */
  itkSetMacro( MaximumNumberOfIterations, unsigned int );
  itkGetConstMacro( MaximumNumberOfIterations, unsigned int );

  /** \brief The expected superpixel size and shape
   *
   * The requested size of a superpixel used to form a regular grid for
   * initialization  and limits the search space for pixels. The size
   * may be set anisotropically to provide a directional bias. This
   * may be set to reflect spacing of this image.
   */
  itkSetMacro(SuperGridSize, SuperGridSizeType);
  itkGetConstMacro(SuperGridSize, SuperGridSizeType);
  void SetSuperGridSize(unsigned int factor);
  void SetSuperGridSize(unsigned int i, unsigned int factor);

  /** \brief Enable perturbation of initial cluster center location
   *
   * After grid based initialization, this option enables moving the
   * initial cluster center location to the minimum gradient in a small
   * neighborhood. If the grid size is less than three this is
   * automatically disabled.
   */
  itkSetMacro(InitializationPerturbation, bool);
  itkGetMacro(InitializationPerturbation, bool);
  itkBooleanMacro(InitializationPerturbation);


  /** \brief Post processing step to enforce superpixel morphology.
   *
   * Enable an additional computation which ensures all label pixels of
   * the same value are spatially connected. Disconnected labeled components are
   * assigned a new value if of sufficient size, or are relabeled to
   * the previously encountered value if small.
   */
  itkSetMacro(EnforceConnectivity, bool);
  itkGetMacro(EnforceConnectivity, bool);
  itkBooleanMacro(EnforceConnectivity);


  /** \brief Get the current average cluster residual.
   *
   * After each iteration the residual is computed as the distance
   * between the current clusters and the previous. This is averaged
   * so that the value is independent of the number of clusters.
   */
  itkGetConstMacro( AverageResidual, double );

protected:
  SLICImageFilter();
  ~SLICImageFilter() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate full output and require full input */
  void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

  void BeforeThreadedGenerateData() ITK_OVERRIDE;

  void ThreadedUpdateDistanceAndLabel(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId);

  void ThreadedUpdateClusters(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId);

  void ThreadedPerturbClusters(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId);

  void ThreadedConnectivity(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId);

  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;


  void AfterThreadedGenerateData() ITK_OVERRIDE;

  DistanceType Distance(const ClusterType &cluster1, const ClusterType &cluster2);

  DistanceType Distance(const ClusterType &cluster, const InputPixelType &v, const PointType &pt);

private:

  SuperGridSizeType m_SuperGridSize;
  unsigned int      m_MaximumNumberOfIterations;
  double            m_SpatialProximityWeight;

  FixedArray<double,ImageDimension> m_DistanceScales;
  std::vector<ClusterComponentType> m_Clusters;
  std::vector<ClusterComponentType> m_OldClusters;


  void RelabelConnectedRegion( const IndexType &seed,
                               OutputPixelType requiredLabel,
                               OutputPixelType outputLabel,
                               std::vector<IndexType> & indexStack);

  struct UpdateCluster
  {
    size_t count;
    vnl_vector<ClusterComponentType> cluster;
  };

  typedef  std::map<size_t, UpdateCluster> UpdateClusterMap;

  typedef Image<unsigned char, ImageDimension> MarkerImageType;

  std::vector<UpdateClusterMap> m_UpdateClusterPerThread;

  typename Barrier::Pointer           m_Barrier;
  typename DistanceImageType::Pointer m_DistanceImage;
  typename MarkerImageType::Pointer   m_MarkerImage;

  bool m_EnforceConnectivity;

  bool m_InitializationPerturbation;

  double m_AverageResidual;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSLICImageFilter.hxx"
#endif

#endif //itkSLICImageFilter_h
