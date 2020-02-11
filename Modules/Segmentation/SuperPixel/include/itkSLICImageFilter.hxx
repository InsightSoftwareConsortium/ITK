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
#ifndef itkSLICImageFilter_hxx
#define itkSLICImageFilter_hxx

#include "itkSLICImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkImageRegionIterator.h"

#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageScanlineIterator.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkConstantBoundaryCondition.h"

#include "itkShrinkImageFilter.h"

#include "itkVariableLengthVector.h"

#include "itkPlatformMultiThreader.h"

#include "itkMath.h"

#include <numeric>


namespace itk
{

template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::SLICImageFilter()
  : m_MaximumNumberOfIterations((ImageDimension > 2) ? 5 : 10)
  , m_AverageResidual(NumericTraits<double>::max())
{
  this->DynamicMultiThreadingOff();
  this->SetMultiThreader(PlatformMultiThreader::New());

  m_SuperGridSize.Fill(50);
}

template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::SetSuperGridSize(unsigned int factor)
{
  unsigned int i;
  for (i = 0; i < ImageDimension; ++i)
  {
    if (factor != m_SuperGridSize[i])
    {
      break;
    }
  }
  if (i < ImageDimension)
  {
    this->Modified();
    m_SuperGridSize.Fill(factor);
  }
}

template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::SetSuperGridSize(unsigned int i, unsigned int factor)
{
  if (m_SuperGridSize[i] == factor)
  {
    return;
  }

  this->Modified();
  m_SuperGridSize[i] = factor;
}

template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "SuperGridSize: " << m_SuperGridSize << std::endl;
  os << indent << "MaximumNumberOfIterations: " << m_MaximumNumberOfIterations << std::endl;
  os << indent << "SpatialProximityWeight: " << m_SpatialProximityWeight << std::endl;
  os << indent << "EnforceConnectivity: " << m_EnforceConnectivity << std::endl;
  os << indent << "AverageResidual: " << m_AverageResidual << std::endl;
}

template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::EnlargeOutputRequestedRegion(DataObject * output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}


template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::BeforeThreadedGenerateData()
{
  itkDebugMacro("Starting BeforeThreadedGenerateData");

  typename InputImageType::Pointer inputImage = InputImageType::New();
  inputImage->Graft(const_cast<InputImageType *>(this->GetInput()));


  m_AverageResidual = NumericTraits<double>::max();

  itkDebugMacro("Shrinking Starting");
  typename InputImageType::Pointer shrunkImage;
  {
    using ShrinkImageFilterType = itk::ShrinkImageFilter<InputImageType, InputImageType>;
    typename ShrinkImageFilterType::Pointer shrinker = ShrinkImageFilterType::New();
    shrinker->SetInput(inputImage);
    shrinker->SetShrinkFactors(m_SuperGridSize);
    shrinker->UpdateLargestPossibleRegion();

    shrunkImage = shrinker->GetOutput();
  }
  itkDebugMacro("Shinking Completed");

  const typename InputImageType::RegionType region = inputImage->GetBufferedRegion();
  const unsigned int                        numberOfComponents = inputImage->GetNumberOfComponentsPerPixel();
  const unsigned int                        numberOfClusterComponents = numberOfComponents + ImageDimension;
  const size_t                              numberOfClusters = shrunkImage->GetBufferedRegion().GetNumberOfPixels();


  // allocate array of scalars
  m_Clusters.resize(numberOfClusters * numberOfClusterComponents);
  m_OldClusters.resize(numberOfClusters * numberOfClusterComponents);


  using InputConstIteratorType = ImageScanlineConstIterator<InputImageType>;

  InputConstIteratorType it(shrunkImage, shrunkImage->GetLargestPossibleRegion());

  // Initialize cluster centers
  size_t cnt = 0;
  while (!it.IsAtEnd())
  {
    const size_t ln = shrunkImage->GetLargestPossibleRegion().GetSize(0);
    for (unsigned x = 0; x < ln; ++x)
    {
      // construct vector as reference to the scalar array
      ClusterType cluster(numberOfClusterComponents, &m_Clusters[cnt * numberOfClusterComponents]);

      NumericTraits<InputPixelType>::AssignToArray(it.Get(), cluster);

      const IndexType &                  idx = it.GetIndex();
      typename InputImageType::PointType pt;
      shrunkImage->TransformIndexToPhysicalPoint(idx, pt);
      ContinuousIndexType cidx;
      inputImage->TransformPhysicalPointToContinuousIndex(pt, cidx);
      for (unsigned int i = 0; i < ImageDimension; ++i)
      {
        cluster[numberOfComponents + i] = cidx[i];
      }
      ++it;
      ++cnt;
    }
    it.NextLine();
  }
  itkDebugMacro("Initial Clustering Completed");

  shrunkImage = nullptr;

  m_DistanceImage = DistanceImageType::New();
  m_DistanceImage->CopyInformation(inputImage);
  m_DistanceImage->SetBufferedRegion(region);
  m_DistanceImage->Allocate();

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    m_DistanceScales[i] = m_SpatialProximityWeight / m_SuperGridSize[i];
  }


  m_UpdateClusterPerThread.clear();

  this->Superclass::BeforeThreadedGenerateData();
}


template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::ThreadedUpdateDistanceAndLabel(
  const OutputImageRegionType & outputRegionForThread)
{
  using InputConstIteratorType = ImageScanlineConstIterator<InputImageType>;
  using DistanceIteratorType = ImageScanlineIterator<DistanceImageType>;

  const InputImageType * inputImage = this->GetInput();
  OutputImageType *      outputImage = this->GetOutput();
  const unsigned int     numberOfComponents = inputImage->GetNumberOfComponentsPerPixel();
  const unsigned int     numberOfClusterComponents = numberOfComponents + ImageDimension;

  typename InputImageType::SizeType searchRadius;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    searchRadius[i] = m_SuperGridSize[i];
  }

  for (size_t i = 0; i * numberOfClusterComponents < m_Clusters.size(); ++i)
  {
    ClusterType                         cluster(numberOfClusterComponents, &m_Clusters[i * numberOfClusterComponents]);
    typename InputImageType::RegionType localRegion;
    typename InputImageType::PointType  pt;
    IndexType                           idx;

    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      idx[d] = Math::RoundHalfIntegerUp<IndexValueType>(cluster[numberOfComponents + d]);
    }

    localRegion.SetIndex(idx);
    localRegion.GetModifiableSize().Fill(1u);
    localRegion.PadByRadius(searchRadius);
    if (!localRegion.Crop(outputRegionForThread))
    {
      continue;
    }


    const size_t ln = localRegion.GetSize(0);

    InputConstIteratorType inputIter(inputImage, localRegion);
    DistanceIteratorType   distanceIter(m_DistanceImage, localRegion);


    while (!inputIter.IsAtEnd())
    {
      for (size_t x = 0; x < ln; ++x)
      {
        const IndexType & currentIdx = inputIter.GetIndex();

        pt = ContinuousIndexType(currentIdx);
        const double distance = this->Distance(cluster, inputIter.Get(), pt);
        if (distance < distanceIter.Get())
        {
          distanceIter.Set(distance);
          outputImage->SetPixel(currentIdx, i);
        }

        ++distanceIter;
        ++inputIter;
      }
      inputIter.NextLine();
      distanceIter.NextLine();
    }

    // for neighborhood iterator size S
  }
}


template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::ThreadedUpdateClusters(
  const OutputImageRegionType & updateRegionForThread)
{
  const InputImageType * inputImage = this->GetInput();
  OutputImageType *      outputImage = this->GetOutput();

  const unsigned int numberOfComponents = inputImage->GetNumberOfComponentsPerPixel();
  const unsigned int numberOfClusterComponents = numberOfComponents + ImageDimension;

  using InputConstIteratorType = ImageScanlineConstIterator<InputImageType>;
  using OutputIteratorType = ImageScanlineIterator<OutputImageType>;

  UpdateClusterMap clusterMap;

  itkDebugMacro("Estimating Centers");
  // calculate new centers
  OutputIteratorType     itOut = OutputIteratorType(outputImage, updateRegionForThread);
  InputConstIteratorType itIn = InputConstIteratorType(inputImage, updateRegionForThread);
  while (!itOut.IsAtEnd())
  {
    const size_t ln = updateRegionForThread.GetSize(0);
    for (unsigned x = 0; x < ln; ++x)
    {
      const IndexType &                         idx = itOut.GetIndex();
      const InputPixelType &                    v = itIn.Get();
      const typename OutputImageType::PixelType l = itOut.Get();

      std::pair<typename UpdateClusterMap::iterator, bool> r = clusterMap.insert(std::make_pair(l, UpdateCluster()));
      vnl_vector<ClusterComponentType> &                   cluster = r.first->second.cluster;
      if (r.second)
      {
        cluster.set_size(numberOfClusterComponents);
        cluster.fill(0.0);
        r.first->second.count = 0;
      }
      ++r.first->second.count;

      const typename NumericTraits<InputPixelType>::MeasurementVectorType & mv = v;
      for (unsigned int i = 0; i < numberOfComponents; ++i)
      {
        cluster[i] += mv[i];
      }

      for (unsigned int i = 0; i < ImageDimension; ++i)
      {
        cluster[numberOfComponents + i] += idx[i];
      }

      ++itIn;
      ++itOut;
    }
    itIn.NextLine();
    itOut.NextLine();
  }

  // TODO improve merge algoithm
  std::lock_guard<std::mutex> mutexHolder(m_Mutex);
  m_UpdateClusterPerThread.push_back(clusterMap);
}


template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::ThreadedPerturbClusters(SizeValueType clusterIndex)
{

  const InputImageType * inputImage = this->GetInput();

  const unsigned int numberOfComponents = inputImage->GetNumberOfComponentsPerPixel();
  const unsigned int numberOfClusterComponents = numberOfComponents + ImageDimension;


  itk::Size<ImageDimension> radius;
  radius.Fill(1);
  unsigned long center;
  unsigned long stride[ImageDimension];


  typename InputImageType::SizeType searchRadius;
  searchRadius.Fill(1);


  using NeighborhoodType = ConstNeighborhoodIterator<TInputImage>;

  // get center and dimension strides for iterator neighborhoods
  NeighborhoodType it(radius, inputImage, inputImage->GetLargestPossibleRegion());
  center = it.Size() / 2;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    stride[i] = it.GetStride(i);
  }


  const typename InputImageType::SpacingType spacing = inputImage->GetSpacing();

  using GradientType = typename NumericTraits<InputPixelType>::RealType;
  GradientType J[ImageDimension];

  // cluster is a reference to array
  ClusterType cluster(numberOfClusterComponents, &m_Clusters[clusterIndex * numberOfClusterComponents]);
  typename InputImageType::RegionType localRegion;
  IndexType                           idx;

  for (unsigned int d = 0; d < ImageDimension; ++d)
  {
    idx[d] = Math::RoundHalfIntegerUp<IndexValueType>(cluster[numberOfComponents + d]);
  }

  localRegion.SetIndex(idx);
  localRegion.GetModifiableSize().Fill(1u);
  localRegion.PadByRadius(searchRadius);


  it.SetRegion(localRegion);

  double minG = NumericTraits<double>::max();

  IndexType minIdx = idx;

  while (!it.IsAtEnd())
  {

    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      J[i] = it.GetPixel(center + stride[i]);
      J[i] -= it.GetPixel(center - stride[i]);
      J[i] /= 2.0 * spacing[i];
    }

    double gNorm = 0.0;

    // Compute some of squares over dimensions and components
    // (Frobenius norm of the Jacobian Matrix)
    for (unsigned int i = 0; i < ImageDimension; ++i)
    {
      // convert to a type that has the operator[], for scalars this
      // will be FixedArray, for VectorImages, this will be the same
      // type as the pixel and not conversion or allocation will occur.
      const typename NumericTraits<InputPixelType>::MeasurementVectorType & vG = J[i];
      for (unsigned int j = 0; j < numberOfComponents; ++j)
      {
        gNorm += vG[j] * vG[j];
      }
    }


    if (gNorm < minG)
    {
      minG = gNorm;
      minIdx = it.GetIndex();
    }
    ++it;
  }


  NumericTraits<InputPixelType>::AssignToArray(inputImage->GetPixel(minIdx), cluster);

  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    cluster[numberOfComponents + i] = minIdx[i];
  }
}


template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::ThreadedConnectivity(SizeValueType clusterIndex)
{
  itkDebugMacro("Threaded Connectivity");

  const InputImageType * inputImage = this->GetInput();
  OutputImageType *      outputImage = this->GetOutput();
  const unsigned int     numberOfComponents = inputImage->GetNumberOfComponentsPerPixel();
  const unsigned int     numberOfClusterComponents = numberOfComponents + ImageDimension;

  const size_t minSuperSize =
    std::accumulate(m_SuperGridSize.cbegin(), m_SuperGridSize.cend(), size_t(1), std::multiplies<size_t>()) / 4;

  ConstantBoundaryCondition<TOutputImage> lbc;
  lbc.SetConstant(NumericTraits<typename OutputImageType::PixelType>::max());

  itk::Size<ImageDimension> radius;
  radius.Fill(1);

  using NeighborhoodType = ConstNeighborhoodIterator<TOutputImage, ConstantBoundaryCondition<TOutputImage>>;

  std::vector<IndexType> indexStack;

  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    radius[j] = m_SuperGridSize[j] / 2;
  }

  // get center and dimension strides for iterator neighborhoods
  NeighborhoodType searchLabelIt(radius, outputImage, outputImage->GetLargestPossibleRegion());
  searchLabelIt.OverrideBoundaryCondition(&lbc);


  ClusterType cluster(numberOfClusterComponents, &m_Clusters[clusterIndex * numberOfClusterComponents]);
  typename InputImageType::RegionType localRegion;
  IndexType                           idx;

  for (unsigned int d = 0; d < ImageDimension; ++d)
  {
    idx[d] = Math::RoundHalfIntegerUp<IndexValueType>(cluster[numberOfComponents + d]);
  }


  if (outputImage->GetPixel(idx) != clusterIndex)
  {
    itkDebugMacro("Searching for cluster: " << clusterIndex << " near idx: " << idx);

    searchLabelIt.SetLocation(idx);
    size_t n = 0;
    for (; n < searchLabelIt.Size(); ++n)
    {
      if (searchLabelIt.GetPixel(n) == clusterIndex)
      {
        idx = searchLabelIt.GetIndex(n);

        itkDebugMacro("Non-Center does  match Id. @: " << idx << " for: " << clusterIndex);
        break;
      }
    }

    if (n >= searchLabelIt.Size())
    {
#if defined(DEBUG)
      itkWarningMacro("Failed to find cluster: " << clusterIndex << " in super grid size neighborhood!");
#endif
      return;
    }
  }

  this->RelabelConnectedRegion(idx, clusterIndex, clusterIndex, indexStack);

  if (indexStack.size() < minSuperSize)
  {
    // std::cout << "\tLabel is too small: " << indexStack.size() << std::endl;
    // The connected Superpixel is too small, so demark the marker image
    for (size_t indexStackDelabel = 0; indexStackDelabel < indexStack.size(); ++indexStackDelabel)
    {
      m_MarkerImage->SetPixel(indexStack[indexStackDelabel], 0);
    }
  }
}


template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::SingleThreadedConnectivity()
{
  itkDebugMacro("Single Threaded Connectivity");

  const InputImageType * inputImage = this->GetInput();
  OutputImageType *      outputImage = this->GetOutput();

  const unsigned int numberOfComponents = inputImage->GetNumberOfComponentsPerPixel();
  const unsigned int numberOfClusterComponents = numberOfComponents + ImageDimension;

  OutputPixelType nextLabel = m_Clusters.size() / numberOfClusterComponents;
  OutputPixelType prevLabel = m_Clusters.size() / numberOfClusterComponents;

  const size_t minSuperSize =
    std::accumulate(m_SuperGridSize.cbegin(), m_SuperGridSize.cend(), size_t(1), std::multiplies<size_t>()) / 4;

  std::vector<IndexType> indexStack;

  // Next we relabel the remaining regions ( defined by having the a
  // label id ) not connected to the SuperPixel centroids. If the
  // region is larger than the minimum superpixel size than it gets
  // a new label, otherwise it just gets the previously encountered
  // label id.


  using OutputIteratorType = ImageScanlineIterator<OutputImageType>;
  OutputIteratorType outputIter(outputImage, outputImage->GetRequestedRegion());

  using MarkerIteratorType = ImageScanlineIterator<MarkerImageType>;
  MarkerIteratorType markerIter(m_MarkerImage, outputImage->GetRequestedRegion());


  while (!markerIter.IsAtEnd())
  {
    while (!markerIter.IsAtEndOfLine())
    {
      if (markerIter.Get() == 0)
      {
        // try relabeling the connected component to the next label id
        this->RelabelConnectedRegion(markerIter.GetIndex(), outputIter.Get(), nextLabel, indexStack);

        if (indexStack.size() >= minSuperSize)
        {
          prevLabel = nextLabel++;
        }
        else
        {
          for (size_t indexStackDelabel = 0; indexStackDelabel < indexStack.size(); ++indexStackDelabel)
          {
            outputImage->SetPixel(indexStack[indexStackDelabel], prevLabel);
          }
        }
      }
      else
      {
        prevLabel = outputIter.Get();
      }
      ++markerIter;
      ++outputIter;
    }

    markerIter.NextLine();
    outputIter.NextLine();
  }
}


template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::GenerateData()
{
  this->AllocateOutputs();
  this->BeforeThreadedGenerateData();

  this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());

  const InputImageType * inputImage = this->GetInput();
  OutputImageType *      outputImage = this->GetOutput();

  const typename InputImageType::RegionType region = inputImage->GetBufferedRegion();
  const unsigned int                        numberOfComponents = inputImage->GetNumberOfComponentsPerPixel();
  const unsigned int                        numberOfClusterComponents = numberOfComponents + ImageDimension;
  const size_t                              numberOfClusters = m_Clusters.size() / numberOfClusterComponents;

  itkDebugMacro("Perturb cluster centers");
  bool doPerturbCluster = true;
  for (unsigned int i = 0; i < ImageDimension; ++i)
  {
    if (m_SuperGridSize[i] < 3)
    {
      doPerturbCluster = false;
      break;
    }
  }
  if (doPerturbCluster && m_InitializationPerturbation)
  {

    this->GetMultiThreader()->ParallelizeArray(
      0, numberOfClusters, [this](SizeValueType idx) { this->ThreadedPerturbClusters(idx); }, this);
  }

  itkDebugMacro("Entering Main Loop");
  for (unsigned int loopCnt = 0; loopCnt < m_MaximumNumberOfIterations; ++loopCnt)
  {
    itkDebugMacro("Iteration :" << loopCnt);

    m_DistanceImage->FillBuffer(NumericTraits<typename DistanceImageType::PixelType>::max());
    m_UpdateClusterPerThread.clear();

    this->GetMultiThreader()->template ParallelizeImageRegion<ImageDimension>(
      outputImage->GetRequestedRegion(),
      [this](const OutputImageRegionType & outputRegionForThread) {
        this->ThreadedUpdateDistanceAndLabel(outputRegionForThread);
      },
      this);


    this->GetMultiThreader()->template ParallelizeImageRegion<ImageDimension>(
      outputImage->GetRequestedRegion(),
      [this](const OutputImageRegionType & outputRegionForThread) {
        this->ThreadedUpdateClusters(outputRegionForThread);
      },
      this);

    // prepare to update clusters
    swap(m_Clusters, m_OldClusters);
    std::fill(m_Clusters.begin(), m_Clusters.end(), 0.0);
    std::vector<size_t> clusterCount(m_Clusters.size() / numberOfClusterComponents, 0);

    // reduce the produce cluster maps per-thread into m_Cluster array
    for (unsigned int i = 0; i < m_UpdateClusterPerThread.size(); ++i)
    {
      UpdateClusterMap & clusterMap = m_UpdateClusterPerThread[i];
      for (typename UpdateClusterMap::const_iterator clusterIter = clusterMap.begin(); clusterIter != clusterMap.end();
           ++clusterIter)
      {
        const size_t clusterIdx = clusterIter->first;
        clusterCount[clusterIdx] += clusterIter->second.count;

        ClusterType cluster(numberOfClusterComponents, &m_Clusters[clusterIdx * numberOfClusterComponents]);
        cluster += clusterIter->second.cluster;
      }
    }

    // average, l1
    double l1Residual = 0.0;
    for (size_t i = 0; i * numberOfClusterComponents < m_Clusters.size(); ++i)
    {

      ClusterType cluster(numberOfClusterComponents, &m_Clusters[i * numberOfClusterComponents]);
      cluster /= clusterCount[i];

      ClusterType oldCluster(numberOfClusterComponents, &m_OldClusters[i * numberOfClusterComponents]);
      l1Residual += Distance(cluster, oldCluster);
    }

    m_AverageResidual = std::sqrt(l1Residual) / m_Clusters.size();
    this->InvokeEvent(IterationEvent());

    // while error <= threshold
  }


  if (m_EnforceConnectivity)
  {

    m_DistanceImage = nullptr;

    m_MarkerImage = MarkerImageType::New();
    m_MarkerImage->CopyInformation(inputImage);
    m_MarkerImage->SetBufferedRegion(region);
    m_MarkerImage->Allocate();
    m_MarkerImage->FillBuffer(NumericTraits<typename MarkerImageType::PixelType>::Zero);


    this->GetMultiThreader()->ParallelizeArray(
      0, numberOfClusters, [this](SizeValueType idx) { this->ThreadedConnectivity(idx); }, this);
    this->SingleThreadedConnectivity();
  }


  this->AfterThreadedGenerateData();
}

template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::AfterThreadedGenerateData()
{
  itkDebugMacro("Starting AfterThreadedGenerateData");


  m_DistanceImage = nullptr;
  m_MarkerImage = nullptr;

  // cleanup
  std::vector<ClusterComponentType>().swap(m_Clusters);
  std::vector<ClusterComponentType>().swap(m_OldClusters);
  for (unsigned int i = 0; i < m_UpdateClusterPerThread.size(); ++i)
  {
    UpdateClusterMap().swap(m_UpdateClusterPerThread[i]);
  }
}


template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
typename SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::DistanceType
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::Distance(const ClusterType & cluster1,
                                                                     const ClusterType & cluster2)
{
  const unsigned int s = cluster1.size();
  DistanceType       d1 = 0.0;
  DistanceType       d2 = 0.0;
  unsigned int       i = 0;
  for (; i < s - ImageDimension; ++i)
  {
    const DistanceType d = (cluster1[i] - cluster2[i]);
    d1 += d * d;
  }

  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    const DistanceType d = (cluster1[i] - cluster2[i]) * m_DistanceScales[j];
    d2 += d * d;
    ++i;
  }
  return d1 + d2;
}

template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
typename SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::DistanceType
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::Distance(const ClusterType &    cluster,
                                                                     const InputPixelType & _v,
                                                                     const PointType &      pt)
{
  const unsigned int                                                    s = cluster.size();
  DistanceType                                                          d1 = 0.0;
  DistanceType                                                          d2 = 0.0;
  unsigned int                                                          i = 0;
  const typename NumericTraits<InputPixelType>::MeasurementVectorType & v = _v;
  for (; i < s - ImageDimension; ++i)
  {
    const DistanceType d = (cluster[i] - v[i]);
    d1 += d * d;
  }

  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    const DistanceType d = (cluster[i] - pt[j]) * m_DistanceScales[j];
    d2 += d * d;
    ++i;
  }
  return d1 + d2;
}


template <typename TInputImage, typename TOutputImage, typename TDistancePixel>
void
SLICImageFilter<TInputImage, TOutputImage, TDistancePixel>::RelabelConnectedRegion(const IndexType & seed,
                                                                                   OutputPixelType   requiredLabel,
                                                                                   OutputPixelType   outputLabel,
                                                                                   std::vector<IndexType> & indexStack)
{

  OutputImageType * outputImage = this->GetOutput();

  ConstantBoundaryCondition<TOutputImage> lbc;
  lbc.SetConstant(NumericTraits<typename OutputImageType::PixelType>::max());

  itk::Size<ImageDimension> radius;
  radius.Fill(1);
  unsigned long center;
  unsigned long stride[ImageDimension];

  using NeighborhoodType = NeighborhoodIterator<TOutputImage, ConstantBoundaryCondition<TOutputImage>>;

  NeighborhoodType labelIt(radius, outputImage, outputImage->GetRequestedRegion());
  labelIt.OverrideBoundaryCondition(&lbc);

  center = labelIt.Size() / 2;
  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    stride[j] = labelIt.GetStride(j);
  }

  using MarkerNeighborhoodType = NeighborhoodIterator<MarkerImageType>;
  MarkerNeighborhoodType markerIter(radius, m_MarkerImage, outputImage->GetRequestedRegion());

  indexStack.clear();
  indexStack.push_back(seed);
  m_MarkerImage->SetPixel(seed, 1);
  outputImage->SetPixel(seed, outputLabel);

  size_t indexStackCount = 0;
  while (indexStackCount < indexStack.size())
  {
    const IndexType & idx = indexStack[indexStackCount++];

    markerIter.SetLocation(idx);
    labelIt.SetLocation(idx);
    for (unsigned int j = 0; j < ImageDimension; ++j)
    {
      unsigned int nIdx = center + stride[j];

      if (markerIter.GetPixel(nIdx) == 0 && labelIt.GetPixel(nIdx) == requiredLabel)
      {
        indexStack.push_back(labelIt.GetIndex(nIdx));
        markerIter.SetPixel(nIdx, 1);
        labelIt.SetPixel(nIdx, outputLabel);
      }
      nIdx = center - stride[j];
      if (markerIter.GetPixel(nIdx) == 0 && labelIt.GetPixel(nIdx) == requiredLabel)
      {
        indexStack.push_back(labelIt.GetIndex(nIdx));
        markerIter.SetPixel(nIdx, 1);
        labelIt.SetPixel(nIdx, outputLabel);
      }
    }
  }
}

} // end namespace itk

#endif // itkSLICImageFilter_hxx
