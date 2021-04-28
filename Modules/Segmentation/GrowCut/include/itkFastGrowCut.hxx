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

#ifndef itkFastGrowCut_hxx
#define itkFastGrowCut_hxx

#include "itkFastGrowCut.h"

#include "itkPrintHelper.h"
#include "itkImageMaskSpatialObject.h"

namespace
{
template <typename PixelType>
void
ExtractITKImageROI(const itk::Image<PixelType, 3> * im,
                   const std::vector<long> &        imROI,
                   std::vector<PixelType> &         imROIVec)
{

  // Copy itk image ROI to vector
  typedef itk::Image<PixelType, 3> ImageType;
  typename ImageType::IndexType    index;
  long                             i, j, k, kk, DIMXYZ;

  DIMXYZ = (imROI[3] - imROI[0] + 1) * (imROI[4] - imROI[1] + 1) * (imROI[5] - imROI[2] + 1);
  imROIVec.clear();
  imROIVec.resize(DIMXYZ);
  kk = 0;
  for (k = imROI[2]; k <= imROI[5]; k++)
    for (j = imROI[1]; j <= imROI[4]; j++)
      for (i = imROI[0]; i <= imROI[3]; i++)
      {
        index[0] = i;
        index[1] = j;
        index[2] = k;
        imROIVec[kk++] = im->GetPixel(index);
      }
}
} // namespace

namespace itk
{
template <typename TInputImage, typename TLabelImage, typename TMaskImage>
bool
FastGrowCut<TInputImage, TLabelImage, TMaskImage>::InitializationAHP()
{
  // Release memory before reallocating
  if (m_Heap != nullptr)
  {
    delete m_Heap;
    m_Heap = nullptr;
  }
  if (m_HeapNodes != nullptr)
  {
    delete[] m_HeapNodes;
    m_HeapNodes = nullptr;
  }

  auto seedLabelVolume = this->GetSeedImage();
  auto resultLabelVolume = this->GetOutput();
  auto maskLabelVolume = this->GetMaskImage();

  RegionType region = resultLabelVolume->GetRequestedRegion();

  NodeIndexType m_DimX = region.GetSize(0);
  NodeIndexType m_DimY = region.GetSize(1);
  NodeIndexType m_DimZ = region.GetSize(2);
  NodeIndexType dimXYZ = m_DimX * m_DimY * m_DimZ;

  m_HeapNodes = new FibHeapNode[dimXYZ + 1]; // size is +1 for storing the zeroValueElement
  if (m_HeapNodes == nullptr)
  {
    itkExceptionMacro("Memory allocation failed. Dimensions: " << m_DimX << "x" << m_DimY << "x" << m_DimZ);
  }

  m_Heap = new FibHeap;
  m_Heap->SetHeapNodes(m_HeapNodes);

  const LabelPixelType * seedLabelVolumePtr = seedLabelVolume->GetBufferPointer();

  if (!m_bSegInitialized)
  {
    m_DistanceVolume->SetOrigin(resultLabelVolume->GetOrigin());
    m_DistanceVolume->SetSpacing(resultLabelVolume->GetSpacing());
    m_DistanceVolume->SetRegions(region);
    m_DistanceVolume->SetLargestPossibleRegion(resultLabelVolume->GetLargestPossibleRegion());
    m_DistanceVolume->Allocate(true);
    LabelPixelType *   resultLabelVolumePtr = static_cast<LabelPixelType *>(resultLabelVolume->GetBufferPointer());
    NodeKeyValueType * distanceVolumePtr = static_cast<NodeKeyValueType *>(m_DistanceVolume->GetBufferPointer());

    // Compute index offset
    m_NeighborIndexOffsets.clear();
    m_NeighborDistancePenalties.clear();
    // Neighbors are traversed in the order of m_NeighborIndexOffsets,
    // therefore one would expect that the offsets should
    // be as continuous as possible (e.g., x coordinate
    // should change most quickly), but that resulted in
    // about 5-6% longer computation time. Therefore,
    // we put indices in order x1y1z1, x1y1z2, x1y1z3, etc.
    SpacingType spacing = seedLabelVolume->GetSpacing();
    for (long ix = -1; ix <= 1; ix++)
    {
      for (long iy = -1; iy <= 1; iy++)
      {
        for (long iz = -1; iz <= 1; iz++)
        {
          if (ix == 0 && iy == 0 && iz == 0)
          {
            continue;
          }
          m_NeighborIndexOffsets.push_back(ix + long(m_DimX) * (iy + long(m_DimY) * iz));
          m_NeighborDistancePenalties.push_back(this->m_DistancePenalty * sqrt((spacing[0] * ix) * (spacing[0] * ix) +
                                                                               (spacing[1] * iy) * (spacing[1] * iy) +
                                                                               (spacing[2] * iz) * (spacing[2] * iz)));
        }
      }
    }

    // Determine neighborhood size for computation at each voxel.
    // The neighborhood size is everywhere the same (size of m_NeighborIndexOffsets)
    // except at the edges of the volume, where the neighborhood size is 0.
    m_NumberOfNeighbors.resize(dimXYZ);
    const unsigned char numberOfNeighbors = static_cast<unsigned char>(m_NeighborIndexOffsets.size());
    unsigned char *     nbSizePtr = &(m_NumberOfNeighbors[0]);
    for (NodeIndexType z = 0; z < m_DimZ; z++)
    {
      bool zEdge = (z == 0 || z == m_DimZ - 1);
      for (NodeIndexType y = 0; y < m_DimY; y++)
      {
        bool yEdge = (y == 0 || y == m_DimY - 1);
        *(nbSizePtr++) = 0; // x == 0 (there is always padding, so we don't need to check if m_DimX>0)
        unsigned char nbSize = (zEdge || yEdge) ? 0 : numberOfNeighbors;
        for (NodeIndexType x = m_DimX - 2; x > 0; x--)
        {
          *(nbSizePtr++) = nbSize;
        }
        *(nbSizePtr++) =
          0; // x == m_DimX-1 (there is always padding, so we don'neighborNewDistance need to check if m_DimX>1)
      }
    }

    if (!maskLabelVolume)
    {
      // no mask
      for (NodeIndexType index = 0; index < dimXYZ; index++)
      {
        LabelPixelType seedValue = seedLabelVolumePtr[index];
        resultLabelVolumePtr[index] = seedValue;
        if (seedValue == 0)
        {
          m_HeapNodes[index] = DIST_INF;
          distanceVolumePtr[index] = DIST_INF;
        }
        else
        {
          m_HeapNodes[index] = DIST_EPSILON;
          distanceVolumePtr[index] = DIST_EPSILON;
        }
        m_HeapNodes[index].SetIndexValue(index);
        m_Heap->Insert(&m_HeapNodes[index]);
      }
    }
    else
    {
      // with mask
      const MaskPixelType * maskLabelVolumePtr = maskLabelVolume->GetBufferPointer();
      for (NodeIndexType index = 0; index < dimXYZ; index++)
      {
        if (maskLabelVolumePtr[index] != 0)
        {
          // masked region
          resultLabelVolumePtr[index] = 0;
          // small distance will prevent overwriting of masked voxels
          m_HeapNodes[index] = DIST_EPSILON;
          distanceVolumePtr[index] = DIST_EPSILON;
          // we don't add masked voxels to the heap
          // to exclude them from region growing
        }
        else
        {
          // non-masked region
          LabelPixelType seedValue = seedLabelVolumePtr[index];
          resultLabelVolumePtr[index] = seedValue;
          if (seedValue == 0)
          {
            m_HeapNodes[index] = DIST_INF;
            distanceVolumePtr[index] = DIST_INF;
          }
          else
          {
            m_HeapNodes[index] = DIST_EPSILON;
            distanceVolumePtr[index] = DIST_EPSILON;
          }
          m_HeapNodes[index].SetIndexValue(index);
          m_Heap->Insert(&m_HeapNodes[index]);
        }
      }
    }
  }
  else
  {
    // Already initialized
    LabelPixelType *   resultLabelVolumePtr = resultLabelVolume->GetBufferPointer();
    NodeKeyValueType * distanceVolumePtr = m_DistanceVolume->GetBufferPointer();
    for (NodeIndexType index = 0; index < dimXYZ; index++)
    {
      if (seedLabelVolumePtr[index] != 0)
      {
        // Only grow from new/changed seeds
        if (resultLabelVolumePtr[index] != seedLabelVolumePtr[index] // changed seed
            || distanceVolumePtr[index] > DIST_EPSILON               // new seed
        )
        {
          m_HeapNodes[index] = DIST_EPSILON;
          distanceVolumePtr[index] = DIST_EPSILON;
          resultLabelVolumePtr[index] = seedLabelVolumePtr[index];
          m_HeapNodes[index].SetIndexValue(index);
          m_Heap->Insert(&m_HeapNodes[index]);
        }
        // Old seeds will be completely ignored in updates, as their labels have been already propagated
        // and their value cannot changed (because their value is prescribed).
      }
      else
      {
        m_HeapNodes[index] = DIST_INF;
        m_HeapNodes[index].SetIndexValue(index);
        m_Heap->Insert(&m_HeapNodes[index]);
      }
    }
  }

  // Insert 0 then extract it, which will balance heap
  NodeIndexType zeroValueElementIndex = dimXYZ;
  m_HeapNodes[zeroValueElementIndex] = 0;
  m_HeapNodes[zeroValueElementIndex].SetIndexValue(zeroValueElementIndex);
  m_Heap->Insert(&m_HeapNodes[zeroValueElementIndex]);
  m_Heap->ExtractMin();

  return true;
}

template <typename TInputImage, typename TLabelImage, typename TMaskImage>
void
FastGrowCut<TInputImage, TLabelImage, TMaskImage>::DijkstraBasedClassificationAHP()
{
  if (m_Heap == nullptr || m_HeapNodes == nullptr)
  {
    return;
  }

  auto intensityVolume = this->GetInput();
  auto resultLabelVolume = this->GetOutput();

  LabelPixelType *           resultLabelVolumePtr = resultLabelVolume->GetBufferPointer();
  const IntensityPixelType * imSrc = intensityVolume->GetBufferPointer();

  if (!m_bSegInitialized)
  {
    // Full computation
    NodeKeyValueType * distanceVolumePtr = m_DistanceVolume->GetBufferPointer();
    LabelPixelType *   resultLabelVolumePtr = resultLabelVolume->GetBufferPointer();

    // Normal Dijkstra (to be used in initializing the segmenter for the current image)
    while (!m_Heap->IsEmpty())
    {
      FibHeapNode *    hnMin = m_Heap->ExtractMin();
      NodeIndexType    index = hnMin->GetIndexValue();
      NodeKeyValueType currentDistance = hnMin->GetKeyValue();
      LabelPixelType   currentLabel = resultLabelVolumePtr[index];

      // Update neighbors
      NodeKeyValueType pixCenter = imSrc[index];
      unsigned char    nbSize = m_NumberOfNeighbors[index];
      for (unsigned char i = 0; i < nbSize; i++)
      {
        NodeIndexType    indexNgbh = index + m_NeighborIndexOffsets[i];
        NodeKeyValueType neighborCurrentDistance = distanceVolumePtr[indexNgbh];
        NodeKeyValueType neighborNewDistance =
          fabs(pixCenter - imSrc[indexNgbh]) + currentDistance + m_NeighborDistancePenalties[i];
        if (neighborCurrentDistance > neighborNewDistance)
        {
          distanceVolumePtr[indexNgbh] = neighborNewDistance;
          resultLabelVolumePtr[indexNgbh] = currentLabel;
          m_Heap->DecreaseKey(&m_HeapNodes[indexNgbh], neighborNewDistance);
        }
      }
    }
  }
  else
  {
    // Quick update

    // Adaptive Dijkstra
    NodeKeyValueType * distanceVolumePtr = m_DistanceVolume->GetBufferPointer();
    while (!m_Heap->IsEmpty())
    {
      FibHeapNode *    hnMin = m_Heap->ExtractMin();
      NodeKeyValueType currentDistance = hnMin->GetKeyValue();

      // Stop if minimum value is infinite (it means there are no more voxels to propagate labels from)
      if (currentDistance == DIST_INF)
      {
        break;
      }

      NodeIndexType  index = hnMin->GetIndexValue();
      LabelPixelType currentLabel = resultLabelVolumePtr[index];

      // Update neighbors
      NodeKeyValueType pixCenter = imSrc[index];
      unsigned char    nbSize = m_NumberOfNeighbors[index];
      for (unsigned char i = 0; i < nbSize; i++)
      {
        NodeIndexType    indexNgbh = index + m_NeighborIndexOffsets[i];
        NodeKeyValueType neighborCurrentDistance = distanceVolumePtr[indexNgbh];
        NodeKeyValueType neighborNewDistance =
          fabs(pixCenter - imSrc[indexNgbh]) + currentDistance + m_NeighborDistancePenalties[i];
        if (neighborCurrentDistance > neighborNewDistance)
        {
          distanceVolumePtr[indexNgbh] = neighborNewDistance;
          resultLabelVolumePtr[indexNgbh] = currentLabel;

          m_Heap->DecreaseKey(&m_HeapNodes[indexNgbh], neighborNewDistance);
        }
      }
    }
  }

  m_bSegInitialized = true;

  // Release memory
  delete m_Heap;
  m_Heap = nullptr;
  delete[] m_HeapNodes;
  m_HeapNodes = nullptr;
}

template <typename TInputImage, typename TLabelImage, typename TMaskImage>
void
FastGrowCut<TInputImage, TLabelImage, TMaskImage>::GenerateData()
{
  auto       inputImage = this->GetInput();
  auto       seedImage = this->GetSeedImage();
  auto       outputImage = this->GetOutput();
  RegionType inRegion = inputImage->GetLargestPossibleRegion();

  SpacingType  spacing = inputImage->GetSpacing();
  const double compareTolerance = (spacing[0] + spacing[1] + spacing[2]) / 3.0 * 0.01;

  // Copy seedImage into the output
  RegionType region = outputImage->GetRequestedRegion();

  // currently, RequestedRegion is controlled via maskInput
  if (region != inRegion)
  {
    itkExceptionMacro("Currently, RequestedRegion has to be equal to LargestPossibleRegion");
  }

  outputImage->SetLargestPossibleRegion(inRegion);
  outputImage->SetBufferedRegion(region);
  outputImage->Allocate();
  // ImageAlgorithm::Copy(seedImage, outputImage, region, region);

  // These checks are done elsewhere in ITK pipeline:
  // if ((spacing - outputImage->GetSpacing()).GetNorm() > compareTolerance ||
  //     (inputImage->GetOrigin() - outputImage->GetOrigin()).GetNorm() > compareTolerance)
  // {
  //   // Restart growcut from scratch if image size is changed (then cached buffers cannot be reused)
  //   this->Reset();
  // }


  IdentifierType maxNumberOfVoxels = std::numeric_limits<NodeIndexType>::max();
  if (region.GetNumberOfPixels() >= maxNumberOfVoxels)
  {
    // we use unsigned int as index type to reduce memory usage, which limits number of voxels to 2^32,
    // but this is not a practical limitation, as images containing more than 2^32 voxels would take too
    // much time an memory to grow-cut anyway
    itkExceptionMacro("Image size is too large (" << region.GetNumberOfPixels() << " voxels)."
                                                  << " Maximum number of voxels is " << maxNumberOfVoxels - 1 << ".");
  }

  SizeType size = region.GetSize();
  if (size[0] <= 2 || size[1] <= 2 || size[2] <= 2)
  {
    // image is too small (there should be space for at least one voxel padding around the image)
    itkExceptionMacro("Image size is too small. Minimum size along each dimension is 3.");
  }

  this->InitializationAHP();
  this->DijkstraBasedClassificationAHP();
}

template <typename TInputImage, typename TLabelImage, typename TMaskImage>
void
FastGrowCut<TInputImage, TLabelImage, TMaskImage>::EnlargeOutputRequestedRegion(DataObject * output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template <typename TInputImage, typename TLabelImage, typename TMaskImage>
void
FastGrowCut<TInputImage, TLabelImage, TMaskImage>::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if (this->GetInput())
  {
    typename InputImageType::Pointer input = const_cast<TInputImage *>(this->GetInput());
    input->SetRequestedRegionToLargestPossibleRegion();
  }
}


template <typename TInputImage, typename TLabelImage, typename TMaskImage>
void
FastGrowCut<TInputImage, TLabelImage, TMaskImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  // TODO: print other members
}

} // namespace itk
#endif // ifndef itkFastGrowCut_hxx
