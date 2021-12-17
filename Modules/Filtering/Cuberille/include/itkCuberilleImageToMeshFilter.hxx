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

#ifndef itkCuberilleImageToMeshFilter_hxx
#define itkCuberilleImageToMeshFilter_hxx

#include "itkMath.h"
#include "itkNumericTraits.h"
#include <itkConnectedComponentImageFilter.h>
#include <array>
#include <utility>
#include <bitset>

namespace itk
{

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::CuberilleImageToMeshFilter()
  : m_IsoSurfaceValue(NumericTraits<InputPixelType>::One)
  , m_MaxSpacing(NumericTraits<SpacingValueType>::One)

{
  this->SetNumberOfRequiredInputs(1);
  this->CalculateLabelsArray();
}

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::~CuberilleImageToMeshFilter()
{
  m_GradientInterpolator = nullptr;
}

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::SetInput(const InputImageType * image)
{
  this->ProcessObject::SetNthInput(0, const_cast<InputImageType *>(image));
}

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::GenerateData()
{
#if DEBUG_PRINT
  m_ProjectVertexTerminate[0] = 0;
  m_ProjectVertexTerminate[1] = 0;
  m_ProjectVertexTerminate[2] = 0;
#endif

  // Get input/output
  InputImageConstPointer           image = Superclass::GetInput(0);
  typename OutputMeshType::Pointer mesh = Superclass::GetOutput();

  // Compute maximum spacing
  m_MaxSpacing = image->GetSpacing().GetVnlVector().max_value();

  // Set default step length
  if (m_ProjectVertexStepLength < 0.0)
  {
    m_ProjectVertexStepLength = m_MaxSpacing * 0.25;
  }

  // Create interpolator for pixel value
  if (m_Interpolator.IsNull())
  {
    m_Interpolator = LinearInterpolateImageFunction<InputImageType>::New();
  }
  m_Interpolator->SetInputImage(image);

  // Create interpolator for gradient image
  ComputeGradientImage();

  // Set up iterator
  typename InputImageIteratorType::SizeType radius;
  radius.Fill(1);
  InputImageIteratorType it(radius, image, image->GetBufferedRegion());
  setConnectivity<InputImageIteratorType>(&it, false); // Set face connectivity

  // TODO: Estimate number of vertices/faces for which to reserve
  // TODO: There is an issue with quad edge mesh related to reserving, see
  // http://www.itk.org/mailman/private/insight-developers/2010-June/014653.html
  // SizeType size = image->GetLargestPossibleRegion().GetSize();
  // mesh->GetPoints()->Reserve( (size[0]*size[1]*size[2]) / 200 );
  // mesh->GetCells()->Reserve( (size[0]*size[1]*size[2]) / 100 );

  // Set up helper structures
  unsigned int                                               look0 = 1;
  unsigned int                                               look1 = 0;
  unsigned char                                              numFaces = 0;
  std::array<bool, 6>                                        faceHasQuad;
  std::array<bool, 8>                                        vertexHasQuad;
  std::array<PointIdentifier, 8>                             v;
  std::array<PointIdentifier, 4>                             f;
  PointIdentifier                                            nextVertexId = 0;
  CellIdentifier                                             nextCellId = 0;
  IndexType                                                  index;
  typename IndexType::IndexValueType                         lastZ = -1;
  InputPixelType                                             center = 0;
  std::array<typename InputImageIteratorType::OffsetType, 6> offset;
  offset[0][0] = -1;
  offset[0][1] = +0;
  offset[0][2] = +0; // -X
  offset[1][0] = +0;
  offset[1][1] = -1;
  offset[1][2] = +0; // -Y
  offset[2][0] = +1;
  offset[2][1] = +0;
  offset[2][2] = +0; // +X
  offset[3][0] = +0;
  offset[3][1] = +1;
  offset[3][2] = +0; // +Y
  offset[4][0] = +0;
  offset[4][1] = +0;
  offset[4][2] = -1; // -Z
  offset[5][0] = +0;
  offset[5][1] = +0;
  offset[5][2] = +1; // +Z
  VertexLookupMapType lookup[2];
  lookup[look0].Clear();
  lookup[look1].Clear();

  // TODO: Handle voxels on the edge of the image

  // Iterate input image
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    // Determine if current pixel is suitable
    center = it.GetCenterPixel();
    if (center < m_IsoSurfaceValue)
    {
      continue;
    }

    // Re-initialize for new pixel
    numFaces = 0;
    faceHasQuad.fill(false);
    vertexHasQuad.fill(false);

    // Re-initialize for new z plane
    index = it.GetIndex();
    if (index[2] != lastZ)
    {
      std::swap(look0, look1);
      lookup[look1].Clear();
      lastZ = index[2];
    }

    // Compute which faces (if any) have quads
    for (unsigned int i = 0; i < 6; ++i)
    {
      // NOTE: suitability check above means center <= m_IsoSurfaceValue
      faceHasQuad[i] = (it.GetPixel(offset[i]) < m_IsoSurfaceValue);
      if (faceHasQuad[i])
      {
        numFaces++;
        SetVerticesFromFace(i, vertexHasQuad);
      }
    }

    // Process each face
    if (numFaces > 0)
    {
      // Create vertices
      for (unsigned int i = 0; i < 8; ++i)
      {
        if (!vertexHasQuad[i])
        {
          continue;
        }

        // Use the vertex lookup to get the vertex for the correct slice
        IndexType    vindex = GetVertexLookupIndex(i, index);
        const auto   bitmaskID = this->CalculateBitmaskIDForVertexIndex(vindex);
        const auto   diff = vindex - index;
        size_t       offsetID = 7 - (diff[0] + diff[1] * 2 + diff[2] * 4);
        const auto   components = this->m_LabelsArray.at(bitmaskID);
        const auto   component = components.at(offsetID);
        unsigned int look = (i < 4) ? look0 : look1; // First four are first slice
        if (!lookup[look].GetVertex(vindex[0], vindex[1], component, v[i]))
        {
          // Vertex was not in lookup, create and add to lookup
          v[i] = nextVertexId;
          const auto numComponents = (*std::max_element(components.begin(), components.end())) + 1;
          const auto pv = AddVertex(nextVertexId, vindex, image, mesh, numComponents);
          lookup[look].AddVertex(vindex[0], vindex[1], pv);
        }

      } // end foreach vertex

      // Create faces
      if (faceHasQuad[0])
      {
        f[0] = v[0];
        f[1] = v[4];
        f[2] = v[7];
        f[3] = v[3];
        AddQuadFace(nextCellId, f, mesh, center);
      }
      if (faceHasQuad[1])
      {
        f[0] = v[0];
        f[1] = v[1];
        f[2] = v[5];
        f[3] = v[4];
        AddQuadFace(nextCellId, f, mesh, center);
      }
      if (faceHasQuad[2])
      {
        f[0] = v[1];
        f[1] = v[2];
        f[2] = v[6];
        f[3] = v[5];
        AddQuadFace(nextCellId, f, mesh, center);
      }
      if (faceHasQuad[3])
      {
        f[0] = v[2];
        f[1] = v[3];
        f[2] = v[7];
        f[3] = v[6];
        AddQuadFace(nextCellId, f, mesh, center);
      }
      if (faceHasQuad[4])
      {
        f[0] = v[0];
        f[1] = v[3];
        f[2] = v[2];
        f[3] = v[1];
        AddQuadFace(nextCellId, f, mesh, center);
      }
      if (faceHasQuad[5])
      {
        f[0] = v[4];
        f[1] = v[5];
        f[2] = v[6];
        f[3] = v[7];
        AddQuadFace(nextCellId, f, mesh, center);
      }

    } // end if num faces > 0
  }

#if DEBUG_PRINT
  std::cout << "Number of terminations due to surface distance threshold: " << m_ProjectVertexTerminate[0] << std::endl;
  std::cout << "Number of terminations due to maximum number of steps: " << m_ProjectVertexTerminate[1] << std::endl;
#  if USE_ADVANCED_PROJECTION
  std::cout << "Number of terminations due to too many sign swaps: " << m_ProjectVertexTerminate[2] << std::endl;
#  endif
#endif
}

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::SetVerticesFromFace(unsigned int          face,
                                                                                         std::array<bool, 8> & v)
{
  switch (face)
  {
    case 0:
      v[0] = true;
      v[4] = true;
      v[7] = true;
      v[3] = true;
      break;
    case 1:
      v[0] = true;
      v[1] = true;
      v[5] = true;
      v[4] = true;
      break;
    case 2:
      v[1] = true;
      v[2] = true;
      v[6] = true;
      v[5] = true;
      break;
    case 3:
      v[2] = true;
      v[3] = true;
      v[7] = true;
      v[6] = true;
      break;
    case 4:
      v[0] = true;
      v[3] = true;
      v[2] = true;
      v[1] = true;
      break;
    case 5:
      v[4] = true;
      v[5] = true;
      v[6] = true;
      v[7] = true;
      break;
  }
}

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
typename TInputImage::IndexType
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::GetVertexLookupIndex(
  unsigned int                    vertex,
  typename TInputImage::IndexType index)
{
  IndexType result(index);
  switch (vertex)
  {
    case 0:
      break;
    case 1:
      result[0] += 1;
      break;
    case 2:
      result[0] += 1;
      result[1] += 1;
      break;
    case 3:
      result[1] += 1;
      break;
    case 4:
      result[2] += 1;
      break;
    case 5:
      result[0] += 1;
      result[2] += 1;
      break;
    case 6:
      result[0] += 1;
      result[1] += 1;
      result[2] += 1;
      break;
    case 7:
      result[1] += 1;
      result[2] += 1;
      break;
  }
  return result;
}

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
typename CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::PointVectorType
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::AddVertex(
  typename TOutputMesh::PointIdentifier & id,
  typename TInputImage::IndexType         index,
  const TInputImage *                     image,
  TOutputMesh *                           mesh,
  const size_t                            numComponents)
{
  PointType vertex;
  image->TransformIndexToPhysicalPoint(index, vertex);
  const auto spacing = image->GetSpacing();
  const auto direction = image->GetDirection();
  const auto offset = direction * spacing * 0.5;
  vertex -= offset;
  if (m_ProjectVerticesToIsoSurface)
  {
    ProjectVertexToIsoSurface(vertex);
  }
  PointVectorType pointIDVector;
  for (size_t i = 0; i < numComponents; ++i)
  {
    pointIDVector.push_back(id);
    mesh->GetPoints()->InsertElement(id++, vertex);
  }
  return pointIDVector;
}

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::AddQuadFace(
  typename TOutputMesh::CellIdentifier & id,
  std::array<PointIdentifier, 4>         face,
  TOutputMesh *                          mesh,
  const InputPixelType &                 pixel)
{
  if (m_GenerateTriangleFaces)
  {
    // Get vertices
    PointType v[4];
    for (unsigned int i = 0; i < 4; ++i)
    {
      v[i] = mesh->GetPoints()->GetElement(face[i]);
    }

    // Split the quad along the longest edge to avoid skinny triangles
    PointIdentifier face1[3];
    PointIdentifier face2[3];
    if (v[0].SquaredEuclideanDistanceTo(v[2]) >= v[1].SquaredEuclideanDistanceTo(v[3]))
    {
      face1[0] = face[0];
      face1[1] = face[1];
      face1[2] = face[3];
      face2[0] = face[1];
      face2[1] = face[2];
      face2[2] = face[3];
    }
    else
    {
      face1[0] = face[0];
      face1[1] = face[1];
      face1[2] = face[2];
      face2[0] = face[0];
      face2[1] = face[2];
      face2[2] = face[3];
    }

    // Add triangle 1 cell
    TriangleCellAutoPointer tri1;
    tri1.TakeOwnership(new TriangleCellType);
    tri1->SetPointIds(face1);
    mesh->SetCell(id++, tri1);
    if (this->m_SavePixelAsCellData)
    {
      mesh->SetCellData((id - 1), pixel);
    }

    // Add triangle 2 cell
    TriangleCellAutoPointer tri2;
    tri2.TakeOwnership(new TriangleCellType);
    tri2->SetPointIds(face2);
    mesh->SetCell(id++, tri2);
    if (this->m_SavePixelAsCellData)
    {
      mesh->SetCellData((id - 1), pixel);
    }
  }
  else
  {
    // Add quateraleral cell
    QuadrilateralCellAutoPointer quad1;
    quad1.TakeOwnership(new QuadrilateralCellType);
    quad1->SetPointIds(face.data());
    mesh->SetCell(id++, quad1);
    if (this->m_SavePixelAsCellData)
    {
      mesh->SetCellData((id - 1), pixel);
    }
  }
}

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::ProjectVertexToIsoSurface(PointType & vertex)
{
#if USE_ADVANCED_PROJECTION
  // Set up
  bool                   done = false;
  double                 step = m_ProjectVertexStepLength;
  unsigned int           numberOfSteps = 0;
  PointType              temp[2];
  PointType              tempBest;
  InterpolatorOutputType value[2];
  double                 diff[2];
  double                 diffBest = -1.0;
  unsigned int           i, swaps = 0;
  int                    previousi = -1;

  while (!done)
  {
    // Compute normal vector
    GradientPixelType normal = m_GradientInterpolator->Evaluate(vertex);
    if (normal.Normalize() == 0.0) // old norm was zero
    {
      break;
    }

    // Step along both directions of normal
    for (i = 0; i < InputImageType::ImageDimension; ++i)
    {
      temp[0][i] = vertex[i] + (normal[i] * +1.0 * step);
      temp[1][i] = vertex[i] + (normal[i] * -1.0 * step);
    }
    step *= m_ProjectVertexStepLengthRelaxationFactor;

    // Compute which direction moves vertex closer to iso-surface value
    value[0] = m_Interpolator->Evaluate(temp[0]);
    value[1] = m_Interpolator->Evaluate(temp[1]);
    diff[0] = itk::Math::abs(value[0] - m_IsoSurfaceValue);
    diff[1] = itk::Math::abs(value[1] - m_IsoSurfaceValue);
    i = (diff[0] <= diff[1]) ? 0 : 1;
    if (previousi < 0)
    {
      previousi = i;
    }
    swaps += (int)(previousi != i);
    vertex = temp[i];

    // Determine whether vertex is close enough to iso-surface value
    done |= diff[i] < m_ProjectVertexSurfaceDistanceThreshold;
#  if DEBUG_PRINT
    if (done)
    {
      m_ProjectVertexTerminate[0]++;
    }
#  endif
    if (done)
    {
      break;
    }

    // Determine whether we have done enough steps
    done |= numberOfSteps++ > m_ProjectVertexMaximumNumberOfSteps;
#  if DEBUG_PRINT
    if (done)
    {
      m_ProjectVertexTerminate[1]++;
    }
#  endif
    if (done)
    {
      break;
    }

    // Determine whether there has been too many sign swaps (oscillating)
    done |= (swaps >= 5);
#  if DEBUG_PRINT
    if (done)
    {
      m_ProjectVertexTerminate[2]++;
    }
#  endif
    if (done)
    {
      break;
    }
  }
#elif USE_LINESEARCH_PROJECTION

  // Set up
  unsigned int           k;
  GradientPixelType      normal;
  InterpolatorOutputType value;
  PointType              temp, bestVertex;
  double                 sign, d, metric, bestMetric = 10000;

  // Compute normal vector
  normal = m_GradientInterpolator->Evaluate(vertex);
  if (normal.Normalize() == 0.0) // old norm was zero
  {
    break;
  }

  // Search on both sides of the line
  for (sign = -1.0; sign <= 1.0; sign += 2.0)
  {
    k = (sign == -1.0) ? 0 : 1;
    for (unsigned int j = 1; j < m_ProjectVertexMaximumNumberOfSteps / 2; ++j)
    {
      // Compute current location along line
      d = (double)j / ((double)m_ProjectVertexMaximumNumberOfSteps / 2.0);
      for (unsigned int i = 0; i < InputImageType::ImageDimension; ++i)
      {
        temp[i] = vertex[i] + (normal[i] * sign * m_ProjectVertexStepLength * d);
      }
      // Compute metric (combination of difference and distance)
      value = m_Interpolator->Evaluate(temp);
      metric = itk::Math::abs(value - m_IsoSurfaceValue); // Difference
      // metric /= NumericTraits<InputPixelType>::max(); // Normalized difference
      // metric /= d; // Distance

      // Determine if current position is the "best"
      if (metric < bestMetric)
      {
        bestMetric = metric;
        bestVertex = temp;
      }
    }
  }
  vertex = bestVertex;

#else
  // Set up
  bool                   done = false;
  double                 sign = 1.0;
  double                 step = m_ProjectVertexStepLength;
  unsigned int           numberOfSteps = 0;
  GradientPixelType      normal;
  InterpolatorOutputType value;

  while (!done)
  {
    // Compute normal vector
    normal = m_GradientInterpolator->Evaluate(vertex);
    if (normal.Normalize() == 0.0) // old norm was zero
    {
      break;
    }

    // Compute whether vertex is close enough to iso-surface value
    value = m_Interpolator->Evaluate(vertex);
    done |= itk::Math::abs(value - m_IsoSurfaceValue) < m_ProjectVertexSurfaceDistanceThreshold;
#  if DEBUG_PRINT
    if (done)
    {
      m_ProjectVertexTerminate[0]++;
    }
#  endif
    if (done)
    {
      break;
    }

    // Step along the normal towards the iso-surface value
    sign = (value < m_IsoSurfaceValue) ? +1.0 : -1.0;
    for (unsigned int i = 0; i < InputImageType::ImageDimension; ++i)
    {
      vertex[i] += (normal[i] * sign * step);
    }
    step *= m_ProjectVertexStepLengthRelaxationFactor;
    done |= numberOfSteps++ > m_ProjectVertexMaximumNumberOfSteps;
#  if DEBUG_PRINT
    if (done)
    {
      m_ProjectVertexTerminate[1]++;
    }
#  endif
  }
#endif
}

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::ComputeGradientImage()
{
  if (m_ProjectVerticesToIsoSurface && m_GradientInterpolator.IsNull())
  {
    typename GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
    gradientFilter->SetInput(Superclass::GetInput(0));
#if USE_GRADIENT_RECURSIVE_GAUSSIAN
    gradientFilter->SetSigma(m_MaxSpacing * 1.0);
    gradientFilter->SetNormalizeAcrossScale(true);
#endif
    gradientFilter->Update();
    m_GradientInterpolator = GradientInterpolatorType::New();
    m_GradientInterpolator->SetInputImage(gradientFilter->GetOutput());
    gradientFilter->GetOutput()->DisconnectPipeline();
    gradientFilter = nullptr;
  }
}

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
size_t
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::CalculateBitmaskIDForVertexIndex(
  const IndexType & vindex)
{
  typename IndexType::OffsetType ones;
  ones.Fill(1);
  const IndexType localorigin = vindex - ones;
  std::bitset<8>  bitmask;
  for (size_t i = 0; i < 8; ++i)
  {
    std::bitset<3>                 bits(i);
    typename IndexType::OffsetType offset;
    offset[0] = bits[0];
    offset[1] = bits[1];
    offset[2] = bits[2];
    const auto index = localorigin + offset;
    const auto pixel = this->GetInput()->GetPixel(index);
    bitmask[i] = (pixel >= this->m_IsoSurfaceValue);
  }
  return static_cast<size_t>(bitmask.to_ulong());
}

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::CalculateLabelsArray()
{

  //  The commented code below iterates through all possible binary 2x2x2 regions,
  //  and runs connected components on the result.  The total number of foreground
  //  connected components is equal to the number of vertices which must be
  //  replicated--these vertices are stored in the VertexMap.  One less than the
  //  number of the connected component is equal to the index of the corresponding
  //  vertex in the VertexMap.
  //
  //  This method works for all but four of the 256 possible cases.  The failing
  //  cases occur when there are two background pixels at opposite corners of the
  //  2x2x2 region, and all remaining pixels are foreground.  In these cases,
  //  there is only one connected component, but two vertices must be added.  These
  //  cases are handled separately.
  //
  //  While it would be possible to generate this at runtime, it could theoretically
  //  cause a significant performance hit in a case where many instances of this class
  //  must be instantiated.  Therefore, the values are hardcoded below.  The commented
  //  code was used to generate the hardcoded values.
  //
  //  NOTE: There was previously a bug in itk::ConnectedComponentImageFilter which
  //  caused the hardcoded values below to be incorrect, though this has since been
  //  fixed.  Therefore, the commented code below will fail if built prior to the
  //  following patch:
  //
  //  Git Hash: c32a7846cb6502eaa79780de3ff7b0ce81597118
  //
  //  using TImage = itk::Image<unsigned char, 3>;
  //  using TConnected = itk::ConnectedComponentImageFilter< TImage, TImage >;
  //
  //  for (size_t mask = 0; mask < std::pow(2, 8); ++mask) {
  //
  //    std::bitset<8> bitmask(mask);
  //
  //    const auto image = TImage::New();
  //    TImage::SizeType size;
  //    size.Fill( 2 );
  //
  //    TImage::IndexType origin;
  //    origin.Fill( 0 );
  //
  //    TImage::RegionType region(origin, size);
  //
  //    image->SetRegions( region );
  //    image->Allocate();
  //    image->FillBuffer( 0 );
  //
  //    for (size_t index = 0; index < std::pow(2, 3); ++index) {
  //      std::bitset<3> bitindex(index);
  //      image->SetPixel( {{bitindex[0], bitindex[1], bitindex[2]}}, bitmask[index] );
  //    }
  //
  //    const auto connected = TConnected::New();
  //    connected->SetInput( image );
  //    connected->FullyConnectedOff();
  //    connected->Update();
  //
  //    TLabels labels;
  //
  //    for (size_t index = 0; index < std::pow(2, 3); ++index) {
  //      std::bitset<3> bitindex(index);
  //      const auto component = connected->GetOutput()->GetPixel({{
  //        bitindex[0],
  //        bitindex[1],
  //        bitindex[2]
  //      }});
  //      labels[index] = component - 1;
  //    }
  //
  //    this->m_LabelsArray[mask] = labels;
  //
  //  }
  //
  //  // Manually handle the corner cases, discussed above.
  //
  //  this->m_LabelsArray[126] = { { -1, 0, 0, 1, 0, 1, 1, -1 } };
  //  this->m_LabelsArray[189] = { { 0, -1, 1, 0, 1, 0, -1, 1 } };
  //  this->m_LabelsArray[219] = { { 0, 1, -1, 0, 1, -1, 0, 1 } };
  //  this->m_LabelsArray[231] = { { 0, 1, 1, -1, -1, 0, 0, 1 } };
  //
  //  for (size_t i = 0; i < std::pow(2,8); ++i) {
  //    std::cout << "this->m_LabelsArray[" << i << "] ";
  //    std::cout << "= { {";
  //    for (size_t j = 0; j < std::pow(2,3); ++j) {
  //      const auto k = static_cast<int>(this->m_LabelsArray[i][j]);
  //      std::cout << ' ' << k;
  //      if (j != 7) std::cout << ',';
  //    }
  //    std::cout << " } };\n";
  //  }

  this->m_LabelsArray[0] = { { -1, -1, -1, -1, -1, -1, -1, -1 } };
  this->m_LabelsArray[1] = { { 0, -1, -1, -1, -1, -1, -1, -1 } };
  this->m_LabelsArray[2] = { { -1, 0, -1, -1, -1, -1, -1, -1 } };
  this->m_LabelsArray[3] = { { 0, 0, -1, -1, -1, -1, -1, -1 } };
  this->m_LabelsArray[4] = { { -1, -1, 0, -1, -1, -1, -1, -1 } };
  this->m_LabelsArray[5] = { { 0, -1, 0, -1, -1, -1, -1, -1 } };
  this->m_LabelsArray[6] = { { -1, 0, 1, -1, -1, -1, -1, -1 } };
  this->m_LabelsArray[7] = { { 0, 0, 0, -1, -1, -1, -1, -1 } };
  this->m_LabelsArray[8] = { { -1, -1, -1, 0, -1, -1, -1, -1 } };
  this->m_LabelsArray[9] = { { 0, -1, -1, 1, -1, -1, -1, -1 } };
  this->m_LabelsArray[10] = { { -1, 0, -1, 0, -1, -1, -1, -1 } };
  this->m_LabelsArray[11] = { { 0, 0, -1, 0, -1, -1, -1, -1 } };
  this->m_LabelsArray[12] = { { -1, -1, 0, 0, -1, -1, -1, -1 } };
  this->m_LabelsArray[13] = { { 0, -1, 0, 0, -1, -1, -1, -1 } };
  this->m_LabelsArray[14] = { { -1, 0, 0, 0, -1, -1, -1, -1 } };
  this->m_LabelsArray[15] = { { 0, 0, 0, 0, -1, -1, -1, -1 } };
  this->m_LabelsArray[16] = { { -1, -1, -1, -1, 0, -1, -1, -1 } };
  this->m_LabelsArray[17] = { { 0, -1, -1, -1, 0, -1, -1, -1 } };
  this->m_LabelsArray[18] = { { -1, 0, -1, -1, 1, -1, -1, -1 } };
  this->m_LabelsArray[19] = { { 0, 0, -1, -1, 0, -1, -1, -1 } };
  this->m_LabelsArray[20] = { { -1, -1, 0, -1, 1, -1, -1, -1 } };
  this->m_LabelsArray[21] = { { 0, -1, 0, -1, 0, -1, -1, -1 } };
  this->m_LabelsArray[22] = { { -1, 0, 1, -1, 2, -1, -1, -1 } };
  this->m_LabelsArray[23] = { { 0, 0, 0, -1, 0, -1, -1, -1 } };
  this->m_LabelsArray[24] = { { -1, -1, -1, 0, 1, -1, -1, -1 } };
  this->m_LabelsArray[25] = { { 0, -1, -1, 1, 0, -1, -1, -1 } };
  this->m_LabelsArray[26] = { { -1, 0, -1, 0, 1, -1, -1, -1 } };
  this->m_LabelsArray[27] = { { 0, 0, -1, 0, 0, -1, -1, -1 } };
  this->m_LabelsArray[28] = { { -1, -1, 0, 0, 1, -1, -1, -1 } };
  this->m_LabelsArray[29] = { { 0, -1, 0, 0, 0, -1, -1, -1 } };
  this->m_LabelsArray[30] = { { -1, 0, 0, 0, 1, -1, -1, -1 } };
  this->m_LabelsArray[31] = { { 0, 0, 0, 0, 0, -1, -1, -1 } };
  this->m_LabelsArray[32] = { { -1, -1, -1, -1, -1, 0, -1, -1 } };
  this->m_LabelsArray[33] = { { 0, -1, -1, -1, -1, 1, -1, -1 } };
  this->m_LabelsArray[34] = { { -1, 0, -1, -1, -1, 0, -1, -1 } };
  this->m_LabelsArray[35] = { { 0, 0, -1, -1, -1, 0, -1, -1 } };
  this->m_LabelsArray[36] = { { -1, -1, 0, -1, -1, 1, -1, -1 } };
  this->m_LabelsArray[37] = { { 0, -1, 0, -1, -1, 1, -1, -1 } };
  this->m_LabelsArray[38] = { { -1, 0, 1, -1, -1, 0, -1, -1 } };
  this->m_LabelsArray[39] = { { 0, 0, 0, -1, -1, 0, -1, -1 } };
  this->m_LabelsArray[40] = { { -1, -1, -1, 0, -1, 1, -1, -1 } };
  this->m_LabelsArray[41] = { { 0, -1, -1, 1, -1, 2, -1, -1 } };
  this->m_LabelsArray[42] = { { -1, 0, -1, 0, -1, 0, -1, -1 } };
  this->m_LabelsArray[43] = { { 0, 0, -1, 0, -1, 0, -1, -1 } };
  this->m_LabelsArray[44] = { { -1, -1, 0, 0, -1, 1, -1, -1 } };
  this->m_LabelsArray[45] = { { 0, -1, 0, 0, -1, 1, -1, -1 } };
  this->m_LabelsArray[46] = { { -1, 0, 0, 0, -1, 0, -1, -1 } };
  this->m_LabelsArray[47] = { { 0, 0, 0, 0, -1, 0, -1, -1 } };
  this->m_LabelsArray[48] = { { -1, -1, -1, -1, 0, 0, -1, -1 } };
  this->m_LabelsArray[49] = { { 0, -1, -1, -1, 0, 0, -1, -1 } };
  this->m_LabelsArray[50] = { { -1, 0, -1, -1, 0, 0, -1, -1 } };
  this->m_LabelsArray[51] = { { 0, 0, -1, -1, 0, 0, -1, -1 } };
  this->m_LabelsArray[52] = { { -1, -1, 0, -1, 1, 1, -1, -1 } };
  this->m_LabelsArray[53] = { { 0, -1, 0, -1, 0, 0, -1, -1 } };
  this->m_LabelsArray[54] = { { -1, 0, 1, -1, 0, 0, -1, -1 } };
  this->m_LabelsArray[55] = { { 0, 0, 0, -1, 0, 0, -1, -1 } };
  this->m_LabelsArray[56] = { { -1, -1, -1, 0, 1, 1, -1, -1 } };
  this->m_LabelsArray[57] = { { 0, -1, -1, 1, 0, 0, -1, -1 } };
  this->m_LabelsArray[58] = { { -1, 0, -1, 0, 0, 0, -1, -1 } };
  this->m_LabelsArray[59] = { { 0, 0, -1, 0, 0, 0, -1, -1 } };
  this->m_LabelsArray[60] = { { -1, -1, 0, 0, 1, 1, -1, -1 } };
  this->m_LabelsArray[61] = { { 0, -1, 0, 0, 0, 0, -1, -1 } };
  this->m_LabelsArray[62] = { { -1, 0, 0, 0, 0, 0, -1, -1 } };
  this->m_LabelsArray[63] = { { 0, 0, 0, 0, 0, 0, -1, -1 } };
  this->m_LabelsArray[64] = { { -1, -1, -1, -1, -1, -1, 0, -1 } };
  this->m_LabelsArray[65] = { { 0, -1, -1, -1, -1, -1, 1, -1 } };
  this->m_LabelsArray[66] = { { -1, 0, -1, -1, -1, -1, 1, -1 } };
  this->m_LabelsArray[67] = { { 0, 0, -1, -1, -1, -1, 1, -1 } };
  this->m_LabelsArray[68] = { { -1, -1, 0, -1, -1, -1, 0, -1 } };
  this->m_LabelsArray[69] = { { 0, -1, 0, -1, -1, -1, 0, -1 } };
  this->m_LabelsArray[70] = { { -1, 0, 1, -1, -1, -1, 1, -1 } };
  this->m_LabelsArray[71] = { { 0, 0, 0, -1, -1, -1, 0, -1 } };
  this->m_LabelsArray[72] = { { -1, -1, -1, 0, -1, -1, 1, -1 } };
  this->m_LabelsArray[73] = { { 0, -1, -1, 1, -1, -1, 2, -1 } };
  this->m_LabelsArray[74] = { { -1, 0, -1, 0, -1, -1, 1, -1 } };
  this->m_LabelsArray[75] = { { 0, 0, -1, 0, -1, -1, 1, -1 } };
  this->m_LabelsArray[76] = { { -1, -1, 0, 0, -1, -1, 0, -1 } };
  this->m_LabelsArray[77] = { { 0, -1, 0, 0, -1, -1, 0, -1 } };
  this->m_LabelsArray[78] = { { -1, 0, 0, 0, -1, -1, 0, -1 } };
  this->m_LabelsArray[79] = { { 0, 0, 0, 0, -1, -1, 0, -1 } };
  this->m_LabelsArray[80] = { { -1, -1, -1, -1, 0, -1, 0, -1 } };
  this->m_LabelsArray[81] = { { 0, -1, -1, -1, 0, -1, 0, -1 } };
  this->m_LabelsArray[82] = { { -1, 0, -1, -1, 1, -1, 1, -1 } };
  this->m_LabelsArray[83] = { { 0, 0, -1, -1, 0, -1, 0, -1 } };
  this->m_LabelsArray[84] = { { -1, -1, 0, -1, 0, -1, 0, -1 } };
  this->m_LabelsArray[85] = { { 0, -1, 0, -1, 0, -1, 0, -1 } };
  this->m_LabelsArray[86] = { { -1, 0, 1, -1, 1, -1, 1, -1 } };
  this->m_LabelsArray[87] = { { 0, 0, 0, -1, 0, -1, 0, -1 } };
  this->m_LabelsArray[88] = { { -1, -1, -1, 0, 1, -1, 1, -1 } };
  this->m_LabelsArray[89] = { { 0, -1, -1, 1, 0, -1, 0, -1 } };
  this->m_LabelsArray[90] = { { -1, 0, -1, 0, 1, -1, 1, -1 } };
  this->m_LabelsArray[91] = { { 0, 0, -1, 0, 0, -1, 0, -1 } };
  this->m_LabelsArray[92] = { { -1, -1, 0, 0, 0, -1, 0, -1 } };
  this->m_LabelsArray[93] = { { 0, -1, 0, 0, 0, -1, 0, -1 } };
  this->m_LabelsArray[94] = { { -1, 0, 0, 0, 0, -1, 0, -1 } };
  this->m_LabelsArray[95] = { { 0, 0, 0, 0, 0, -1, 0, -1 } };
  this->m_LabelsArray[96] = { { -1, -1, -1, -1, -1, 0, 1, -1 } };
  this->m_LabelsArray[97] = { { 0, -1, -1, -1, -1, 1, 2, -1 } };
  this->m_LabelsArray[98] = { { -1, 0, -1, -1, -1, 0, 1, -1 } };
  this->m_LabelsArray[99] = { { 0, 0, -1, -1, -1, 0, 1, -1 } };
  this->m_LabelsArray[100] = { { -1, -1, 0, -1, -1, 1, 0, -1 } };
  this->m_LabelsArray[101] = { { 0, -1, 0, -1, -1, 1, 0, -1 } };
  this->m_LabelsArray[102] = { { -1, 0, 1, -1, -1, 0, 1, -1 } };
  this->m_LabelsArray[103] = { { 0, 0, 0, -1, -1, 0, 0, -1 } };
  this->m_LabelsArray[104] = { { -1, -1, -1, 0, -1, 1, 2, -1 } };
  this->m_LabelsArray[105] = { { 0, -1, -1, 1, -1, 2, 3, -1 } };
  this->m_LabelsArray[106] = { { -1, 0, -1, 0, -1, 0, 1, -1 } };
  this->m_LabelsArray[107] = { { 0, 0, -1, 0, -1, 0, 1, -1 } };
  this->m_LabelsArray[108] = { { -1, -1, 0, 0, -1, 1, 0, -1 } };
  this->m_LabelsArray[109] = { { 0, -1, 0, 0, -1, 1, 0, -1 } };
  this->m_LabelsArray[110] = { { -1, 0, 0, 0, -1, 0, 0, -1 } };
  this->m_LabelsArray[111] = { { 0, 0, 0, 0, -1, 0, 0, -1 } };
  this->m_LabelsArray[112] = { { -1, -1, -1, -1, 0, 0, 0, -1 } };
  this->m_LabelsArray[113] = { { 0, -1, -1, -1, 0, 0, 0, -1 } };
  this->m_LabelsArray[114] = { { -1, 0, -1, -1, 0, 0, 0, -1 } };
  this->m_LabelsArray[115] = { { 0, 0, -1, -1, 0, 0, 0, -1 } };
  this->m_LabelsArray[116] = { { -1, -1, 0, -1, 0, 0, 0, -1 } };
  this->m_LabelsArray[117] = { { 0, -1, 0, -1, 0, 0, 0, -1 } };
  this->m_LabelsArray[118] = { { -1, 0, 0, -1, 0, 0, 0, -1 } };
  this->m_LabelsArray[119] = { { 0, 0, 0, -1, 0, 0, 0, -1 } };
  this->m_LabelsArray[120] = { { -1, -1, -1, 0, 1, 1, 1, -1 } };
  this->m_LabelsArray[121] = { { 0, -1, -1, 1, 0, 0, 0, -1 } };
  this->m_LabelsArray[122] = { { -1, 0, -1, 0, 0, 0, 0, -1 } };
  this->m_LabelsArray[123] = { { 0, 0, -1, 0, 0, 0, 0, -1 } };
  this->m_LabelsArray[124] = { { -1, -1, 0, 0, 0, 0, 0, -1 } };
  this->m_LabelsArray[125] = { { 0, -1, 0, 0, 0, 0, 0, -1 } };
  this->m_LabelsArray[126] = { { -1, 0, 0, 1, 0, 1, 1, -1 } };
  this->m_LabelsArray[127] = { { 0, 0, 0, 0, 0, 0, 0, -1 } };
  this->m_LabelsArray[128] = { { -1, -1, -1, -1, -1, -1, -1, 0 } };
  this->m_LabelsArray[129] = { { 0, -1, -1, -1, -1, -1, -1, 1 } };
  this->m_LabelsArray[130] = { { -1, 0, -1, -1, -1, -1, -1, 1 } };
  this->m_LabelsArray[131] = { { 0, 0, -1, -1, -1, -1, -1, 1 } };
  this->m_LabelsArray[132] = { { -1, -1, 0, -1, -1, -1, -1, 1 } };
  this->m_LabelsArray[133] = { { 0, -1, 0, -1, -1, -1, -1, 1 } };
  this->m_LabelsArray[134] = { { -1, 0, 1, -1, -1, -1, -1, 2 } };
  this->m_LabelsArray[135] = { { 0, 0, 0, -1, -1, -1, -1, 1 } };
  this->m_LabelsArray[136] = { { -1, -1, -1, 0, -1, -1, -1, 0 } };
  this->m_LabelsArray[137] = { { 0, -1, -1, 1, -1, -1, -1, 1 } };
  this->m_LabelsArray[138] = { { -1, 0, -1, 0, -1, -1, -1, 0 } };
  this->m_LabelsArray[139] = { { 0, 0, -1, 0, -1, -1, -1, 0 } };
  this->m_LabelsArray[140] = { { -1, -1, 0, 0, -1, -1, -1, 0 } };
  this->m_LabelsArray[141] = { { 0, -1, 0, 0, -1, -1, -1, 0 } };
  this->m_LabelsArray[142] = { { -1, 0, 0, 0, -1, -1, -1, 0 } };
  this->m_LabelsArray[143] = { { 0, 0, 0, 0, -1, -1, -1, 0 } };
  this->m_LabelsArray[144] = { { -1, -1, -1, -1, 0, -1, -1, 1 } };
  this->m_LabelsArray[145] = { { 0, -1, -1, -1, 0, -1, -1, 1 } };
  this->m_LabelsArray[146] = { { -1, 0, -1, -1, 1, -1, -1, 2 } };
  this->m_LabelsArray[147] = { { 0, 0, -1, -1, 0, -1, -1, 1 } };
  this->m_LabelsArray[148] = { { -1, -1, 0, -1, 1, -1, -1, 2 } };
  this->m_LabelsArray[149] = { { 0, -1, 0, -1, 0, -1, -1, 1 } };
  this->m_LabelsArray[150] = { { -1, 0, 1, -1, 2, -1, -1, 3 } };
  this->m_LabelsArray[151] = { { 0, 0, 0, -1, 0, -1, -1, 1 } };
  this->m_LabelsArray[152] = { { -1, -1, -1, 0, 1, -1, -1, 0 } };
  this->m_LabelsArray[153] = { { 0, -1, -1, 1, 0, -1, -1, 1 } };
  this->m_LabelsArray[154] = { { -1, 0, -1, 0, 1, -1, -1, 0 } };
  this->m_LabelsArray[155] = { { 0, 0, -1, 0, 0, -1, -1, 0 } };
  this->m_LabelsArray[156] = { { -1, -1, 0, 0, 1, -1, -1, 0 } };
  this->m_LabelsArray[157] = { { 0, -1, 0, 0, 0, -1, -1, 0 } };
  this->m_LabelsArray[158] = { { -1, 0, 0, 0, 1, -1, -1, 0 } };
  this->m_LabelsArray[159] = { { 0, 0, 0, 0, 0, -1, -1, 0 } };
  this->m_LabelsArray[160] = { { -1, -1, -1, -1, -1, 0, -1, 0 } };
  this->m_LabelsArray[161] = { { 0, -1, -1, -1, -1, 1, -1, 1 } };
  this->m_LabelsArray[162] = { { -1, 0, -1, -1, -1, 0, -1, 0 } };
  this->m_LabelsArray[163] = { { 0, 0, -1, -1, -1, 0, -1, 0 } };
  this->m_LabelsArray[164] = { { -1, -1, 0, -1, -1, 1, -1, 1 } };
  this->m_LabelsArray[165] = { { 0, -1, 0, -1, -1, 1, -1, 1 } };
  this->m_LabelsArray[166] = { { -1, 0, 1, -1, -1, 0, -1, 0 } };
  this->m_LabelsArray[167] = { { 0, 0, 0, -1, -1, 0, -1, 0 } };
  this->m_LabelsArray[168] = { { -1, -1, -1, 0, -1, 0, -1, 0 } };
  this->m_LabelsArray[169] = { { 0, -1, -1, 1, -1, 1, -1, 1 } };
  this->m_LabelsArray[170] = { { -1, 0, -1, 0, -1, 0, -1, 0 } };
  this->m_LabelsArray[171] = { { 0, 0, -1, 0, -1, 0, -1, 0 } };
  this->m_LabelsArray[172] = { { -1, -1, 0, 0, -1, 0, -1, 0 } };
  this->m_LabelsArray[173] = { { 0, -1, 0, 0, -1, 0, -1, 0 } };
  this->m_LabelsArray[174] = { { -1, 0, 0, 0, -1, 0, -1, 0 } };
  this->m_LabelsArray[175] = { { 0, 0, 0, 0, -1, 0, -1, 0 } };
  this->m_LabelsArray[176] = { { -1, -1, -1, -1, 0, 0, -1, 0 } };
  this->m_LabelsArray[177] = { { 0, -1, -1, -1, 0, 0, -1, 0 } };
  this->m_LabelsArray[178] = { { -1, 0, -1, -1, 0, 0, -1, 0 } };
  this->m_LabelsArray[179] = { { 0, 0, -1, -1, 0, 0, -1, 0 } };
  this->m_LabelsArray[180] = { { -1, -1, 0, -1, 1, 1, -1, 1 } };
  this->m_LabelsArray[181] = { { 0, -1, 0, -1, 0, 0, -1, 0 } };
  this->m_LabelsArray[182] = { { -1, 0, 1, -1, 0, 0, -1, 0 } };
  this->m_LabelsArray[183] = { { 0, 0, 0, -1, 0, 0, -1, 0 } };
  this->m_LabelsArray[184] = { { -1, -1, -1, 0, 0, 0, -1, 0 } };
  this->m_LabelsArray[185] = { { 0, -1, -1, 0, 0, 0, -1, 0 } };
  this->m_LabelsArray[186] = { { -1, 0, -1, 0, 0, 0, -1, 0 } };
  this->m_LabelsArray[187] = { { 0, 0, -1, 0, 0, 0, -1, 0 } };
  this->m_LabelsArray[188] = { { -1, -1, 0, 0, 0, 0, -1, 0 } };
  this->m_LabelsArray[189] = { { 0, -1, 1, 0, 1, 0, -1, 1 } };
  this->m_LabelsArray[190] = { { -1, 0, 0, 0, 0, 0, -1, 0 } };
  this->m_LabelsArray[191] = { { 0, 0, 0, 0, 0, 0, -1, 0 } };
  this->m_LabelsArray[192] = { { -1, -1, -1, -1, -1, -1, 0, 0 } };
  this->m_LabelsArray[193] = { { 0, -1, -1, -1, -1, -1, 1, 1 } };
  this->m_LabelsArray[194] = { { -1, 0, -1, -1, -1, -1, 1, 1 } };
  this->m_LabelsArray[195] = { { 0, 0, -1, -1, -1, -1, 1, 1 } };
  this->m_LabelsArray[196] = { { -1, -1, 0, -1, -1, -1, 0, 0 } };
  this->m_LabelsArray[197] = { { 0, -1, 0, -1, -1, -1, 0, 0 } };
  this->m_LabelsArray[198] = { { -1, 0, 1, -1, -1, -1, 1, 1 } };
  this->m_LabelsArray[199] = { { 0, 0, 0, -1, -1, -1, 0, 0 } };
  this->m_LabelsArray[200] = { { -1, -1, -1, 0, -1, -1, 0, 0 } };
  this->m_LabelsArray[201] = { { 0, -1, -1, 1, -1, -1, 1, 1 } };
  this->m_LabelsArray[202] = { { -1, 0, -1, 0, -1, -1, 0, 0 } };
  this->m_LabelsArray[203] = { { 0, 0, -1, 0, -1, -1, 0, 0 } };
  this->m_LabelsArray[204] = { { -1, -1, 0, 0, -1, -1, 0, 0 } };
  this->m_LabelsArray[205] = { { 0, -1, 0, 0, -1, -1, 0, 0 } };
  this->m_LabelsArray[206] = { { -1, 0, 0, 0, -1, -1, 0, 0 } };
  this->m_LabelsArray[207] = { { 0, 0, 0, 0, -1, -1, 0, 0 } };
  this->m_LabelsArray[208] = { { -1, -1, -1, -1, 0, -1, 0, 0 } };
  this->m_LabelsArray[209] = { { 0, -1, -1, -1, 0, -1, 0, 0 } };
  this->m_LabelsArray[210] = { { -1, 0, -1, -1, 1, -1, 1, 1 } };
  this->m_LabelsArray[211] = { { 0, 0, -1, -1, 0, -1, 0, 0 } };
  this->m_LabelsArray[212] = { { -1, -1, 0, -1, 0, -1, 0, 0 } };
  this->m_LabelsArray[213] = { { 0, -1, 0, -1, 0, -1, 0, 0 } };
  this->m_LabelsArray[214] = { { -1, 0, 1, -1, 1, -1, 1, 1 } };
  this->m_LabelsArray[215] = { { 0, 0, 0, -1, 0, -1, 0, 0 } };
  this->m_LabelsArray[216] = { { -1, -1, -1, 0, 0, -1, 0, 0 } };
  this->m_LabelsArray[217] = { { 0, -1, -1, 0, 0, -1, 0, 0 } };
  this->m_LabelsArray[218] = { { -1, 0, -1, 0, 0, -1, 0, 0 } };
  this->m_LabelsArray[219] = { { 0, 1, -1, 0, 1, -1, 0, 1 } };
  this->m_LabelsArray[220] = { { -1, -1, 0, 0, 0, -1, 0, 0 } };
  this->m_LabelsArray[221] = { { 0, -1, 0, 0, 0, -1, 0, 0 } };
  this->m_LabelsArray[222] = { { -1, 0, 0, 0, 0, -1, 0, 0 } };
  this->m_LabelsArray[223] = { { 0, 0, 0, 0, 0, -1, 0, 0 } };
  this->m_LabelsArray[224] = { { -1, -1, -1, -1, -1, 0, 0, 0 } };
  this->m_LabelsArray[225] = { { 0, -1, -1, -1, -1, 1, 1, 1 } };
  this->m_LabelsArray[226] = { { -1, 0, -1, -1, -1, 0, 0, 0 } };
  this->m_LabelsArray[227] = { { 0, 0, -1, -1, -1, 0, 0, 0 } };
  this->m_LabelsArray[228] = { { -1, -1, 0, -1, -1, 0, 0, 0 } };
  this->m_LabelsArray[229] = { { 0, -1, 0, -1, -1, 0, 0, 0 } };
  this->m_LabelsArray[230] = { { -1, 0, 0, -1, -1, 0, 0, 0 } };
  this->m_LabelsArray[231] = { { 0, 1, 1, -1, -1, 0, 0, 1 } };
  this->m_LabelsArray[232] = { { -1, -1, -1, 0, -1, 0, 0, 0 } };
  this->m_LabelsArray[233] = { { 0, -1, -1, 1, -1, 1, 1, 1 } };
  this->m_LabelsArray[234] = { { -1, 0, -1, 0, -1, 0, 0, 0 } };
  this->m_LabelsArray[235] = { { 0, 0, -1, 0, -1, 0, 0, 0 } };
  this->m_LabelsArray[236] = { { -1, -1, 0, 0, -1, 0, 0, 0 } };
  this->m_LabelsArray[237] = { { 0, -1, 0, 0, -1, 0, 0, 0 } };
  this->m_LabelsArray[238] = { { -1, 0, 0, 0, -1, 0, 0, 0 } };
  this->m_LabelsArray[239] = { { 0, 0, 0, 0, -1, 0, 0, 0 } };
  this->m_LabelsArray[240] = { { -1, -1, -1, -1, 0, 0, 0, 0 } };
  this->m_LabelsArray[241] = { { 0, -1, -1, -1, 0, 0, 0, 0 } };
  this->m_LabelsArray[242] = { { -1, 0, -1, -1, 0, 0, 0, 0 } };
  this->m_LabelsArray[243] = { { 0, 0, -1, -1, 0, 0, 0, 0 } };
  this->m_LabelsArray[244] = { { -1, -1, 0, -1, 0, 0, 0, 0 } };
  this->m_LabelsArray[245] = { { 0, -1, 0, -1, 0, 0, 0, 0 } };
  this->m_LabelsArray[246] = { { -1, 0, 0, -1, 0, 0, 0, 0 } };
  this->m_LabelsArray[247] = { { 0, 0, 0, -1, 0, 0, 0, 0 } };
  this->m_LabelsArray[248] = { { -1, -1, -1, 0, 0, 0, 0, 0 } };
  this->m_LabelsArray[249] = { { 0, -1, -1, 0, 0, 0, 0, 0 } };
  this->m_LabelsArray[250] = { { -1, 0, -1, 0, 0, 0, 0, 0 } };
  this->m_LabelsArray[251] = { { 0, 0, -1, 0, 0, 0, 0, 0 } };
  this->m_LabelsArray[252] = { { -1, -1, 0, 0, 0, 0, 0, 0 } };
  this->m_LabelsArray[253] = { { 0, -1, 0, 0, 0, 0, 0, 0 } };
  this->m_LabelsArray[254] = { { -1, 0, 0, 0, 0, 0, 0, 0 } };
  this->m_LabelsArray[255] = { { 0, 0, 0, 0, 0, 0, 0, 0 } };
}

template <typename TInputImage, typename TOutputMesh, typename TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage, TOutputMesh, TInterpolator>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent
     << "IsoSurfaceValue: " << static_cast<typename NumericTraits<InputPixelType>::PrintType>(m_IsoSurfaceValue)
     << std::endl;
  os << indent << "MaxSpacing: " << static_cast<typename NumericTraits<SpacingValueType>::PrintType>(m_MaxSpacing)
     << std::endl;
  os << indent << "GenerateTriangleFaces: " << static_cast<NumericTraits<bool>::PrintType>(m_GenerateTriangleFaces)
     << std::endl;
  os << indent
     << "ProjectVerticesToIsoSurface: " << static_cast<NumericTraits<bool>::PrintType>(m_ProjectVerticesToIsoSurface)
     << std::endl;
  os << indent << "ProjectVertexSurfaceDistanceThreshold: " << m_ProjectVertexSurfaceDistanceThreshold << std::endl;
  os << indent << "ProjectVertexStepLength: " << m_ProjectVertexStepLength << std::endl;
  os << indent << "ProjectVertexStepLengthRelaxationFactor: " << m_ProjectVertexStepLengthRelaxationFactor << std::endl;
  os << indent << "ProjectVertexMaximumNumberOfSteps: " << m_ProjectVertexMaximumNumberOfSteps << std::endl;
}
} // end namespace itk
#endif
