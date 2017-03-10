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
#ifndef itkCuberilleImageToMeshFilter_hxx
#define itkCuberilleImageToMeshFilter_hxx

#define SWAP(x,y) unsigned int t;t=x;x=y;y=t;

#include "itkCuberilleImageToMeshFilter.h"
#include "itkNumericTraits.h"
#include "itkConnectedComponentAlgorithm.h"

namespace itk
{

template<class TInputImage, class TOutputMesh, class TInterpolator>
CuberilleImageToMeshFilter<TInputImage,TOutputMesh,TInterpolator>
::CuberilleImageToMeshFilter() :
  m_IsoSurfaceValue( NumericTraits< InputPixelType >::One ),
  m_MaxSpacing( NumericTraits< SpacingValueType >::One ),
  m_GenerateTriangleFaces( true ),
  m_ProjectVerticesToIsoSurface( true ),
  m_ProjectVertexSurfaceDistanceThreshold( 0.5 ),
  m_ProjectVertexStepLength( -1.0 ),
  m_ProjectVertexStepLengthRelaxationFactor( 0.95 ),
  m_ProjectVertexMaximumNumberOfSteps( 50 )
{
  this->SetNumberOfRequiredInputs(1);
}

template<class TInputImage, class TOutputMesh, class TInterpolator>
CuberilleImageToMeshFilter<TInputImage,TOutputMesh,TInterpolator>
::~CuberilleImageToMeshFilter()
{
  m_GradientInterpolator = ITK_NULLPTR;
}

template<class TInputImage, class TOutputMesh, class TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage,TOutputMesh,TInterpolator>
::SetInput(const InputImageType* image)
{
  this->ProcessObject::SetNthInput(0, const_cast< InputImageType * >( image ) );
}

template<class TInputImage, class TOutputMesh, class TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage,TOutputMesh,TInterpolator>
::GenerateData()
{
#if DEBUG_PRINT
  m_ProjectVertexTerminate[0] = 0;
  m_ProjectVertexTerminate[1] = 0;
  m_ProjectVertexTerminate[2] = 0;
#endif

  // Get input/output
  InputImageConstPointer image = Superclass::GetInput( 0 );
  typename OutputMeshType::Pointer mesh = Superclass::GetOutput();

  // Compute maximum spacing
  m_MaxSpacing = image->GetSpacing()[0];
  for( unsigned int i = 1; i < InputImageType::ImageDimension; ++i )
    {
    m_MaxSpacing = vnl_math_max( m_MaxSpacing, image->GetSpacing()[i] );
    }

  // Set default step length
  if( m_ProjectVertexStepLength < 0.0 )
    {
    m_ProjectVertexStepLength = m_MaxSpacing * 0.25;
    }

  // Create interpolator for pixel value
  if( m_Interpolator.IsNull() )
    {
    m_Interpolator = InterpolatorType::New();
    }
  m_Interpolator->SetInputImage( image );

  // Create interpolator for gradient image
  ComputeGradientImage();

  // Set up iterator
  typename InputImageIteratorType::SizeType radius; radius.Fill( 1 );
  InputImageIteratorType it( radius, image, image->GetBufferedRegion() );
  setConnectivity<InputImageIteratorType>( &it, false ); // Set face connectivity

  // TODO: Estimate number of vertices/faces for which to reserve
  // TODO: There is an issue with quad edge mesh related to reserving, see
  // http://www.itk.org/mailman/private/insight-developers/2010-June/014653.html
  //SizeType size = image->GetLargestPossibleRegion().GetSize();
  //mesh->GetPoints()->Reserve( (size[0]*size[1]*size[2]) / 200 );
  //mesh->GetCells()->Reserve( (size[0]*size[1]*size[2]) / 100 );

  // Set up helper structures
  unsigned int look, look0, look1 = 0;
  unsigned char numFaces = 0;
  bool faceHasQuad[6];
  bool vertexHasQuad[8];
  PointIdentifier v[8];
  PointIdentifier f[4];
  PointIdentifier nextVertexId = 0;
  CellIdentifier nextCellId = 0;
  IndexType index;
  typename IndexType::IndexValueType lastZ = -1;
  InputPixelType center = 0;
  typename InputImageIteratorType::OffsetType offset[6];
  offset[0][0] = -1; offset[0][1] = +0; offset[0][2] = +0;
  offset[1][0] = +0; offset[1][1] = -1; offset[1][2] = +0;
  offset[2][0] = +1; offset[2][1] = +0; offset[2][2] = +0;
  offset[3][0] = +0; offset[3][1] = +1; offset[3][2] = +0;
  offset[4][0] = +0; offset[4][1] = +0; offset[4][2] = -1;
  offset[5][0] = +0; offset[5][1] = +0; offset[5][2] = +1;
  VertexLookupMapType lookup[2];
  look0 = 1; look1 = 0;
  lookup[look0].Clear();
  lookup[look1].Clear();

  // TODO: Handle voxels on the edge of the image

  // Iterate input image
  for ( it.GoToBegin(); !it.IsAtEnd(); ++it )
    {
    // Determine if current pixel is suitable
    center = it.GetCenterPixel();
    if( center < m_IsoSurfaceValue )
      {
      continue;
      }

    // Re-initialize for new pixel
    numFaces = 0;
    for( unsigned int i = 0; i < 6; ++i )
      {
      faceHasQuad[i] = false;
      }
    for( unsigned int i = 0; i < 8; ++i )
      {
      vertexHasQuad[i] = false;
      }

    // Re-initialize for new z plane
    index = it.GetIndex();
    if( index[2] != lastZ )
      {
      SWAP( look0, look1 );
      lookup[look1].Clear();
      lastZ = index[2];
      }

    // Compute which faces (if any) have quads
    for( unsigned int i = 0; i < 6; ++i )
      {
      // NOTE: suitability check above means center <= m_IsoSurfaceValue
      faceHasQuad[i] = ( it.GetPixel( offset[i] ) < m_IsoSurfaceValue );
      if( faceHasQuad[i] )
        {
        numFaces++;
        SetVerticesFromFace( i, &vertexHasQuad[0] );
        }
      }

    // Process each face
    if( numFaces > 0 )
      {
      // Create vertices
      for( unsigned int i = 0; i < 8; ++i )
        {
        if( vertexHasQuad[i] )
          {
          // Use the vertex lookup to get the vertex for the correct slice
          IndexType vindex = GetVertexLookupIndex( i, index );
          look = ( i < 4 ) ? look0 : look1; // First four are first slice
          if( !lookup[look].GetVertex(vindex[0], vindex[1], v[i]) )
            {
            // Vertex was not in lookup, create and add to lookup
            v[i] = nextVertexId;
            AddVertex( nextVertexId, vindex, image, mesh );
            lookup[look].AddVertex( vindex[0], vindex[1], v[i] );
            }
          } // end if vertex has quad
        } // end foreach vertex

      // Create faces
      if ( faceHasQuad[0] ) { f[0] = v[0]; f[1] = v[4]; f[2] = v[7]; f[3] = v[3]; AddQuadFace(nextCellId, f, mesh); }
      if ( faceHasQuad[1] ) { f[0] = v[0]; f[1] = v[1]; f[2] = v[5]; f[3] = v[4]; AddQuadFace(nextCellId, f, mesh); }
      if ( faceHasQuad[2] ) { f[0] = v[1]; f[1] = v[2]; f[2] = v[6]; f[3] = v[5]; AddQuadFace(nextCellId, f, mesh); }
      if ( faceHasQuad[3] ) { f[0] = v[2]; f[1] = v[3]; f[2] = v[7]; f[3] = v[6]; AddQuadFace(nextCellId, f, mesh); }
      if ( faceHasQuad[4] ) { f[0] = v[0]; f[1] = v[3]; f[2] = v[2]; f[3] = v[1]; AddQuadFace(nextCellId, f, mesh); }
      if ( faceHasQuad[5] ) { f[0] = v[4]; f[1] = v[5]; f[2] = v[6]; f[3] = v[7]; AddQuadFace(nextCellId, f, mesh); }

      } // end if num faces > 0

    }

#if DEBUG_PRINT
  std::cout << "Number of terminations due to surface distance threshold: " << m_ProjectVertexTerminate[0] << std::endl;
  std::cout << "Number of terminations due to maximum number of steps: " << m_ProjectVertexTerminate[1] << std::endl;
#if USE_ADVANCED_PROJECTION
  std::cout << "Number of terminations due to too many sign swaps: " << m_ProjectVertexTerminate[2] << std::endl;
#endif
#endif

}

template<class TInputImage, class TOutputMesh, class TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage,TOutputMesh,TInterpolator>
::SetVerticesFromFace( unsigned int face, bool *v )
{
  switch ( face )
  {
  case 0: v[0] = true; v[4] = true; v[7] = true; v[3] = true; break;
  case 1: v[0] = true; v[1] = true; v[5] = true; v[4] = true; break;
  case 2: v[1] = true; v[2] = true; v[6] = true; v[5] = true; break;
  case 3: v[2] = true; v[3] = true; v[7] = true; v[6] = true; break;
  case 4: v[0] = true; v[3] = true; v[2] = true; v[1] = true; break;
  case 5: v[4] = true; v[5] = true; v[6] = true; v[7] = true; break;
  }
}

template<class TInputImage, class TOutputMesh, class TInterpolator>
typename TInputImage::IndexType
CuberilleImageToMeshFilter<TInputImage,TOutputMesh,TInterpolator>
::GetVertexLookupIndex( unsigned int vertex, typename TInputImage::IndexType index )
{
  IndexType result( index );
  switch ( vertex )
  {
  case 0: break;
  case 1: result[0] += 1; break;
  case 2: result[0] += 1; result[1] += 1; break;
  case 3: result[1] += 1; break;
  case 4: result[2] += 1; break;
  case 5: result[0] += 1; result[2] += 1; break;
  case 6: result[0] += 1; result[1] += 1; result[2] += 1; break;
  case 7: result[1] += 1; result[2] += 1; break;
  }
  return result;
}

template<class TInputImage, class TOutputMesh, class TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage,TOutputMesh,TInterpolator>
::AddVertex( typename TOutputMesh::PointIdentifier &id,
             typename TInputImage::IndexType index,
             const TInputImage* image,
             TOutputMesh* mesh )
{
  PointType vertex;
  image->TransformIndexToPhysicalPoint( index, vertex );
  SpacingType spacing = image->GetSpacing();
  vertex[0] -= ( spacing[0] / 2.0 );
  vertex[1] -= ( spacing[1] / 2.0 );
  vertex[2] -= ( spacing[2] / 2.0 );
  if( m_ProjectVerticesToIsoSurface )
    {
    ProjectVertexToIsoSurface( vertex );
    }
  mesh->GetPoints()->InsertElement( id++, vertex );
}

template<class TInputImage, class TOutputMesh, class TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage,TOutputMesh,TInterpolator>
::AddQuadFace( typename TOutputMesh::CellIdentifier &id,
               typename TOutputMesh::PointIdentifier face[4],
               TOutputMesh* mesh )
{
  if( m_GenerateTriangleFaces )
    {
    // Get vertices
    PointType v[4];
    for( unsigned int i = 0; i < 4; ++i )
      {
      v[i] = mesh->GetPoints()->GetElement( face[i] );
      }

    // Split the quad along the longest edge to avoid skinny triangles
    PointIdentifier face1[3];
    PointIdentifier face2[3];
    if( v[0].SquaredEuclideanDistanceTo(v[2]) >= v[1].SquaredEuclideanDistanceTo(v[3]) )
      {
      face1[0] = face[0]; face1[1] = face[1]; face1[2] = face[3];
      face2[0] = face[1]; face2[1] = face[2]; face2[2] = face[3];
      }
    else
      {
      face1[0] = face[0]; face1[1] = face[1]; face1[2] = face[2];
      face2[0] = face[0]; face2[1] = face[2]; face2[2] = face[3];
      }

    // Add triangle 1 cell
    TriangleCellAutoPointer tri1;
    tri1.TakeOwnership( new TriangleCellType );
    tri1->SetPointIds( face1 );
    mesh->SetCell( id++, tri1 );
    //mesh->SetCellData( id, (OutputPixelType)0 );

    // Add triangle 2 cell
    TriangleCellAutoPointer tri2;
    tri2.TakeOwnership( new TriangleCellType );
    tri2->SetPointIds( face2 );
    mesh->SetCell( id++, tri2 );
    //mesh->SetCellData( id, (OutputPixelType)0 );
    }
  else
    {
    // Add quateraleral cell
    QuadrilateralCellAutoPointer quad1;
    quad1.TakeOwnership( new QuadrilateralCellType );
    quad1->SetPointIds( face );
    mesh->SetCell( id++, quad1 );
    //mesh->SetCellData( id, (OutputPixelType)0 );
    }
}

template<class TInputImage, class TOutputMesh, class TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage,TOutputMesh,TInterpolator>
::ProjectVertexToIsoSurface( PointType &vertex )
{
#if USE_ADVANCED_PROJECTION
  // Set up
  bool done = false;
  double step = m_ProjectVertexStepLength;
  unsigned int numberOfSteps = 0;
  PointType temp[2];
  PointType tempBest;
  InterpolatorOutputType value[2];
  double diff[2];
  double diffBest = -1.0;
  unsigned int i, swaps = 0;
  int previousi = -1;

  while( !done )
    {
    // Compute normal vector
    GradientPixelType normal = m_GradientInterpolator->Evaluate(vertex);
    if( normal.Normalize() == 0.0 ) //old norm was zero
      {
      break;
      }

    // Step along both directions of normal
    for( i = 0; i < InputImageType::ImageDimension; ++i )
      {
      temp[0][i] = vertex[i] + ( normal[i] * +1.0 * step );
      temp[1][i] = vertex[i] + ( normal[i] * -1.0 * step );
      }
    step *= m_ProjectVertexStepLengthRelaxationFactor;

    // Compute which direction moves vertex closer to iso-surface value
    value[0] = m_Interpolator->Evaluate( temp[0] );
    value[1] = m_Interpolator->Evaluate( temp[1] );
    diff[0] = vnl_math_abs( value[0] - m_IsoSurfaceValue );
    diff[1] = vnl_math_abs( value[1] - m_IsoSurfaceValue );
    i = ( diff[0] <= diff[1] ) ? 0 : 1;
    if( previousi < 0 )
      {
      previousi = i;
      }
    swaps += (int)( previousi != i );
    vertex = temp[i];

    // Determine whether vertex is close enough to iso-surface value
    done |= diff[i] < m_ProjectVertexSurfaceDistanceThreshold;
#if DEBUG_PRINT
    if( done )
      {
      m_ProjectVertexTerminate[0]++;
      }
#endif
    if( done )
      {
      break;
      }

    // Determine whether we have done enough steps
    done |= numberOfSteps++ > m_ProjectVertexMaximumNumberOfSteps;
#if DEBUG_PRINT
    if( done )
      {
      m_ProjectVertexTerminate[1]++;
      }
#endif
    if( done )
      {
      break;
      }

    // Determine whether there has been too many sign swaps (oscillating)
    done |= ( swaps >= 5 );
#if DEBUG_PRINT
    if( done )
      {
      m_ProjectVertexTerminate[2]++;
      }
#endif
    if( done )
      {
      break;
      }
  }
#elif USE_LINESEARCH_PROJECTION

  // Set up
  unsigned int k;
  GradientPixelType normal;
  InterpolatorOutputType value;
  PointType temp, bestVertex;
  double sign, d, metric, bestMetric = 10000;

  // Compute normal vector
  normal = m_GradientInterpolator->Evaluate( vertex );
  if( normal.Normalize() == 0.0 ) //old norm was zero
    {
    break;
    }

  // Search on both sides of the line
  for( sign = -1.0; sign <= 1.0; sign += 2.0 )
    {
    k = ( sign == -1.0 ) ? 0 : 1;
    for( unsigned int j = 1; j < m_ProjectVertexMaximumNumberOfSteps/2; ++j )
      {
      // Compute current location along line
      d = (double)j / ( (double)m_ProjectVertexMaximumNumberOfSteps / 2.0 );
      for( unsigned int i = 0; i < InputImageType::ImageDimension; ++i )
        {
        temp[i] = vertex[i] + ( normal[i] * sign * m_ProjectVertexStepLength * d );
        }
      // Compute metric (combination of difference and distance)
      value = m_Interpolator->Evaluate( temp );
      metric = vnl_math_abs( value - m_IsoSurfaceValue ); // Difference
      //metric /= NumericTraits<InputPixelType>::max(); // Normalized difference
      //metric /= d; // Distance

      // Determine if current position is the "best"
      if( metric < bestMetric )
        {
        bestMetric = metric;
        bestVertex = temp;
        }
      }
    }
  vertex = bestVertex;

#else
  // Set up
  bool done = false;
  double sign = 1.0;
  double step = m_ProjectVertexStepLength;
  unsigned int numberOfSteps = 0;
  GradientPixelType normal;
  InterpolatorOutputType value;

  while( !done )
    {
    // Compute normal vector
    normal = m_GradientInterpolator->Evaluate( vertex );
    if( normal.Normalize() == 0.0 ) //old norm was zero
      {
      break;
      }

    // Compute whether vertex is close enough to iso-surface value
    value = m_Interpolator->Evaluate(vertex);
    done |= vnl_math_abs( value - m_IsoSurfaceValue ) < m_ProjectVertexSurfaceDistanceThreshold;
#if DEBUG_PRINT
    if( done )
      {
      m_ProjectVertexTerminate[0]++;
      }
#endif
    if( done )
      {
      break;
      }

    // Step along the normal towards the iso-surface value
    sign = ( value < m_IsoSurfaceValue ) ? +1.0 : -1.0;
    for( unsigned int i = 0; i < InputImageType::ImageDimension; ++i )
      {
      vertex[i] += ( normal[i] * sign * step );
      }
    step *= m_ProjectVertexStepLengthRelaxationFactor;
    done |= numberOfSteps++ > m_ProjectVertexMaximumNumberOfSteps;
#if DEBUG_PRINT
    if( done )
      {
      m_ProjectVertexTerminate[1]++;
      }
#endif
    }
#endif

}

template<class TInputImage, class TOutputMesh, class TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage,TOutputMesh,TInterpolator>
::ComputeGradientImage()
{
  if( m_ProjectVerticesToIsoSurface && m_GradientInterpolator.IsNull() )
    {
    typename GradientFilterType::Pointer gradientFilter = GradientFilterType::New();
    gradientFilter->SetInput( Superclass::GetInput(0) );
#if USE_GRADIENT_RECURSIVE_GAUSSIAN
    gradientFilter->SetSigma( m_MaxSpacing * 1.0 );
    gradientFilter->SetNormalizeAcrossScale( true );
#endif
    gradientFilter->Update();
    m_GradientInterpolator = GradientInterpolatorType::New();
    m_GradientInterpolator->SetInputImage( gradientFilter->GetOutput( ) );
    gradientFilter->GetOutput()->DisconnectPipeline();
    gradientFilter = ITK_NULLPTR;
    }
}

template<class TInputImage, class TOutputMesh, class TInterpolator>
void
CuberilleImageToMeshFilter<TInputImage,TOutputMesh,TInterpolator>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);

  os << indent
    << "IsoSurfaceValue: "
    << static_cast< typename NumericTraits<InputPixelType>::PrintType >( m_IsoSurfaceValue )
    << std::endl;
  os << indent
    << "MaxSpacing: "
    << static_cast< typename NumericTraits<SpacingValueType>::PrintType >( m_MaxSpacing )
    << std::endl;
  os << indent
    << "GenerateTriangleFaces: "
    << static_cast< NumericTraits<bool>::PrintType >( m_GenerateTriangleFaces )
    << std::endl;
  os << indent
    << "ProjectVerticesToIsoSurface: "
    << static_cast< NumericTraits<bool>::PrintType >( m_ProjectVerticesToIsoSurface )
    << std::endl;
  os << indent << "ProjectVertexSurfaceDistanceThreshold: "
    << m_ProjectVertexSurfaceDistanceThreshold << std::endl;
  os << indent << "ProjectVertexStepLength: "
    << m_ProjectVertexStepLength << std::endl;
  os << indent << "ProjectVertexStepLengthRelaxationFactor: "
    << m_ProjectVertexStepLengthRelaxationFactor << std::endl;
  os << indent << "ProjectVertexMaximumNumberOfSteps: "
    << m_ProjectVertexMaximumNumberOfSteps << std::endl;
}
} // end namespace itk
#endif
