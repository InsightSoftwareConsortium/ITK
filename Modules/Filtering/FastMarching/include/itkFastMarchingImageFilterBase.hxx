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

#ifndef itkFastMarchingImageFilterBase_hxx
#define itkFastMarchingImageFilterBase_hxx

#include "itkFastMarchingImageFilterBase.h"

#include "itkImageRegionIterator.h"
#include "itkConnectedComponentImageFilter.h"
#include "itkRelabelComponentImageFilter.h"

namespace itk
{

template< typename TInput, typename TOutput >
class
FastMarchingImageFilterBase< TInput, TOutput >:: InternalNodeStructure
{
public:
  InternalNodeStructure( ) :
    m_Value( NumericTraits< OutputPixelType >::max() ), m_Axis( 0 ) {}

  NodeType        m_Node;
  OutputPixelType m_Value;
  unsigned int    m_Axis;

  bool operator< ( const InternalNodeStructure& iRight ) const
    {
    return m_Value < iRight.m_Value;
    }
};

template< typename TInput, typename TOutput >
FastMarchingImageFilterBase< TInput, TOutput >::
FastMarchingImageFilterBase() :
  m_OverrideOutputInformation( false ),
  m_LabelImage( LabelImageType::New() )
{
  m_StartIndex.Fill(0);
  m_LastIndex.Fill(0);

  OutputSizeType outputSize;
  outputSize.Fill(16);

  NodeType outputIndex;
  outputIndex.Fill(0);

  m_OutputRegion.SetSize(outputSize);
  m_OutputRegion.SetIndex(outputIndex);

  m_OutputOrigin.Fill(0.0);
  m_OutputSpacing.Fill(1.0);
  m_OutputDirection.SetIdentity();

  m_InputCache = ITK_NULLPTR;
}

template< typename TInput, typename TOutput >
FastMarchingImageFilterBase< TInput, TOutput >::
~FastMarchingImageFilterBase()
{
}

template< typename TInput, typename TOutput >
void
FastMarchingImageFilterBase< TInput, TOutput >::
GenerateOutputInformation()
{
  // Copy output information from input image
  Superclass::GenerateOutputInformation();

  // Use user-specified output information
  if ( !this->GetInput() || m_OverrideOutputInformation )
    {
    OutputImagePointer output = this->GetOutput();
    output->SetLargestPossibleRegion(m_OutputRegion);
    output->SetOrigin(m_OutputOrigin);
    output->SetSpacing(m_OutputSpacing);
    output->SetDirection(m_OutputDirection);
    }
}

template< typename TInput, typename TOutput >
void
FastMarchingImageFilterBase< TInput, TOutput >::
EnlargeOutputRequestedRegion( DataObject *output)
{
  // Enlarge the requested region of the output to the whole data set
  OutputImageType *imgData = dynamic_cast< OutputImageType * >( output );
  if ( imgData )
    {
    imgData->SetRequestedRegionToLargestPossibleRegion();
    }
  else
    {
    // Pointer could not be cast to TLevelSet *
    itkWarningMacro( << "itk::FastMarchingImageFilter"
                     << "::EnlargeOutputRequestedRegion cannot cast "
                     << typeid( output ).name() << " to "
                     << typeid( OutputImageType * ).name() );
    }
}

template< typename TInput, typename TOutput >
IdentifierType
FastMarchingImageFilterBase< TInput, TOutput >::
GetTotalNumberOfNodes() const
{
  return this->m_BufferedRegion.GetNumberOfPixels();
}

template< typename TInput, typename TOutput >
void
FastMarchingImageFilterBase< TInput, TOutput >::
SetOutputValue( OutputImageType* oImage,
               const NodeType& iNode,
               const OutputPixelType& iValue )
{
  return oImage->SetPixel( iNode, iValue );
}

template< typename TInput, typename TOutput >
const
typename
FastMarchingImageFilterBase< TInput, TOutput >::
OutputPixelType
FastMarchingImageFilterBase< TInput, TOutput >::
GetOutputValue( OutputImageType* oImage, const NodeType& iNode ) const
{
  return oImage->GetPixel( iNode );
}

template< typename TInput, typename TOutput >
unsigned char
FastMarchingImageFilterBase< TInput, TOutput >::
GetLabelValueForGivenNode( const NodeType& iNode ) const
{
  return m_LabelImage->GetPixel( iNode );
}

template< typename TInput, typename TOutput >
void
FastMarchingImageFilterBase< TInput, TOutput >::
SetLabelValueForGivenNode( const NodeType& iNode, const LabelType& iLabel )
{
  m_LabelImage->SetPixel( iNode, iLabel );
}

template< typename TInput, typename TOutput >
void
FastMarchingImageFilterBase< TInput, TOutput >::
UpdateNeighbors( OutputImageType* oImage, const NodeType& iNode )
{
  NodeType neighIndex = iNode;

  unsigned char label;

  typename NodeType::IndexValueType v, start, last;

  int s;

  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    v = iNode[j];
    start = m_StartIndex[j];
    last = m_LastIndex[j];

    for( s = -1; s < 2; s+= 2 )
      {
      if ( ( v > start ) && ( v < last ) )
        {
        neighIndex[j] = v + s;
        }
      label = m_LabelImage->GetPixel(neighIndex);

      if ( ( label != Traits::Alive ) &&
           ( label != Traits::InitialTrial ) &&
           ( label != Traits::Forbidden ) )
        {
        this->UpdateValue( oImage, neighIndex );
        }
      }

    // Reset neighIndex
    neighIndex[j] = v;
    }
}

template< typename TInput, typename TOutput >
void
FastMarchingImageFilterBase< TInput, TOutput >::
UpdateValue( OutputImageType* oImage, const NodeType& iNode )
  {
    InternalNodeStructureArray NodesUsed;

  GetInternalNodesUsed( oImage, iNode, NodesUsed );

  OutputPixelType outputPixel =
      static_cast< OutputPixelType >( Solve( oImage, iNode, NodesUsed ) );

  if ( outputPixel < this->m_LargeValue )
    {
    this->SetOutputValue( oImage, iNode, outputPixel );

    this->SetLabelValueForGivenNode( iNode, Traits::Trial );

    // Insert point into trial heap
    this->m_Heap.push( NodePairType( iNode, outputPixel ) );
    }
}

template< typename TInput, typename TOutput >
void
FastMarchingImageFilterBase< TInput, TOutput >::
GetInternalNodesUsed( OutputImageType* oImage,
                      const NodeType& iNode,
                      InternalNodeStructureArray& ioNodesUsed )
{
  NodeType neighbor_node = iNode;

  OutputPixelType neighValue;

  // Just to make sure the index is initialized (really cautious)
  InternalNodeStructure temp_node;
  temp_node.m_Node = iNode;

  typename NodeType::IndexValueType v, start, last, temp;

  int s;

  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    temp_node.m_Value = this->m_LargeValue;

    v = iNode[j];
    start = m_StartIndex[j];
    last = m_LastIndex[j];

    // Find smallest valued neighbor in this dimension
    for ( s = -1; s < 2; s = s + 2 )
      {
      temp = v + s;

      // Make sure neighIndex is not outside from the image
      if ( ( temp <= last ) && ( temp >= start ) )
        {
        neighbor_node[j] = temp;

        if ( this->GetLabelValueForGivenNode( neighbor_node ) == Traits::Alive )
          {
          neighValue = this->GetOutputValue( oImage, neighbor_node );

          // let's find the minimum value given a direction j
          if ( temp_node.m_Value > neighValue )
            {
            temp_node.m_Value = neighValue;
            temp_node.m_Node = neighbor_node;
            }
          }
        }
      }

    // Put the minimum neighbor onto the heap
    temp_node.m_Axis = j;
    ioNodesUsed[j] = temp_node;

    // Reset neighIndex
    neighbor_node[j] = v;
    }
}

template< typename TInput, typename TOutput >
double
FastMarchingImageFilterBase< TInput, TOutput >::
Solve( OutputImageType* oImage,
      const NodeType& iNode,
      InternalNodeStructureArray& iNeighbors ) const
{
  (void) oImage;

  // Sort the local list
  std::sort( iNeighbors.Begin(), iNeighbors.End() );

  double oSolution = NumericTraits< double >::max();

  double aa( 0.0 );
  double bb( 0.0 );
  double cc( this->m_InverseSpeed );

  if ( m_InputCache )
    {
    cc = static_cast< double >( m_InputCache->GetPixel(iNode) ) /
        this->m_NormalizationFactor;
#if defined(__APPLE__) && (__clang_major__ == 3) && (__clang_minor__ == 0) && defined(NDEBUG) && defined(__x86_64__)
    cc = -1.0 * itk::Math::sqr(1.0 / (cc + itk::Math::eps) );
#else
    cc = -1.0 * itk::Math::sqr(1.0 / cc);
#endif
    }

  double discrim = 0.;
  double value = 0.;
  double spaceFactor = 0.;
  unsigned int axis = 0;

  typename InternalNodeStructureArray::Iterator
      n_it = iNeighbors.Begin();

  while( n_it != iNeighbors.End() )
    {
    value = static_cast< double >( n_it->m_Value );

    if ( oSolution >= value )
      {
      axis = n_it->m_Axis;

      // spaceFactor = \frac{1}{spacing[axis]^2}
      spaceFactor = itk::Math::sqr(1.0 / m_OutputSpacing[axis]);

      aa += spaceFactor;
      bb += value * spaceFactor;
      cc += itk::Math::sqr(value) * spaceFactor;

      discrim = itk::Math::sqr(bb) - aa * cc;

      if ( discrim < itk::Math::eps )
        {
        // Discriminant of quadratic eqn. is negative
        itkExceptionMacro(
          <<"Discriminant of quadratic equation is negative" );
        }

      oSolution = ( std::sqrt(discrim) + bb ) / aa;
      }
    else
      {
      break;
      }
    ++n_it;
    }

  return oSolution;
}

template< typename TInput, typename TOutput >
bool
FastMarchingImageFilterBase< TInput, TOutput >::
CheckTopology( OutputImageType* oImage, const NodeType& iNode )
{
  if( this->m_TopologyCheck != Superclass::Nothing )
    {
    if( ( ImageDimension == 2 ) || ( ImageDimension == 3 ) )
      {
      bool wellComposednessViolation
        = this->DoesVoxelChangeViolateWellComposedness( iNode );

      bool strictTopologyViolation
         = this->DoesVoxelChangeViolateStrictTopology( iNode );

      if( ( this->m_TopologyCheck == Superclass::Strict ) &&
          ( wellComposednessViolation || strictTopologyViolation ) )
        {
        oImage->SetPixel( iNode, this->m_TopologyValue );
        this->m_LabelImage->SetPixel( iNode, Traits::Topology );
        return false;
        }

      if( this->m_TopologyCheck == Superclass::NoHandles )
        {
        if( wellComposednessViolation )
          {
          oImage->SetPixel( iNode, this->m_TopologyValue );
          m_LabelImage->SetPixel( iNode, Traits::Topology );
          return false;
          }
        if( strictTopologyViolation )
          {
          // Check for handles
          typename NeighborhoodIteratorType::RadiusType radius;
          radius.Fill( 1 );
          NeighborhoodIteratorType ItL( radius, this->m_LabelImage,
            this->m_LabelImage->GetBufferedRegion() );
          ItL.SetLocation( iNode );

          NeighborhoodIterator<ConnectedComponentImageType>
              ItC( radius, this->m_ConnectedComponentImage,
                this->m_ConnectedComponentImage->GetBufferedRegion() );
          ItC.SetLocation( iNode );

          unsigned int minLabel = 0;
          unsigned int otherLabel = 0;

          bool doesChangeCreateHandle = false;

          for( unsigned int d = 0; d < ImageDimension; d++ )
            {
            if( ItL.GetNext( d ) == Traits::Alive &&
                ItL.GetPrevious( d ) == Traits::Alive )
              {
              if( ItC.GetNext( d ) == ItC.GetPrevious( d ) )
                {
                doesChangeCreateHandle = true;
                }
              else
                {
                minLabel = std::min( ItC.GetNext( d ),
                                         ItC.GetPrevious( d ) );
                otherLabel = std::max( ItC.GetNext( d ),
                                           ItC.GetPrevious( d ) );
                }
              break;
              }
            }
          if( doesChangeCreateHandle )
            {
            oImage->SetPixel( iNode, this->m_TopologyValue );
            this->m_LabelImage->SetPixel( iNode, Traits::Topology );
            return false;
            }
          else
            {
            ItC.GoToBegin();

            while( !ItC.IsAtEnd() )
              {
              if( ItC.GetCenterPixel() == otherLabel )
                {
                ItC.SetCenterPixel( minLabel );
                }
              ++ItC;
              }
            }
          }
        }
      }
    else
      {
      itkWarningMacro( << "CheckTopology has not be implemented for Dimension != 2 and != 3."
                    << "m_TopologyCheck should be set to Nothing." );
      }
    }
  return true;
}

template< typename TInput, typename TOutput >
void FastMarchingImageFilterBase< TInput, TOutput >::
InitializeOutput( OutputImageType* oImage )
{
  // Allocate memory for the output buffer
  oImage->SetBufferedRegion( oImage->GetRequestedRegion() );
  oImage->Allocate();
  oImage->FillBuffer( this->m_LargeValue );

  // Cache some buffered region information
  m_BufferedRegion = oImage->GetBufferedRegion();
  m_StartIndex = m_BufferedRegion.GetIndex();
  m_LastIndex = m_StartIndex + m_BufferedRegion.GetSize();

  m_OutputSpacing = oImage->GetSpacing();
  m_OutputOrigin = oImage->GetOrigin();
  m_OutputDirection = oImage->GetDirection();

  typename OutputImageType::OffsetType offset;
  offset.Fill(1);
  m_LastIndex -= offset;

  // Checking for handles only requires an image to keep track of
  // connected components.
  if( this->m_TopologyCheck == Superclass::NoHandles )
    {
    m_ConnectedComponentImage = ConnectedComponentImageType::New();
    m_ConnectedComponentImage->SetOrigin( m_OutputOrigin );
    m_ConnectedComponentImage->SetSpacing( m_OutputSpacing );
    m_ConnectedComponentImage->SetRegions( m_BufferedRegion );
    m_ConnectedComponentImage->SetDirection( m_OutputDirection );
    m_ConnectedComponentImage->Allocate();
    m_ConnectedComponentImage->FillBuffer( 0 );
    }

  // Allocate memory for the PointTypeImage
  m_LabelImage->CopyInformation(oImage);
  m_LabelImage->SetBufferedRegion( m_BufferedRegion );
  m_LabelImage->Allocate();
  m_LabelImage->FillBuffer( Traits::Far );

  NodeType idx;
  OutputPixelType outputPixel = this->m_LargeValue;

  if ( this->m_AlivePoints )
    {
    NodePairContainerConstIterator pointsIter = this->m_AlivePoints->Begin();
    NodePairContainerConstIterator pointsEnd = this->m_AlivePoints->End();

    while( pointsIter != pointsEnd )
      {
      // Get node from alive points container
      idx = pointsIter->Value().GetNode();

      // Check if node index is within the output level set
      if ( m_BufferedRegion.IsInside( idx ) )
        {
        // Make this an alive point
        this->SetLabelValueForGivenNode( idx, Traits::Alive );

        if( this->m_TopologyCheck == Superclass::NoHandles )
          {
          m_ConnectedComponentImage->SetPixel( idx, 1 );
          }

        outputPixel = pointsIter->Value().GetValue();
        this->SetOutputValue( oImage, idx, outputPixel );
        }

      ++pointsIter;
      }
    }

  if( this->m_ForbiddenPoints )
    {
    NodePairContainerConstIterator pointsIter = this->m_ForbiddenPoints->Begin();
    NodePairContainerConstIterator pointsEnd = this->m_ForbiddenPoints->End();

    OutputPixelType zero = NumericTraits< OutputPixelType >::ZeroValue();

    while( pointsIter != pointsEnd )
      {
      idx = pointsIter->Value().GetNode();

      // Check if node index is within the output level set
      if ( m_BufferedRegion.IsInside( idx ) )
        {
        // Make this an alive point
        this->SetLabelValueForGivenNode( idx, Traits::Forbidden );
        this->SetOutputValue( oImage, idx, zero );
        }

      ++pointsIter;
      }
    }

  if( this->m_TopologyCheck == Superclass::NoHandles )
    {
    // Now create the connected component image and relabel such that labels
    // are 1, 2, 3, ...
    typedef ConnectedComponentImageFilter<ConnectedComponentImageType,
      ConnectedComponentImageType> ConnectedComponentFilterType;
    typename ConnectedComponentFilterType::Pointer connecter
        = ConnectedComponentFilterType::New();
    connecter->SetInput( m_ConnectedComponentImage );

    typedef RelabelComponentImageFilter<ConnectedComponentImageType,
        ConnectedComponentImageType> RelabelerType;
    typename RelabelerType::Pointer relabeler = RelabelerType::New();
    relabeler->SetInput( connecter->GetOutput() );

    try
      {
      relabeler->Update();
      }
    catch( ExceptionObject & excep )
      {
      std::cout << excep <<std::endl;
      }

    this->m_ConnectedComponentImage = relabeler->GetOutput();
    }

  // Process the input trial points
  if ( this->m_TrialPoints )
    {
    NodePairContainerConstIterator pointsIter = this->m_TrialPoints->Begin();
    NodePairContainerConstIterator pointsEnd = this->m_TrialPoints->End();

    while( pointsIter != pointsEnd )
      {
      // Get node from trial points container
      idx = pointsIter->Value().GetNode();

      // Check if node index is within the output level set
      if ( m_BufferedRegion.IsInside( idx ) )
        {
        // Make this an initial trial point
        this->SetLabelValueForGivenNode( idx, Traits::InitialTrial );

        outputPixel = pointsIter->Value().GetValue();
        this->SetOutputValue( oImage, idx, outputPixel );

        //this->m_Heap->Push( PriorityQueueElementType( idx, pointsIter->second ) );
        this->m_Heap.push( pointsIter->Value() );
        }
      ++pointsIter;
      }
    }
  // Initialize indices if this->m_TopologyCheck is activated
  if( this->m_TopologyCheck != Superclass::Nothing )
    {
    if( ImageDimension == 2 )
      {
      InitializeIndices2D();
      }
    else
      {
      if( ImageDimension == 3 )
        {
        InitializeIndices3D();
        }
      else
        {
        itkWarningMacro(
              << "Topology checking is only valid for level set dimensions of 2 and 3" );
        }
      }
    }

  // Cache the pointer to the input image
  m_InputCache = this->GetInput();
}

template< typename TInput, typename TOutput >
bool
FastMarchingImageFilterBase< TInput, TOutput >::
DoesVoxelChangeViolateWellComposedness( const NodeType& idx ) const
{
  bool isChangeWellComposed = false;
  if( ImageDimension == 2 )
    {
    isChangeWellComposed = this->IsChangeWellComposed2D( idx );
    }
  else  // ImageDimension == 3
    {
    isChangeWellComposed = this->IsChangeWellComposed3D( idx );
    }

  return !isChangeWellComposed;
}

template< typename TInput, typename TOutput >
bool
FastMarchingImageFilterBase< TInput, TOutput >::
DoesVoxelChangeViolateStrictTopology( const NodeType& idx ) const
{
  typename NeighborhoodIteratorType::RadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType It( radius, this->m_LabelImage,
    this->m_LabelImage->GetBufferedRegion() );
  It.SetLocation( idx );

  unsigned int numberOfCriticalC3Configurations = 0;
  unsigned int numberOfFaces = 0;

  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    if( It.GetNext( d ) == Traits::Alive )
      {
      ++numberOfFaces;
      }
    if( It.GetPrevious( d ) == Traits::Alive )
      {
      ++numberOfFaces;
      }
    if( It.GetNext( d ) == Traits::Alive &&
        It.GetPrevious( d ) == Traits::Alive )
      {
      ++numberOfCriticalC3Configurations;
      }
    }

  if( ( numberOfCriticalC3Configurations > 0 ) &&
      ( numberOfFaces % 2 == 0 ) &&
      ( numberOfCriticalC3Configurations * 2 == numberOfFaces ) )
    {
    return true;
    }
  return false;
}

template< typename TInput, typename TOutput >
bool
FastMarchingImageFilterBase< TInput, TOutput >::
IsChangeWellComposed2D( const NodeType& idx ) const
{
  NeighborhoodRadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType It( radius, this->m_LabelImage,
    this->m_LabelImage->GetBufferedRegion() );
  It.SetLocation( idx );

  std::bitset<9> neighborhoodPixels;

  // Check for critical configurations: 4 90-degree rotations

  for ( unsigned int i = 0; i < 4; i++ )
    {
    for ( unsigned int j = 0; j < 9; j++ )
      {
      neighborhoodPixels[j] =
        ( It.GetPixel( this->m_RotationIndices[i][j] ) != Traits::Alive );
      if( this->m_RotationIndices[i][j] == 4 )
        {
        neighborhoodPixels.flip(j);
        }
      }

    if( this->IsCriticalC1Configuration2D( neighborhoodPixels )
      || this->IsCriticalC2Configuration2D( neighborhoodPixels )
      || this->IsCriticalC3Configuration2D( neighborhoodPixels )
      || this->IsCriticalC4Configuration2D( neighborhoodPixels ) )
      {
      return false;
      }
    }

  // Check for critical configurations: 2 reflections.
  // Note that the reflections for the C1 and C2 cases are covered by the
  // rotation cases above (except in the case of FullInvariance == false).

  for ( unsigned int i = 0; i < 2; i++ )
    {
    for ( unsigned int j = 0; j < 9; j++ )
      {
      neighborhoodPixels[j] =
        ( It.GetPixel( this->m_ReflectionIndices[i][j] ) != Traits::Alive );
      if( this->m_ReflectionIndices[i][j] == 4 )
        {
        neighborhoodPixels.flip(j);
        }
      }
    if( this->IsCriticalC3Configuration2D( neighborhoodPixels )
      || this->IsCriticalC4Configuration2D( neighborhoodPixels ) )
      {
      return false;
      }
    }
  return true;
}

template< typename TInput, typename TOutput >
bool
FastMarchingImageFilterBase< TInput, TOutput >::
IsCriticalC1Configuration2D( const std::bitset<9>& neighborhood ) const
{
  return ( !neighborhood[0] &&  neighborhood[1] &&
            neighborhood[3] && !neighborhood[4] &&
           !neighborhood[8] );
}

template< typename TInput, typename TOutput >
bool
FastMarchingImageFilterBase< TInput, TOutput >::
IsCriticalC2Configuration2D( const std::bitset<9>& neighborhood ) const
{
  return ( !neighborhood[0] &&  neighborhood[1] &&
            neighborhood[3] && !neighborhood[4] &&
            neighborhood[8] &&
           ( neighborhood[5] || neighborhood[7] ) );
}

template< typename TInput, typename TOutput >
bool
FastMarchingImageFilterBase< TInput, TOutput >::
IsCriticalC3Configuration2D( const std::bitset<9>& neighborhood ) const
{
  return ( !neighborhood[0] &&  neighborhood[1] &&
            neighborhood[3] && !neighborhood[4] &&
           !neighborhood[5] &&  neighborhood[6] &&
           !neighborhood[7] &&  neighborhood[8] );
}

template< typename TInput, typename TOutput >
bool
FastMarchingImageFilterBase< TInput, TOutput >::
IsCriticalC4Configuration2D( const std::bitset<9>& neighborhood ) const
{
  return ( !neighborhood[0] &&  neighborhood[1] &&
            neighborhood[3] && !neighborhood[4] &&
           !neighborhood[5] && !neighborhood[6] &&
           !neighborhood[7] &&  neighborhood[8] );
}

template< typename TInput, typename TOutput >
void
FastMarchingImageFilterBase< TInput, TOutput >::
InitializeIndices2D()
{
  this->m_RotationIndices[0].SetSize( 9 );
  this->m_RotationIndices[1].SetSize( 9 );
  this->m_RotationIndices[2].SetSize( 9 );
  this->m_RotationIndices[3].SetSize( 9 );

  this->m_RotationIndices[0][0] = 0;
  this->m_RotationIndices[0][1] = 1;
  this->m_RotationIndices[0][2] = 2;
  this->m_RotationIndices[0][3] = 3;
  this->m_RotationIndices[0][4] = 4;
  this->m_RotationIndices[0][5] = 5;
  this->m_RotationIndices[0][6] = 6;
  this->m_RotationIndices[0][7] = 7;
  this->m_RotationIndices[0][8] = 8;

  this->m_RotationIndices[1][0] = 2;
  this->m_RotationIndices[1][1] = 5;
  this->m_RotationIndices[1][2] = 8;
  this->m_RotationIndices[1][3] = 1;
  this->m_RotationIndices[1][4] = 4;
  this->m_RotationIndices[1][5] = 7;
  this->m_RotationIndices[1][6] = 0;
  this->m_RotationIndices[1][7] = 3;
  this->m_RotationIndices[1][8] = 6;

  this->m_RotationIndices[2][0] = 8;
  this->m_RotationIndices[2][1] = 7;
  this->m_RotationIndices[2][2] = 6;
  this->m_RotationIndices[2][3] = 5;
  this->m_RotationIndices[2][4] = 4;
  this->m_RotationIndices[2][5] = 3;
  this->m_RotationIndices[2][6] = 2;
  this->m_RotationIndices[2][7] = 1;
  this->m_RotationIndices[2][8] = 0;

  this->m_RotationIndices[3][0] = 6;
  this->m_RotationIndices[3][1] = 3;
  this->m_RotationIndices[3][2] = 0;
  this->m_RotationIndices[3][3] = 7;
  this->m_RotationIndices[3][4] = 4;
  this->m_RotationIndices[3][5] = 1;
  this->m_RotationIndices[3][6] = 8;
  this->m_RotationIndices[3][7] = 5;
  this->m_RotationIndices[3][8] = 2;

  this->m_ReflectionIndices[0].SetSize( 9 );
  this->m_ReflectionIndices[1].SetSize( 9 );

  this->m_ReflectionIndices[0][0] = 6;
  this->m_ReflectionIndices[0][1] = 7;
  this->m_ReflectionIndices[0][2] = 8;
  this->m_ReflectionIndices[0][3] = 3;
  this->m_ReflectionIndices[0][4] = 4;
  this->m_ReflectionIndices[0][5] = 5;
  this->m_ReflectionIndices[0][6] = 0;
  this->m_ReflectionIndices[0][7] = 1;
  this->m_ReflectionIndices[0][8] = 2;

  this->m_ReflectionIndices[1][0] = 2;
  this->m_ReflectionIndices[1][1] = 1;
  this->m_ReflectionIndices[1][2] = 0;
  this->m_ReflectionIndices[1][3] = 5;
  this->m_ReflectionIndices[1][4] = 4;
  this->m_ReflectionIndices[1][5] = 3;
  this->m_ReflectionIndices[1][6] = 8;
  this->m_ReflectionIndices[1][7] = 7;
  this->m_ReflectionIndices[1][8] = 6;
}

template< typename TInput, typename TOutput >
bool
FastMarchingImageFilterBase< TInput, TOutput >::
IsChangeWellComposed3D( const NodeType& idx ) const
{
  std::bitset<8> neighborhoodPixels;

  NeighborhoodRadiusType radius;
  radius.Fill( 1 );

  NeighborhoodIteratorType It( radius,
                              this->m_LabelImage,
                              this->m_LabelImage->GetRequestedRegion() );

  It.SetLocation( idx );

  // Check for C1 critical configurations
  for ( unsigned int i = 0; i < 12; i++ )
    {
    for ( unsigned int j = 0; j < 4; j++ )
      {
      neighborhoodPixels[j]
        = ( It.GetPixel( m_C1Indices[i][j] ) == Traits::Alive );
      if( m_C1Indices[i][j] == 13 )
        {
        neighborhoodPixels[j] = !neighborhoodPixels[j];
        }
      }
    if( this->IsCriticalC1Configuration3D( neighborhoodPixels ) )
      {
      return false;
      }
    }

  // Check for C2 critical configurations
  for ( unsigned int i = 0; i < 8; i++ )
    {
    for ( unsigned int j = 0; j < 8; j++ )
      {
      neighborhoodPixels[j]
        = ( It.GetPixel( m_C2Indices[i][j] ) == Traits::Alive );
      if( m_C2Indices[i][j] == 13 )
        {
        neighborhoodPixels[j] = !neighborhoodPixels[j];
        }
      }
    if( IsCriticalC2Configuration3D( neighborhoodPixels ) != 0 )
      {
      return false;
      }
    }

  return true;
}

template< typename TInput, typename TOutput >
bool
FastMarchingImageFilterBase< TInput, TOutput >::
IsCriticalC1Configuration3D( const std::bitset<8>& neighborhood ) const
{
  return ( (  neighborhood[0] &&  neighborhood[1] &&
             !neighborhood[2] && !neighborhood[3] ) ||
           ( !neighborhood[0] && !neighborhood[1] &&
              neighborhood[2] &&  neighborhood[3] ) );
}

template< typename TInput, typename TOutput >
unsigned int
FastMarchingImageFilterBase< TInput, TOutput >::
IsCriticalC2Configuration3D( const std::bitset<8>& neighborhood ) const
{
  // Check if Type 1 or Type 2
  for ( unsigned int i = 0; i < 4; i++ )
    {
    bool isC2 = false;
    if( neighborhood[2*i] == neighborhood[2*i+1] )
      {
      isC2 = true;
      for ( unsigned int j = 0; j < 8; j++ )
        {
        if( neighborhood[j] == neighborhood[2*i] &&
               j != 2*i && j != 2*i+1 )
          {
          isC2 = false;
          }
        }
      }
    if( isC2 )
      {
      if( neighborhood[2*i] )
        {
        return 1;
        }
      else
        {
        return 2;
        }
      }
    }

  return 0;
}

template< typename TInput, typename TOutput >
void
FastMarchingImageFilterBase< TInput, TOutput >::
InitializeIndices3D()
{
  for ( unsigned int i = 0; i <  12; i++ )
    {
    this->m_C1Indices[i].SetSize( 4 );
    }
  for ( unsigned int i = 0; i <  8; i++ )
    {
    this->m_C2Indices[i].SetSize( 8 );
    }

  this->m_C1Indices[0][0] = 1;
  this->m_C1Indices[0][1] = 13;
  this->m_C1Indices[0][2] = 4;
  this->m_C1Indices[0][3] = 10;

  this->m_C1Indices[1][0] = 9;
  this->m_C1Indices[1][1] = 13;
  this->m_C1Indices[1][2] = 10;
  this->m_C1Indices[1][3] = 12;

  this->m_C1Indices[2][0] = 3;
  this->m_C1Indices[2][1] = 13;
  this->m_C1Indices[2][2] = 4;
  this->m_C1Indices[2][3] = 12;

  this->m_C1Indices[3][0] = 4;
  this->m_C1Indices[3][1] = 14;
  this->m_C1Indices[3][2] = 5;
  this->m_C1Indices[3][3] = 13;

  this->m_C1Indices[4][0] = 12;
  this->m_C1Indices[4][1] = 22;
  this->m_C1Indices[4][2] = 13;
  this->m_C1Indices[4][3] = 21;

  this->m_C1Indices[5][0] = 13;
  this->m_C1Indices[5][1] = 23;
  this->m_C1Indices[5][2] = 14;
  this->m_C1Indices[5][3] = 22;

  this->m_C1Indices[6][0] = 4;
  this->m_C1Indices[6][1] = 16;
  this->m_C1Indices[6][2] = 7;
  this->m_C1Indices[6][3] = 13;

  this->m_C1Indices[7][0] = 13;
  this->m_C1Indices[7][1] = 25;
  this->m_C1Indices[7][2] = 16;
  this->m_C1Indices[7][3] = 22;

  this->m_C1Indices[8][0] = 10;
  this->m_C1Indices[8][1] = 22;
  this->m_C1Indices[8][2] = 13;
  this->m_C1Indices[8][3] = 19;

  this->m_C1Indices[9][0] = 12;
  this->m_C1Indices[9][1] = 16;
  this->m_C1Indices[9][2] = 13;
  this->m_C1Indices[9][3] = 15;

  this->m_C1Indices[10][0] = 13;
  this->m_C1Indices[10][1] = 17;
  this->m_C1Indices[10][2] = 14;
  this->m_C1Indices[10][3] = 16;

  this->m_C1Indices[11][0] = 10;
  this->m_C1Indices[11][1] = 14;
  this->m_C1Indices[11][2] = 11;
  this->m_C1Indices[11][3] = 13;

  this->m_C2Indices[0][0] = 0;
  this->m_C2Indices[0][1] = 13;
  this->m_C2Indices[0][2] = 1;
  this->m_C2Indices[0][3] = 12;
  this->m_C2Indices[0][4] = 3;
  this->m_C2Indices[0][5] = 10;
  this->m_C2Indices[0][6] = 4;
  this->m_C2Indices[0][7] = 9;

  this->m_C2Indices[4][0] = 9;
  this->m_C2Indices[4][1] = 22;
  this->m_C2Indices[4][2] = 10;
  this->m_C2Indices[4][3] = 21;
  this->m_C2Indices[4][4] = 12;
  this->m_C2Indices[4][5] = 19;
  this->m_C2Indices[4][6] = 13;
  this->m_C2Indices[4][7] = 18;

  for ( unsigned int i = 1; i < 4; i++ )
    {
    int addend;
    if( i == 2 )
      {
      addend = 2;
      }
    else
      {
      addend = 1;
      }
    for ( unsigned int j = 0; j < 8; j++ )
      {
      this->m_C2Indices[i  ][j] = this->m_C2Indices[i-1][j] + addend;
      this->m_C2Indices[i+4][j] = this->m_C2Indices[i+3][j] + addend;
      }
    }
}

template< typename TInput, typename TOutput >
void FastMarchingImageFilterBase< TInput, TOutput >::
PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "BufferedRegion;: " << m_BufferedRegion << std::endl;
  os << indent << "StartIndex: " << m_StartIndex << std::endl;
  os << indent << "LastIndex: " << m_LastIndex << std::endl;

  os << indent << "OutputRegion: " << m_OutputRegion << std::endl;
  os << indent << "OutputOrigin: " << m_OutputOrigin << std::endl;
  os << indent << "OutputSpacing: " << m_OutputSpacing << std::endl;
  os << indent << "OutputDirection: " << m_OutputDirection << std::endl;

  os << indent << "OverrideOutputInformation: " << m_OverrideOutputInformation
    << std::endl;

  itkPrintSelfObjectMacro( LabelImage );

  itkPrintSelfObjectMacro( ConnectedComponentImage );

  os << indent << "RotationIndices: " << m_RotationIndices << std::endl;
  os << indent << "ReflectionIndices: " << m_ReflectionIndices << std::endl;

  os << indent << "C1Indices: " << m_C1Indices << std::endl;
  os << indent << "C2Indices: " << m_C2Indices << std::endl;

  itkPrintSelfObjectMacro( InputCache );
}
} // end namespace itk
#endif // itkFastMarchingImageFilterBase_hxx
