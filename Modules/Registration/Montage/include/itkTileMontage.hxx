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

#ifndef itkTileMontage_hxx
#define itkTileMontage_hxx

#include "itkTileMontage.h"

#include "itkMultiThreaderBase.h"
#include "itkNumericTraits.h"

#include <algorithm>
#include <cassert>

namespace itk
{
template< typename TImageType, typename TCoordinate >
TileMontage< TImageType, TCoordinate >
::TileMontage()
{
  m_PCM = PCMType::New();
  m_PCMOperator = PCMOperatorType::New();
  m_PCMOptimizer = PCMOptimizerType::New();
  m_Reader = ReaderType::New();
  m_Dummy = ImageType::New();
  m_OriginAdjustment.Fill( 0 );
  m_ForcedSpacing.Fill( 0 );

  // make default padding sufficient for exponential decay to zero
  m_ObligatoryPadding.Fill( 0 );
  SizeType pad;
  pad.Fill( 8 * sizeof( typename TImageType::PixelType ) );
  this->SetObligatoryPadding( pad );

  m_FinishedTiles = 0;
  SizeType initialSize;
  initialSize.Fill( 1 );
  initialSize[0] = 2;
  this->SetMontageSize( initialSize );

  // required for GenerateOutputInformation to be called
  this->SetNthOutput( 0, this->MakeOutput( 0 ).GetPointer() );
}

template< typename TImageType, typename TCoordinate >
void
TileMontage< TImageType, TCoordinate >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  if ( this->GetDebug() )
    {
    Superclass::PrintSelf( os, indent ); // this can be overwhelming
    }
  os << indent << "Montage size: " << m_MontageSize << std::endl;
  os << indent << "Linear Montage size: " << m_LinearMontageSize << std::endl;
  os << indent << "Finished Tiles: " << m_FinishedTiles << std::endl;
  os << indent << "Origin Adjustment: " << m_OriginAdjustment << std::endl;
  os << indent << "Forced Spacing: " << m_ForcedSpacing << std::endl;
  os << indent << "Obligatory Padding: " << m_ObligatoryPadding << std::endl;

  auto nullCount = std::count( m_Filenames.begin(), m_Filenames.end(), std::string() );
  os << indent << "Filenames (filled/capcity): " << m_Filenames.size() - nullCount
    << "/" << m_Filenames.size() << std::endl;
  nullCount = std::count( m_FFTCache.begin(), m_FFTCache.end(), nullptr );
  os << indent << "FFTCache (filled/capcity): " << m_FFTCache.size() - nullCount
    << "/" << m_FFTCache.size() << std::endl;

  os << indent << "PhaseCorrelationImageRegistrationMethod: " << m_PCM.GetPointer() << std::endl;
  os << indent << "PCM Optimizer: " << m_PCMOptimizer.GetPointer() << std::endl;
  os << indent << "PCM Operator: " << m_PCMOperator.GetPointer() << std::endl;
  os << indent << "Image Reader: " << m_Reader.GetPointer() << std::endl;

  os << indent << "MinInner: " << m_MinInner << std::endl;
  os << indent << "MaxInner: " << m_MaxInner << std::endl;
  os << indent << "MinOuter: " << m_MinOuter << std::endl;
  os << indent << "MaxOuter: " << m_MaxOuter << std::endl;
}

template< typename TImageType, typename TCoordinate >
void
TileMontage< TImageType, TCoordinate >
::SetMontageSize( SizeType montageSize )
{
  if ( m_MontageSize != montageSize )
    {
    m_LinearMontageSize = 1u;
    for ( unsigned d = 0; d < ImageDimension; d++ )
      {
      m_LinearMontageSize *= montageSize[d];
      }
    this->SetNumberOfRequiredInputs( m_LinearMontageSize );
    this->SetNumberOfRequiredOutputs( m_LinearMontageSize );
    m_MontageSize = montageSize;
    m_Filenames.resize( m_LinearMontageSize );
    m_FFTCache.resize( m_LinearMontageSize );
    this->Modified();
    }
}

template< typename TImageType, typename TCoordinate >
template< typename TImageToRead >
typename TImageToRead::Pointer
TileMontage< TImageType, TCoordinate >
::GetImageHelper( TileIndexType nDIndex, bool metadataOnly, RegionType region, ImageFileReader< TImageToRead >* reader)
{
  DataObjectPointerArraySizeType linearIndex = nDIndexToLinearIndex( nDIndex );
  const auto cInput = static_cast< TImageToRead* >( this->GetInput( linearIndex ) );
  typename TImageToRead::Pointer input = const_cast< TImageToRead* >( cInput );
  typename TImageToRead::Pointer result = nullptr;
  if ( input.GetPointer() != reinterpret_cast< TImageToRead* >( this->m_Dummy.GetPointer() ) )
    {
    // construct new metadata so adjustments do not modify the original input
    RegionType region = input->GetBufferedRegion();
    result = TImageToRead::New();
    result->SetRegions( region );
    result->SetOrigin( input->GetOrigin() );
    result->SetSpacing( input->GetSpacing() );
    result->SetDirection( input->GetDirection() );
    result->SetPixelContainer( input->GetPixelContainer() );
    }
  else // examine cache and read from file if necessary
    {
    using ImageReaderType = ImageFileReader< TImageToRead >;
    typename ImageReaderType::Pointer iReader = reader;
    if ( iReader == nullptr )
      {
      iReader = ImageReaderType::New();
      }
    iReader->SetFileName( this->m_Filenames[linearIndex] );
    iReader->UpdateOutputInformation();
    result = iReader->GetOutput();

    if ( !metadataOnly )
      {
      RegionType regionToRead = result->GetLargestPossibleRegion();
      if ( region.GetNumberOfPixels() > 0 )
        {
        regionToRead.Crop( region );
        result->SetRequestedRegion( regionToRead );
        }
      iReader->Update();
      }
    result->DisconnectPipeline();
    }

  //adjust origin and spacing
  PointType origin = result->GetOrigin();
  for ( unsigned d = 0; d < ImageDimension; d++ )
    {
    origin[d] += this->m_OriginAdjustment[d] * nDIndex[d];
    }
  result->SetOrigin( origin );
  if ( this->m_ForcedSpacing[0] != 0 )
    {
    result->SetSpacing( this->m_ForcedSpacing );
    }

  return result;
}

template< typename TImageType, typename TCoordinate >
typename TileMontage< TImageType, TCoordinate >::ImageType::Pointer
TileMontage< TImageType, TCoordinate >
::GetImage( TileIndexType nDIndex, bool metadataOnly )
{
  RegionType reg0; // default-initialized to zeroes
  return GetImageHelper< ImageType >( nDIndex, metadataOnly, reg0, m_Reader );
}

template< typename TImageType, typename TCoordinate >
DataObject::DataObjectPointerArraySizeType
TileMontage< TImageType, TCoordinate >
::nDIndexToLinearIndex( TileIndexType nDIndex ) const
{
  DataObjectPointerArraySizeType ind = 0;
  SizeValueType                  stride = 1u;
  for ( unsigned d = 0; d < ImageDimension; d++ )
    {
    itkAssertOrThrowMacro( nDIndex[d] < m_MontageSize[d],
      "Tile index " << nDIndex << " exceeds tile size " << m_MontageSize << " at dimension " << d );
    ind += nDIndex[d] * stride;
    stride *= m_MontageSize[d];
    }
  return ind;
}

template< typename TImageType, typename TCoordinate >
typename TileMontage< TImageType, TCoordinate >::TileIndexType
TileMontage< TImageType, TCoordinate >
::LinearIndexTonDIndex( DataObject::DataObjectPointerArraySizeType linearIndex ) const
{
  TileIndexType ind;
  SizeValueType stride = 1u;
  for ( unsigned d = 0; d < ImageDimension; d++ )
    {
    stride *= m_MontageSize[d];
    ind[d] = linearIndex % stride;
    linearIndex /= stride;
    }
  itkAssertOrThrowMacro( linearIndex < stride,
    "Linear tile index " << linearIndex << " exceeds total montage size " << stride );
  return ind;
}

template< typename TImageType, typename TCoordinate >
typename TileMontage< TImageType, TCoordinate >::TransformPointer
TileMontage< TImageType, TCoordinate >
::RegisterPair( TileIndexType fixed, TileIndexType moving )
{
  DataObjectPointerArraySizeType lFixedInd = nDIndexToLinearIndex( fixed );
  DataObjectPointerArraySizeType lMovingInd = nDIndexToLinearIndex( moving );

  auto mImage = this->GetImage( moving, false );
  m_PCM->SetFixedImage( this->GetImage( fixed, false ) );
  m_PCM->SetMovingImage( mImage );
  m_PCM->SetFixedImageFFT( m_FFTCache[lFixedInd] ); // maybe null
  m_PCM->SetMovingImageFFT( m_FFTCache[lMovingInd] ); // maybe null
  // m_PCM->DebugOn();
  m_PCM->Update();

  m_FFTCache[lFixedInd] = m_PCM->GetFixedImageFFT(); // certainly not null
  m_FFTCache[lMovingInd] = m_PCM->GetMovingImageFFT(); // certrainly not null

  const TransformType* regTr = m_PCM->GetOutput()->Get();
  // this translation is in index space, convert it into physical space
  typename TransformType::OutputVectorType translation = regTr->GetOffset();
  PointType p0, p;
  ContinuousIndexType ci;
  ci.Fill( 0.0 );
  mImage->TransformContinuousIndexToPhysicalPoint( ci, p0 );
  for ( unsigned d = 0; d < ImageDimension; d++ )
    {
    ci[d] = translation[d];
    }
  mImage->TransformContinuousIndexToPhysicalPoint( ci, p );
  translation = p - p0;

  TransformPointer t = TransformType::New();
  t->SetOffset( translation );
  return t;
}

template< typename TImageType, typename TCoordinate >
void
TileMontage< TImageType, TCoordinate >
::ReleaseMemory( TileIndexType finishedTile )
{
  TileIndexType oldIndex;
  bool releaseTile = true;
  for ( unsigned dim = 0; dim < ImageDimension; dim++ )
    {
    if ( finishedTile[dim] > 0 )
      {
      oldIndex[dim] = finishedTile[dim] - 1;
      }
    else
      {
      releaseTile = false;
      }
    }
  if ( releaseTile )
    {
    SizeValueType linearIndex = this->nDIndexToLinearIndex( oldIndex );
    m_FFTCache[linearIndex] = nullptr;
    if ( !m_Filenames[linearIndex].empty() ) // release the input image too
      {
      this->SetInputTile( oldIndex, m_Dummy );
      }
    }
}

template< typename TImageType, typename TCoordinate >
void
TileMontage< TImageType, TCoordinate >
::MontageDimension( int d, TileIndexType initialTile )
{
  TileIndexType currentIndex = initialTile;
  if ( d < 0 )
    {
    return; // nothing to do, terminate recursion
    }
  else // d>=0
    {
    currentIndex[d] = 0; // montage first index in lower dimension
    MontageDimension( d - 1, currentIndex );

    for ( unsigned i = 1; i < m_MontageSize[d]; i++ )
      {
      // register i-th tile to adjacent tiles along all dimension (lower index only)
      currentIndex[d] = i;
      std::vector< TransformPointer > transforms;
      for ( unsigned regDim = 0; regDim < ImageDimension; regDim++ )
        {
        if ( currentIndex[regDim] > 0 ) // we are not at the edge along this dimension
          {
          TileIndexType referenceIndex = currentIndex;
          referenceIndex[regDim] = currentIndex[regDim] - 1;
          TransformPointer      t = this->RegisterPair( referenceIndex, currentIndex );
          TransformConstPointer oldT = this->GetTransform( referenceIndex );
          t->Compose( oldT, true );
          transforms.push_back( t );
          }
        }

      // determine how to best combine transforms - make average for now
      TransformPointer t = TransformType::New(); // identity i.e. 0-translation by default
      for ( unsigned ti = 0; ti < transforms.size(); ti++ )
        {
        t->SetOffset( t->GetOffset() + transforms[ti]->GetOffset() / transforms.size() );
        }

      this->WriteOutTransform( currentIndex, t );

      // montage this index in lower dimension
      MontageDimension( d - 1, currentIndex );

      this->ReleaseMemory( currentIndex ); // kick old tile out of cache
      }

    // kick "rightmost" tile in previous row out of cache
    currentIndex[d] = m_MontageSize[d];
    this->ReleaseMemory( currentIndex );
    }
}

template< typename TImageType, typename TCoordinate >
void
TileMontage< TImageType, TCoordinate >
::WriteOutTransform( TileIndexType index, TransformPointer transform )
{
  const SizeValueType linearIndex = this->nDIndexToLinearIndex( index );
  auto dOut = this->GetOutput( linearIndex );
  const auto cOut = static_cast< TransformOutputType* >( dOut );
  auto  decorator = const_cast< TransformOutputType* >( cOut );
  decorator->Set( transform );
  auto input0 = static_cast< const ImageType* >( this->GetInput( 0 ) );
  auto input = static_cast< const ImageType* >( this->GetInput( linearIndex ) );
  this->UpdateMosaicBounds( index, transform, input, input0 );
  m_FinishedTiles++;
  this->UpdateProgress( float( m_FinishedTiles ) / m_LinearMontageSize );
}

template< typename TImageType, typename TCoordinate >
void
TileMontage< TImageType, TCoordinate >
::UpdateMosaicBounds(
    TileIndexType index,
    TransformConstPointer transform,
    const ImageType* input,
    const ImageType* input0 )
{
  PointType p;
  ContinuousIndexType ci;
  ImageIndexType ind = input->GetLargestPossibleRegion().GetIndex();
  input->TransformIndexToPhysicalPoint( ind, p );
  TransformPointer inverseT = TransformType::New();
  transform->GetInverse( inverseT );
  p = inverseT->TransformPoint( p );
  input0->TransformPhysicalPointToContinuousIndex( p, ci );
  for ( unsigned d = 0; d < ImageDimension; d++ )
    {
    if ( index[d] == 0 ) // this tile is on the minimum edge
      {
      m_MinInner[d] = std::max( m_MinInner[d], ci[d] );
      m_MinOuter[d] = std::min( m_MinOuter[d], ci[d] );
      }
    }
  ind += input->GetLargestPossibleRegion().GetSize();
  input->TransformIndexToPhysicalPoint( ind, p );
  p = inverseT->TransformPoint( p );
  input0->TransformPhysicalPointToContinuousIndex( p, ci );
  for ( unsigned d = 0; d < ImageDimension; d++ )
    {
    if ( index[d] == m_MontageSize[d] - 1 ) // this tile is on the maximum edge
      {
      m_MaxOuter[d] = std::max( m_MaxOuter[d], ci[d] );
      m_MaxInner[d] = std::min( m_MaxInner[d], ci[d] );
      }
    }
}

template< typename TImageType, typename TCoordinate >
void
TileMontage< TImageType, TCoordinate >
::GenerateOutputInformation()
{
  Superclass::GenerateOutputInformation();

  std::vector< double > sizes( ImageDimension ); // default initialized to 0
  SizeType maxSizes;
  maxSizes.Fill( 0 );
  for ( SizeValueType i = 0; i < m_LinearMontageSize; i++ )
    {
    if ( i > 0 ) // otherwise primary output has same modification time as this class
      {          // and GenerateData does not get called
      this->SetNthOutput( i, this->MakeOutput( i ).GetPointer() );
      }
    // the rest of this code determines average and maximum tile sizes
    TileIndexType nDIndex = this->LinearIndexTonDIndex( i );
    typename ImageType::Pointer input = this->GetImage( nDIndex, true );
    RegionType reg = input->GetLargestPossibleRegion();
    for ( unsigned d = 0; d < ImageDimension; d++ )
      {
      sizes[d] += reg.GetSize( d );
      maxSizes[d] = std::max( maxSizes[d], reg.GetSize( d ) );
      }
    }

  // divide by count to get average
  for ( unsigned d = 0; d < ImageDimension; d++ )
    {
    sizes[d] /= m_LinearMontageSize;
    }

  // if maximum size is more than twice the average along any dimension,
  // we will not pad all the images to maxSize
  // in most cases images will be of similar or exactly the same size
  bool forceSame = true;
  for ( unsigned d = 0; d < ImageDimension; d++ )
    {
    if ( sizes[d] * 2 < maxSizes[d] )
      {
      forceSame = false;
      }
    maxSizes[d] += 2 * m_ObligatoryPadding[d];
    }
  if ( forceSame )
    {
    maxSizes = m_PCM->RoundUpToFFTSize( maxSizes );
    m_PCM->SetPadToSize( maxSizes );
    }

  // we connect these classes here in case user has provided new versions
  m_PCM->SetOperator( m_PCMOperator );
  m_PCM->SetOptimizer( m_PCMOptimizer );
}

template< typename TImageType, typename TCoordinate >
void
TileMontage< TImageType, TCoordinate >
::GenerateData()
{
  // initialize mosaic bounds
  auto input0 = static_cast< const ImageType* >( this->GetInput( 0 ) );
  ImageIndexType ind = input0->GetLargestPossibleRegion().GetIndex();
  m_MinInner = ind;
  m_MinOuter = ind;
  ind += input0->GetLargestPossibleRegion().GetSize();
  m_MaxOuter = ind;
  m_MaxInner.Fill( NumericTraits< TCoordinate >::max() );

  typename TransformType::Pointer t0 = TransformType::New();
  TileIndexType ind0;
  ind0.Fill( 0 );
  m_FinishedTiles = 0;

  this->WriteOutTransform( ind0, t0 ); // write identity (no translation) for tile 0
  this->MontageDimension( this->ImageDimension - 1, ind0 );

  // clear rest of the cache after montaging is finished
  for ( SizeValueType i = 0; i < m_LinearMontageSize; i++ )
    {
    m_FFTCache[i] = nullptr;
    if ( !m_Filenames[i].empty() ) // release the input image too
      {
      this->SetInputTile( this->LinearIndexTonDIndex( i ), m_Dummy );
      }
    }
}

} // namespace itk

#endif // itkTileMontage_hxx
