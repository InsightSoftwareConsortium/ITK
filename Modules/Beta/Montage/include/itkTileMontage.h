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

#ifndef itkTileMontage_h
#define itkTileMontage_h

#include "itkImageFileReader.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkPhaseCorrelationImageRegistrationMethod.h"

#include <vector>

namespace itk
{
/** \class TileMontage
 * \brief Determines registrations for an n-Dimensional mosaic of images.
 *
 * Determines registrations which can be used to resample a mosaic into a single image.
 *
 * \author Dženan Zukić, dzenan.zukic@kitware.com
 *
 * \ingroup Montage
 */
template< typename TImageType, typename TCoordinate = float >
class ITK_TEMPLATE_EXPORT TileMontage : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN( TileMontage );

  /** Standard class type aliases. */
  using Self = TileMontage;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;
  using ImageType = TImageType;
  // using ImagePointer = typename ImageType::Pointer;
  // using ImageConstPointer = typename ImageType::ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( TileMontage, ProcessObject );

  /** Dimensionality of input images. */
  itkStaticConstMacro( ImageDimension, unsigned int, ImageType::ImageDimension );

  /** Montage size and tile index types. */
  using SizeType = Size< ImageDimension >;
  using TileIndexType = Size< ImageDimension >;
  using ContinuousIndexType = ContinuousIndex< TCoordinate, ImageDimension >;

  /** Image's dependent types. */
  using PixelType = typename ImageType::PixelType;
  using RegionType = typename ImageType::RegionType;
  using PointType = typename ImageType::PointType;
  using SpacingType = typename ImageType::SpacingType;
  using OffsetType = typename ImageType::OffsetType;
  using ImageIndexType = typename ImageType::IndexType;

  /** Internal PhaseCorrelationImageRegistrationMethod's type alias. */
  using PCMType = PhaseCorrelationImageRegistrationMethod< ImageType, ImageType >;

  using RealType = typename itk::NumericTraits< PixelType >::RealType;

  using PCMOperatorType = itk::PhaseCorrelationOperator< RealType, ImageDimension >;

  using PCMOptimizerType = itk::MaxPhaseCorrelationOptimizer< PCMType >;

  /**  Type for the transform. */
  using TransformType = typename PCMType::TransformType;
  using TransformPointer = typename TransformType::Pointer;
  using TransformConstPointer = typename TransformType::ConstPointer;

  /** Type for the output: Using Decorator pattern for enabling
   *  the Transform to be passed in the data pipeline */
  using TransformOutputType = DataObjectDecorator< TransformType >;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = typename DataObject::Pointer;

  /** Passes ReleaseDataFlag to internal filters. */
  void SetReleaseDataFlag( bool flag ) override
  {
    Superclass::SetReleaseDataFlag( flag );
    m_PCM->SetReleaseDataFlag( flag );
  }

  /** Passes ReleaseDataBeforeUpdateFlag to internal filters. */
  void SetReleaseDataBeforeUpdateFlag( const bool flag ) override
  {
    Superclass::SetReleaseDataBeforeUpdateFlag( flag );
    m_PCM->SetReleaseDataBeforeUpdateFlag( flag );
  }

  /** Set/Get the OriginAdjustment. Origin adjustment multiplied by tile index
   * is added to origin of images when only their filename is specified.
   * This allows assumed positions for tiles even if files have zero origin. */
  itkSetMacro( OriginAdjustment, PointType );
  itkGetConstMacro( OriginAdjustment, PointType );

  /** Set/Get forced spacing.
   * If set, overrides spacing for images read from files. */
  itkSetMacro( ForcedSpacing, SpacingType );
  itkGetConstMacro( ForcedSpacing, SpacingType );

  /** Set/Get obligatory padding.
   * If set, padding of this many pixels is added on both beginning and end
   * sides of each dimension of the image. */
  virtual void SetObligatoryPadding( const SizeType pad )
  {
    if ( this->m_ObligatoryPadding != pad )
      {
      this->m_ObligatoryPadding = pad;
      m_PCM->SetObligatoryPadding( pad );
      this->Modified();
      }
  }
  itkGetConstMacro( ObligatoryPadding, SizeType );

  /** Set/Get the PhaseCorrelationImageRegistrationMethod. */
  virtual void SetPCM( PCMType* pcm )
  {
    if ( this->m_PCM != pcm )
      {
      this->m_PCM = pcm;
      m_PCM->SetObligatoryPadding( m_ObligatoryPadding );
      this->Modified();
      }
  }
  itkGetModifiableObjectMacro( PCM, PCMType );

  /** Set/Get the PhaseCorrelationImageRegistrationMethod. */
  itkSetObjectMacro( PCMOptimizer, PCMOptimizerType );
  itkGetModifiableObjectMacro( PCMOptimizer, PCMOptimizerType );

  /** Get/Set size of the image mosaic. */
  itkGetConstMacro( MontageSize, SizeType );
  void SetMontageSize( SizeType montageSize );

  /** To be called for each tile position in the mosaic
   * before the call to Update(). */
  void SetInputTile( TileIndexType position, ImageType* image )
  {
    SizeValueType linearIndex = this->nDIndexToLinearIndex( position );
    this->SetNthInput( linearIndex, image );
    m_FFTCache[linearIndex] = nullptr;
  }
  void SetInputTile( TileIndexType position, const std::string& imageFilename )
  {
    SizeValueType linearIndex = this->nDIndexToLinearIndex( position );
    m_Filenames[linearIndex] = imageFilename;
    this->SetInputTile( position, m_Dummy );
  }

  /** After Update(), the transform for each tile is available. */
  TransformConstPointer GetOutputTransform( TileIndexType position )
  {
    return static_cast< TransformOutputType* >( this->GetOutput( this->nDIndexToLinearIndex( position ) ) )->Get();
  }

protected:
  TileMontage();
  virtual ~TileMontage(){};
  void PrintSelf( std::ostream& os, Indent indent ) const override;

  /** Method invoked by the pipeline in order to trigger the computation of the registration. */
  void GenerateData() override;

  /** Method invoked by the pipeline to determine the output information. */
  void GenerateOutputInformation() override;

  using Superclass::MakeOutput;

  /** Make a DataObject of the correct type to be used as the specified output. */
  DataObjectPointer MakeOutput( DataObjectPointerArraySizeType ) override
  {
    return TransformOutputType::New();
  }

  /** For reading if only filename was given. */
  using ReaderType = itk::ImageFileReader< ImageType >;

  template <typename TImageToRead>
  typename TImageToRead::Pointer
  GetImageHelper( TileIndexType nDIndex, bool metadataOnly, RegionType region,
                  ImageFileReader< TImageToRead >* reader = nullptr );

  /** Just get image pointer if the image is present, otherwise read it from file. */
  typename ImageType::Pointer GetImage( TileIndexType nDIndex, bool metadataOnly );

  DataObjectPointerArraySizeType nDIndexToLinearIndex( TileIndexType nDIndex ) const;
  TileIndexType LinearIndexTonDIndex( DataObjectPointerArraySizeType linearIndex ) const;

  /** Register a pair of images with given indices. Handles FFTcaching. */
  TransformPointer RegisterPair( TileIndexType fixed, TileIndexType moving );

  /** If possible, removes from memory tile with index smaller by 1 along all dimensions. */
  void ReleaseMemory( TileIndexType finishedTile );

  /** Montage this dimension, and all lower dimensions. */
  void MontageDimension( int d, TileIndexType initialTile );

  /** Accesses output, sets a transform to it, and updates progress. */
  void WriteOutTransform( TileIndexType index, TransformPointer transform );

  /** Updates mosaic bounds. The transform applies to input.
   *  input0 is tile in the top-left corner. */
  void UpdateMosaicBounds(
      TileIndexType index,
      TransformConstPointer transform,
      const ImageType *input,
      const ImageType *input0 );

  /** Image's FFT type. */
  using FFTType = typename PCMType::ComplexImageType;
  using FFTPointer = typename FFTType::Pointer;
  using FFTConstPointer = typename FFTType::ConstPointer;

  using TransformVector = std::vector< TransformPointer >;
  using ConfidencesType = typename PCMType::ConfidencesVector;

  // convets translation from index space into physical space
  TransformPointer OffsetToTransform( const typename PCMOptimizerType::OffsetType& translation,
                                      typename ImageType::Pointer tileInformation );

  void OptimizeTiles();

private:
  SizeType      m_MontageSize;
  SizeValueType m_LinearMontageSize = 0;
  SizeValueType m_FinishedTiles = 0;
  PointType     m_OriginAdjustment;
  SpacingType   m_ForcedSpacing;
  SizeType      m_ObligatoryPadding;

  std::vector< std::string >      m_Filenames;
  std::vector< FFTConstPointer >  m_FFTCache;
  std::vector< TransformVector >  m_TransformCandidates; // to adjacent tiles
  std::vector< ConfidencesType >  m_CandidateConfidences;
  std::vector< TransformPointer > m_CurrentAdjustments;
  typename PCMType::Pointer       m_PCM = PCMType::New();
  typename ReaderType::Pointer    m_Reader = ReaderType::New();
  typename ImageType::Pointer     m_Dummy = ImageType::New();

  typename PCMOperatorType::Pointer  m_PCMOperator = PCMOperatorType::New();
  typename PCMOptimizerType::Pointer m_PCMOptimizer = PCMOptimizerType::New();

  // members needed for ResampleIntoSingleImage
  ContinuousIndexType m_MinInner; // minimum index for cropped montage
  ContinuousIndexType m_MaxInner; // maximum index for cropped montage
  ContinuousIndexType m_MinOuter; // minimum index for total montage
  ContinuousIndexType m_MaxOuter; // maximum index for total montage

  template< typename TImageTypeInner, typename TPixelAccumulateType, typename TInterpolatorInner >
  friend class TileMergeImageFilter;
}; // class TileMontage

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTileMontage.hxx"
#endif

#endif // itkTileMontage_h
