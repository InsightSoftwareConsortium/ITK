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

#include <atomic>
#include <deque>
#include <mutex>
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
template <typename TImageType,
          typename TCoordinate =
            typename std::conditional<std::is_same<typename TImageType::PixelType, double>::value, double, float>::type>
class ITK_TEMPLATE_EXPORT TileMontage : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TileMontage);

  /** Standard class type aliases. */
  using Self = TileMontage;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ImageType = TImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TileMontage, ProcessObject);

  /** Dimensionality of input images. */
  itkStaticConstMacro(ImageDimension, unsigned int, ImageType::ImageDimension);

  /** Montage size and tile index types. */
  using SizeType = Size<ImageDimension>;
  using TileIndexType = Size<ImageDimension>;
  using ContinuousIndexType = ContinuousIndex<TCoordinate, ImageDimension>;

  /** Image's dependent types. */
  using PixelType = typename ImageType::PixelType;
  using RegionType = typename ImageType::RegionType;
  using PointType = typename ImageType::PointType;
  using SpacingType = typename ImageType::SpacingType;
  using OffsetType = typename ImageType::OffsetType;
  using ImageIndexType = typename ImageType::IndexType;

  /** Internal PhaseCorrelationImageRegistrationMethod's type alias. */
  using PCMType = PhaseCorrelationImageRegistrationMethod<ImageType, ImageType, TCoordinate>;

  using RealType = typename PCMType::InternalPixelType;

  using PCMOperatorType = PhaseCorrelationOperator<RealType, ImageDimension>;

  using PCMOptimizerType = MaxPhaseCorrelationOptimizer<PCMType>;

  /**  Type for the transform. */
  using TransformType = typename PCMType::TransformType;
  using TransformPointer = typename TransformType::Pointer;
  using TransformConstPointer = typename TransformType::ConstPointer;

  /** Type for the output: Using Decorator pattern for enabling
   *  the Transform to be passed in the data pipeline */
  using TransformOutputType = DataObjectDecorator<TransformType>;

  /** Smart Pointer type to a DataObject. */
  using DataObjectPointer = typename DataObject::Pointer;

  /** Set/Get the OriginAdjustment. Origin adjustment multiplied by tile index
   * is added to origin of images when only their filename is specified.
   * This allows assumed positions for tiles even if files have zero origin. */
  itkSetMacro(OriginAdjustment, PointType);
  itkGetConstMacro(OriginAdjustment, PointType);

  /** Set/Get forced spacing.
   * If set, overrides spacing for images read from files. */
  itkSetMacro(ForcedSpacing, SpacingType);
  itkGetConstMacro(ForcedSpacing, SpacingType);

  /** Set/Get absolute registration threshold.
   * The maximum allowed residual error for a registration pair during
   * global optimization. Expressed in number of pixels. Default: 1.0.
   * When a registration pair exceeds the threshold, it is replaced by
   * the next best candidate for that pair. If all canidates are
   * exhausted, the registration pair is assumed to have no translation.
   * The weight of this equation is also significantly reduced. */
  itkSetMacro(AbsoluteThreshold, float);
  itkGetConstMacro(AbsoluteThreshold, float);

  /** Set/Get relative registration threshold.
   * The maximum allowed deviation for a registration pair during global
   * optimization. Expressed in multiples of standard deviation. Default: 3.0.
   * The deviation is calculated by taking the translations of all the
   * registration pairs, while assuming zero mean. This implies expectation
   * that deviations from expected positions for all registration pairs
   * are similar. The pairs that don't satify this will be penalized. */
  itkSetMacro(RelativeThreshold, float);
  itkGetConstMacro(RelativeThreshold, float);

  /** Set/Get tile positioning precision.
   * Get/Set expected maximum linear translation needed, in pixels.
   * Zero (the default) means unknown, and allows translations
   * up to about half the image size.*/
  itkSetMacro(PositionTolerance, SizeValueType);
  itkGetConstMacro(PositionTolerance, SizeValueType);

  /** Set/Get tile cropping. Should tiles be cropped to overlapping
   * region for computing the cross correlation? Default: True.
   *
   * This improves results, and in case overlaps are less than 25%
   * computation is also faster. */
  itkSetMacro(CropToOverlap, bool);
  itkGetConstMacro(CropToOverlap, bool);

  /** Set/Get obligatory padding.
   * If set, padding of this many pixels is added on both beginning and end
   * sides of each dimension of the image. */
  virtual void
  SetObligatoryPadding(const SizeType pad)
  {
    if (this->m_ObligatoryPadding != pad)
    {
      this->m_ObligatoryPadding = pad;
      this->Modified();
    }
  }
  itkGetConstMacro(ObligatoryPadding, SizeType);

  /** Set/Get the padding method. */
  itkSetMacro(PaddingMethod, typename PCMType::PaddingMethod);
  itkGetConstMacro(PaddingMethod, typename PCMType::PaddingMethod);

  /** Set/Get the peak interpolation method. */
  itkSetMacro(PeakInterpolationMethod, typename PCMOptimizerType::PeakInterpolationMethod);
  itkGetConstMacro(PeakInterpolationMethod, typename PCMOptimizerType::PeakInterpolationMethod);

  /** Get/Set size of the image mosaic. */
  itkGetConstMacro(MontageSize, SizeType);
  void
  SetMontageSize(SizeType montageSize);

  /** To be called for each tile position in the mosaic
   * before the call to Update(). */
  void
  SetInputTile(SizeValueType linearIndex, ImageType * image)
  {
    this->SetNthInput(linearIndex, image);
    m_FFTCache[linearIndex] = nullptr;
    m_Tiles[linearIndex] = nullptr;
  }
  void
  SetInputTile(SizeValueType linearIndex, const std::string & imageFilename)
  {
    m_Filenames[linearIndex] = imageFilename;
    this->SetInputTile(linearIndex, m_Dummy);
  }
  void
  SetInputTile(TileIndexType position, ImageType * image)
  {
    this->SetInputTile(this->nDIndexToLinearIndex(position), image);
  }
  void
  SetInputTile(TileIndexType position, const std::string & imageFilename)
  {
    this->SetInputTile(this->nDIndexToLinearIndex(position), imageFilename);
  }

  /** After Update(), the transform for each tile is available. */
  TransformConstPointer
  GetOutputTransform(TileIndexType position)
  {
    return static_cast<TransformOutputType *>(this->GetOutput(this->nDIndexToLinearIndex(position)))->Get();
  }

protected:
  TileMontage();
  virtual ~TileMontage(){};
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Method invoked by the pipeline in order to trigger the computation of the registration. */
  void
  GenerateData() override;

  /** Method invoked by the pipeline to determine the output information. */
  void
  GenerateOutputInformation() override;

  using Superclass::MakeOutput;

  /** Make a DataObject of the correct type to be used as the specified output. */
  DataObjectPointer MakeOutput(DataObjectPointerArraySizeType) override { return TransformOutputType::New(); }

  /** For reading if only filename was given. */
  using ReaderType = ImageFileReader<ImageType>;

  using TranslationOffset = typename TransformType::OutputVectorType;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;

  template <typename TImageToRead>
  typename TImageToRead::Pointer
  GetImageHelper(TileIndexType nDIndex, bool metadataOnly, RegionType region);

  /** Just get image pointer if the image is present, otherwise read it from file. */
  typename ImageType::Pointer
  GetImage(TileIndexType nDIndex, bool metadataOnly);

  DataObjectPointerArraySizeType
  nDIndexToLinearIndex(TileIndexType nDIndex) const;
  TileIndexType
  LinearIndexTonDIndex(DataObjectPointerArraySizeType linearIndex) const;

  /** Register a pair of images with given indices. Handles FFTcaching. */
  void
  RegisterPair(TileIndexType fixed, TileIndexType moving);

  /** If possible, removes from memory tile with index smaller by 1 along all dimensions. */
  void
  ReleaseMemory(TileIndexType finishedTile);

  /** Accesses output, sets a transform to it, and updates progress. */
  void
  WriteOutTransform(TileIndexType index, TranslationOffset offset);

  /** Updates mosaic bounds. The transform applies to input.
   *  input0 is tile in the top-left corner. */
  void
  UpdateMosaicBounds(TileIndexType         index,
                     TransformConstPointer transform,
                     const ImageType *     input,
                     const ImageType *     input0);

  /** Image's FFT type. */
  using FFTType = typename PCMType::ComplexImageType;
  using FFTPointer = typename FFTType::Pointer;
  using FFTConstPointer = typename FFTType::ConstPointer;

  using OffsetVector = std::vector<TranslationOffset>;
  using ConfidencesType = typename PCMType::ConfidencesVector;

  void
  OptimizeTiles();

  std::deque<std::mutex> m_TileReadLocks; // to avoid reading the same tile by more than one thread in parallel
  // deque is not reallocated when resized, so no mutex moving causing a crash

private:
  SizeType      m_MontageSize;
  SizeValueType m_LinearMontageSize = 0;
  SizeValueType m_NumberOfPairs = 0;

  std::atomic<SizeValueType> m_FinishedPairs = { 0 };

  PointType     m_OriginAdjustment;
  SpacingType   m_ForcedSpacing;
  float         m_AbsoluteThreshold = 1.0;
  float         m_RelativeThreshold = 3.0;
  SizeValueType m_PositionTolerance = 0;
  bool          m_CropToOverlap = true;
  SizeType      m_ObligatoryPadding;

  std::mutex m_MemberProtector; // to prevent concurrent access to non-thread-safe internal member variables

  typename PCMType::PaddingMethod m_PaddingMethod = PCMType::PaddingMethod::MirrorWithExponentialDecay;
  std::vector<std::string>        m_Filenames;
  std::vector<FFTConstPointer>    m_FFTCache;
  std::vector<ImagePointer>       m_Tiles; // metadata/image storage (if filenames are given instead of actual images)
  std::vector<OffsetVector>       m_TransformCandidates; // to adjacent tiles
  std::vector<ConfidencesType>    m_CandidateConfidences;
  std::vector<TranslationOffset>  m_CurrentAdjustments;

  typename PCMOptimizerType::PeakInterpolationMethod m_PeakInterpolationMethod =
    PCMOptimizerType::PeakInterpolationMethod::Parabolic;

  const typename ImageType::Pointer m_Dummy = ImageType::New();

  // members needed for ResampleIntoSingleImage
  ContinuousIndexType m_MinInner; // minimum index for cropped montage
  ContinuousIndexType m_MaxInner; // maximum index for cropped montage
  ContinuousIndexType m_MinOuter; // minimum index for total montage
  ContinuousIndexType m_MaxOuter; // maximum index for total montage

  template <typename TImageTypeInner, typename TPixelAccumulateType, typename TInterpolatorInner>
  friend class TileMergeImageFilter;
}; // class TileMontage

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTileMontage.hxx"
#endif

#endif // itkTileMontage_h
