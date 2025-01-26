/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkTileMergeImageFilter_h
#define itkTileMergeImageFilter_h

#include "itkTileMontage.h"

#include "itkLinearInterpolateImageFunction.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class TileMergeImageFilter
 * \brief Resamples an n-Dimensional mosaic of images into a single composite image.
 *
 * CropToFill indicates whether the composite image will be cropped so it
 * entirely consists of input tiles (no default background filling).
 * If CropToFill is false, the composite image will have the extent to include
 * all of the input tiles. The pixels not covered by any input tile
 * will have the value specified by the Background member variable.
 *
 * TPixelAccumulateType needs to allow bigger numbers when overlap regions are large.
 * For example, char's default accumulation type is short,
 * but int might be preferred for montages with large overlaps of input tiles.
 *
 * \author Dženan Zukić, dzenan.zukic@kitware.com
 *
 * \ingroup Montage
 */
template <typename TImageType,
          typename TPixelAccumulateType = typename NumericTraits<typename TImageType::PixelType>::AccumulateType,
          typename TInterpolator = LinearInterpolateImageFunction<TImageType, float>>
class ITK_TEMPLATE_EXPORT TileMergeImageFilter
  : public TileMontage<
      Image<typename NumericTraits<typename TImageType::PixelType>::ValueType, TImageType::ImageDimension>,
      typename TInterpolator::CoordinateType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TileMergeImageFilter);

  /** We define superclass with scalar pixel type, to enable compiling even when RGB pixel is supplied. */
  using Superclass =
    TileMontage<Image<typename NumericTraits<typename TImageType::PixelType>::ValueType, TImageType::ImageDimension>,
                typename TInterpolator::CoordinateType>;

  /** Standard class type aliases. */
  using Self = TileMergeImageFilter;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ImageType = TImageType;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(TileMergeImageFilter);

  /** Dimensionality of input images. */
  static constexpr unsigned int ImageDimension = ImageType::ImageDimension;

  /** This is envisioned to be the primary way of setting inputs.
   * All required inputs are taken from TileMontage. Alternatively,
   * inherited members can be called individually, e.g.:
   * SetMontageSize(), SetInputTile(), SetOriginAdjustment() etc. */
  void
  SetMontage(const Superclass * montage);

  /** Montage size and tile index types. */
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;
  using SizeType = typename Superclass::SizeType;
  using TileIndexType = typename Superclass::TileIndexType;

  /** Image's dependent types. */
  using PixelType = typename TImageType::PixelType;
  using ImageIndexType = typename Superclass::ImageIndexType;
  using OffsetType = typename Superclass::OffsetType;
  using PointType = typename Superclass::PointType;
  using RegionType = typename Superclass::RegionType;
  using SpacingType = typename Superclass::SpacingType;

  /**  Type for the transform. */
  using TransformType = typename Superclass::PCMType::TransformType;
  using TransformPointer = typename TransformType::Pointer;
  using TransformConstPointer = typename TransformType::ConstPointer;

  /** Passes ReleaseDataFlag to internal filters. */
  void
  SetReleaseDataFlag(bool flag) override
  {
    Superclass::SetReleaseDataFlag(flag);
  }

  /** Passes ReleaseDataBeforeUpdateFlag to internal filters. */
  void
  SetReleaseDataBeforeUpdateFlag(const bool flag) override
  {
    Superclass::SetReleaseDataBeforeUpdateFlag(flag);
  }

  /** Usage equivalent to ImageSource's GetOutput().
   * \sa ImageSource */
  ImageType *
  GetOutput();
  const ImageType *
  GetOutput() const;
  ImageType *
  GetOutput(unsigned int idx);

  /** Set size of the image mosaic. */
  void
  SetMontageSize(SizeType montageSize);

  /** To be called for each tile position in the mosaic
   * before the call to Update(). */
  void
  SetInputTile(SizeValueType linearIndex, ImageType * image)
  {
    // image will be cast into DataObject* so this casting is not a problem
    Superclass::SetInputTile(linearIndex, reinterpret_cast<typename Superclass::ImageType *>(image));
    m_Transforms[linearIndex] = nullptr;
    m_Tiles[linearIndex] = nullptr;
  }
  void
  SetInputTile(SizeValueType linearIndex, const std::string & imageFilename)
  {
    Superclass::SetInputTile(linearIndex, imageFilename);
    m_Transforms[linearIndex] = nullptr;
    m_Tiles[linearIndex] = nullptr;
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

  /** Input tiles' transforms, as calculated by \sa{Montage}.
   * To be called for each tile position in the mosaic
   * before the call to Update(). */
  void
  SetTileTransform(TileIndexType position, const TransformType * transform);

  /** Get/Set background value (used if CropToFill is false).
   * Default PixelType's value (usually zero) if not set. */
  itkSetMacro(Background, PixelType);
  itkGetMacro(Background, PixelType);

  /** CropToFill indicates whether the output image will be cropped so it
   * entirely consists of input tiles (no default background filling).
   * If CropToFill is false, the composite image will have the extent to include
   * all of the input tiles. The pixels not covered by any input tile
   * will have the value specified by the Background member variable. */
  itkSetMacro(CropToFill, bool);
  itkGetMacro(CropToFill, bool);
  itkBooleanMacro(CropToFill);

protected:
  TileMergeImageFilter();
  ~TileMergeImageFilter() override = default;
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
  typename DataObject::Pointer MakeOutput(typename Superclass::DataObjectPointerArraySizeType) override
  {
    return ImageType::New();
  }

  /** If not already read, reads the image into memory.
   * Only the part which overlaps output image's requested region is read.
   * If size of the wantedRegion is zero, only reads metadata. */
  ImageConstPointer
  GetImage(TileIndexType nDIndex, RegionType wantedRegion);

  /** A set of linear indices of input tiles which contribute to this region. */
  using ContributingTiles = std::set<SizeValueType>;

  void
  SplitRegionAndCopyContributions(std::vector<RegionType> &        regions,
                                  std::vector<ContributingTiles> & regionContributors,
                                  RegionType                       newRegion,
                                  size_t                           oldRegionIndex,
                                  SizeValueType                    tileIndex);

  /** The region will be inside of the rectangle given by min and max indices.
   * The min is rounded up, while the max is rounded down. */
  RegionType
  ConstructRegion(ContinuousIndexType minIndex, ContinuousIndexType maxIndex);

  /** Calculates distance of index from the closes edge of the region. */
  SizeValueType
  DistanceFromEdge(ImageIndexType index, RegionType region);

  /** Resamples a single region into m_SingleImage.
   * This method does not access other regions,
   * and can be run in parallel with other indices. */
  void
  ResampleSingleRegion(SizeValueType regionIndex);

private:
  bool      m_CropToFill = false;       // crop to avoid background filling?
  PixelType m_Background = PixelType(); // default background value (not covered by any input tile)

  std::vector<TransformConstPointer> m_Transforms;
  std::vector<ImagePointer>         m_Tiles; // metadata/image storage (if filenames are given instead of actual images)
  typename Superclass::ConstPointer m_Montage;
  std::vector<RegionType>           m_InputMappings;           // where do input tile regions map into the output
  std::vector<ContinuousIndexType>  m_InputsContinuousIndices; // where do input tile region indices map into the output
  std::vector<RegionType>           m_Regions;                 // regions which completely cover the output,
                                                               // grouped by the set of contributing input tiles
  std::vector<ContributingTiles> m_RegionContributors; // set of input tiles which contribute to corresponding regions
};                                                     // class TileMergeImageFilter

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTileMergeImageFilter.hxx"
#endif

#endif // itkTileMergeImageFilter_h
