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

#ifndef itkTileMerging_h
#define itkTileMerging_h

#include "itkTileMontage.h"
#include "itkLinearInterpolateImageFunction.h"

namespace itk
{
/** \class TileMerging
 * \brief Resamples an n-Dimensional mosaic of images into a single big image.
 *
 * CropToFill indicates whether the big image will be cropped so it
 * entirely consists of input tiles (no default background filling).
 * If CropToFill is false, the big image will have the extent to include
 * all of the input tiles. The pixels not covered by any input tile
 * will have the value specified by the Background member variable.
 *
 * \author Dženan Zukić, dzenan.zukic@kitware.com
 *
 * \ingroup Montage
 */
template <typename TImageType, typename TInterpolator = LinearInterpolateImageFunction<TImageType, float> >
class ITK_TEMPLATE_EXPORT TileMerging: public TileMontage<TImageType, typename TInterpolator::CoordRepType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TileMerging);

  /** Standard class type aliases. */
  using Self = TileMerging;
  using Superclass = TileMontage<TImageType, typename TInterpolator::CoordRepType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;
  using ImageType = TImageType;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TileMerging, TileMontage);

  /** Dimensionality of input images. */
  itkStaticConstMacro(ImageDimension, unsigned int, ImageType::ImageDimension);

  /** This is envisioned to be the primary way of setting inputs.
   * All required inputs are taken from TileMontage. Alternatively,
   * inherited members can be called individually, e.g.:
   * SetMontageSize(), SetInputTile(), SetOriginAdjustment() etc. */
  void SetMontage(const Superclass* montage);

  /** Montage size and tile index types. */
  using typename Superclass::SizeType;
  using typename Superclass::TileIndexType;
  using typename Superclass::ContinuousIndexType;

  /** Image's dependent types. */
  using typename Superclass::PixelType;
  using typename Superclass::RegionType;//using RegionType = typename Superclass::RegionType;
  using typename Superclass::PointType;
  using typename Superclass::SpacingType;
  using typename Superclass::OffsetType;
  using typename Superclass::ImageIndexType;

  /**  Type for the transform. */
  using TransformType = typename Superclass::PCMType::TransformType;
  using TransformPointer = typename TransformType::Pointer;
  using TransformConstPointer = typename TransformType::ConstPointer;

  ///** Type for the output: Using Decorator pattern for enabling
  //*  the Transform to be passed in the data pipeline */
  //using TransformOutputType = DataObjectDecorator< TransformType >;

  ///** Smart Pointer type to a DataObject. */
  //using DataObjectPointer = typename DataObject::Pointer;

  /** Passes ReleaseDataFlag to internal filters. */
  void SetReleaseDataFlag(bool flag) override
  {
    Superclass::SetReleaseDataFlag(flag);
  }

  /** Passes ReleaseDataBeforeUpdateFlag to internal filters. */
  void SetReleaseDataBeforeUpdateFlag(const bool flag) override
  {
    Superclass::SetReleaseDataBeforeUpdateFlag(flag);
  }

  /** Usage equivalent to ImageSource's GetOutput().
   * \sa ImageSource */
  ImageType * GetOutput();
  const ImageType * GetOutput() const;
  ImageType * GetOutput(unsigned int idx);

  /** Set size of the image mosaic. */
  void SetMontageSize(SizeType montageSize);

  /** To be called for each tile position in the mosaic
   * before the call to Update(). */
  void SetInputTile(TileIndexType position, ImageType* image)
  {
    SizeValueType linInd = this->nDIndexToLinearIndex(position);
    Superclass::SetInputTile(position, image);
    m_Transforms[linInd] = nullptr;
    m_Metadata[linInd] = nullptr;
  }
  void SetInputTile(TileIndexType position, const std::string& imageFilename)
  {
    Superclass::SetInputTile(position, imageFilename);
    SizeValueType linInd = this->nDIndexToLinearIndex(position);
    m_Transforms[linInd] = nullptr;
    m_Metadata[linInd] = nullptr;
  }

  /** Input tiles' transforms, as calculated by \sa{Montage}.
   * To be called for each tile position in the mosaic
   * before the call to Update(). */
  void SetTileTransform(TileIndexType position, TransformConstPointer transform);

  /** Get/Set background value (used if CropToFill is false).
   * Default PixelType's value (usually zero) if not set. */
  itkSetMacro(Background, PixelType);
  itkGetMacro(Background, PixelType);

  /** CropToFill indicates whether the output image will be cropped so it
   * entirely consists of input tiles (no default background filling).
   * If CropToFill is false, the big image will have the extent to include
   * all of the input tiles. The pixels not covered by any input tile
   * will have the value specified by the Background member variable. */
  itkSetMacro(CropToFill, bool);
  itkGetMacro(CropToFill, bool);
  itkBooleanMacro(CropToFill);

protected:
  TileMerging();
  virtual ~TileMerging() {};
  void PrintSelf(std::ostream& os, Indent indent) const override;

  /** Method invoked by the pipeline in order to trigger the computation of the registration. */
  void GenerateData() override;

  /** Method invoked by the pipeline to determine the output information. */
  void GenerateOutputInformation() override;

  /** Make a DataObject of the correct type to be used as the specified output. */
  typename DataObject::Pointer MakeOutput(typename Superclass::DataObjectPointerArraySizeType idx) override
  {
    return ImageType::New();
  }

  /** If not already read, reads the image into memory.
   * Ensures that the wanted region is buffered.
   * If size of the wantedRegion is zero, only reads metadata. */
  ImageConstPointer GetImage(TileIndexType nDIndex, RegionType wantedRegion);

  /** A set of linear indices of input tiles which contribute to this region. */
  using ContributingTiles = std::set<SizeValueType>;

  void SplitRegionAndCopyContributions(
      std::vector<RegionType>& regions,
      std::vector<ContributingTiles>& regionContributors,
      RegionType newRegion,
      size_t oldRegionIndex,
      SizeValueType tileIndex);

  /** The region will be inside of the rectangle given by min and max indices.
   * The min is rounded up, while the max is rounded down. */
  RegionType ConstructRegion(ContinuousIndexType minIndex, ContinuousIndexType maxIndex);

  /** Calculates distance of index from the closes edge of the region. */
  SizeValueType DistanceFromEdge(ImageIndexType index, RegionType region);

  /** Resamples a single region into m_SingleImage.
   * This method does not access other regions,
   * and can be run in parallel with other indices. */
  void ResampleSingleRegion(unsigned regionIndex);

private:
    std::vector<TransformConstPointer>       m_Transforms;
    std::vector<typename ImageType::Pointer> m_Metadata; // images with empty buffers

    bool             m_CropToFill; //crop to avoid backfround filling?
    PixelType        m_Background; //default background value (not covered by any input tile)

    typename Superclass::ConstPointer m_Montage;
    std::vector<RegionType>           m_InputMappings; //where do input tile regions map into the output
    std::vector<ContinuousIndexType>  m_InputsContinuousIndices; //where do input tile region indices map into the output
    std::vector<RegionType>           m_Regions; //regions which completely cover the output,
                                                 //grouped by the set of contributing input tiles
    std::vector<ContributingTiles>    m_RegionContributors; //set of input tiles which contribute to corresponding regions
}; // class TileMerging

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTileMerging.hxx"
#endif

#endif //itkTileMerging_h
