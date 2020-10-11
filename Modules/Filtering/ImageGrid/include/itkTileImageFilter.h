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
#ifndef itkTileImageFilter_h
#define itkTileImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkFixedArray.h"

namespace itk
{
/**
 *\class TileImageFilter
 * \brief Tile multiple input images into a single output image.
 *
 * This filter will tile multiple images using a user-specified
 * layout. The tile sizes will be large enough to accommodate the
 * largest image for each tile. The layout is specified with the
 * SetLayout method. The layout has the same dimension as the output
 * image. If all entries of the layout are positive, the tiled output
 * will contain the exact number of tiles. If the layout contains a 0
 * in the last dimension, the filter will compute a size that will
 * accommodate all of the images. Empty tiles are filled with the
 * value specified with the SetDefault value method. The input images
 * must have a dimension less than or equal to the output image. The
 * output image have a larger dimension than the input images. This
 * filter can be used to create a volume from a series of inputs by
 * specifying a layout of 1,1,0.
 * \ingroup ITKImageGrid
 *
 * \sphinx
 * \sphinxexample{Filtering/ImageGrid/Stack2DImagesInto3DImage,Stack 2D Images Into 3D Image}
 * \sphinxexample{Filtering/ImageGrid/TileImagesSideBySide,Tile Images Side By Side}
 * \endsphinx
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT TileImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TileImageFilter);

  /** Standard Self type alias */
  using Self = TileImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(TileImageFilter, ImageToImageFilter);

  /** Image pixel value type alias. */
  using InputPixelType = typename TInputImage::PixelType;
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputPixelComponentType = typename NumericTraits<OutputPixelType>::ValueType;

  /** Image related type alias. */
  using InputImagePointer = typename TInputImage::Pointer;
  using OutputImagePointer = typename TOutputImage::Pointer;


  using InputSizeType = typename TInputImage::SizeType;
  using InputIndexType = typename TInputImage::IndexType;
  using InputImageRegionType = typename TInputImage::RegionType;
  using OutputSizeType = typename TOutputImage::SizeType;
  using OutputIndexType = typename TOutputImage::IndexType;
  using OutputImageRegionType = typename TOutputImage::RegionType;

  /** Image related type alias. */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;
  /**
   *\class TileInfo
   * Define a tile structure
   * \ingroup ITKImageGrid
   */
  class TileInfo
  {
  public:
    int                   m_ImageNumber{ -1 };
    OutputImageRegionType m_Region;
    TileInfo() = default;
  };

  using TileImageType = Image<TileInfo, Self::OutputImageDimension>;

  /** LayoutArray type. */
  using LayoutArrayType = FixedArray<unsigned int, Self::OutputImageDimension>;

  /** Set/Get the layout of the tiles. If the last Layout value is 0,
   * the filter will compute a value that will accommodate all of the
   * images. */
  itkSetMacro(Layout, LayoutArrayType);
  itkGetConstMacro(Layout, LayoutArrayType);

  /** Set the pixel value for locations that are not covered by an
   * input image. The default default pixel value is Zero. */
  itkSetMacro(DefaultPixelValue, OutputPixelType);

  /** Get the pixel value for locations that are not covered by an
   * input image. */
  itkGetConstMacro(DefaultPixelValue, OutputPixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputEqualityComparableCheck, (Concept::EqualityComparable<OutputPixelType>));
  itkConceptMacro(SameTypeCheck, (Concept::SameType<InputPixelType, OutputPixelType>));
  itkConceptMacro(OutputOStreamWritableCheck, (Concept::OStreamWritable<OutputPixelType>));
  // End concept checking
#endif

protected:
  TileImageFilter();
  ~TileImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;

  void
  GenerateOutputInformation() override;

  void
  GenerateData() override;

  /** Override VerifyInputInformation() since this filter's inputs do
   * not need to occupy the same physical space.
   *
   * \sa ProcessObject::VerifyInputInformation
   */
  void
  VerifyInputInformation() ITKv5_CONST override;

private:
  typename TileImageType::Pointer m_TileImage;

  OutputPixelType m_DefaultPixelValue;

  LayoutArrayType m_Layout;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTileImageFilter.hxx"
#endif

#endif
