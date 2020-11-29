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
#ifndef itkScalarToRGBColormapImageFilter_h
#define itkScalarToRGBColormapImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkColormapFunction.h"
#include "ITKColormapExport.h"

namespace itk
{
/**\class ScalarToRGBColormapImageFilterEnums
 * \brief Contains all enum classes for ScalarToRGBColormapImageFilter class
 * \ingroup ITKColormap
 */
class ScalarToRGBColormapImageFilterEnums
{
public:
  /**\class RGBColormapFilter
   * \ingroup ITKColormap
   * Enum type that provides for an easy interface to existing colormaps. */
  enum class RGBColormapFilter : uint8_t
  {
    Red,
    Green,
    Blue,
    Grey,
    Hot,
    Cool,
    Spring,
    Summer,
    Autumn,
    Winter,
    Copper,
    Jet,
    HSV,
    OverUnder
  };
};
#if !defined(ITK_LEGACY_REMOVE)
using RGBColormapFilterEnum = ScalarToRGBColormapImageFilterEnums::RGBColormapFilter;
using RGBColormapFilterEnumType = ScalarToRGBColormapImageFilterEnums::RGBColormapFilter;
// We need to expose the enum values at the class level
// for backwards compatibility
static constexpr RGBColormapFilterEnum Red = RGBColormapFilterEnum::Red;
static constexpr RGBColormapFilterEnum Green = RGBColormapFilterEnum::Green;
static constexpr RGBColormapFilterEnum Blue = RGBColormapFilterEnum::Blue;
static constexpr RGBColormapFilterEnum Grey = RGBColormapFilterEnum::Grey;
static constexpr RGBColormapFilterEnum Hot = RGBColormapFilterEnum::Hot;
static constexpr RGBColormapFilterEnum Cool = RGBColormapFilterEnum::Cool;
static constexpr RGBColormapFilterEnum Spring = RGBColormapFilterEnum::Spring;
static constexpr RGBColormapFilterEnum Summer = RGBColormapFilterEnum::Summer;
static constexpr RGBColormapFilterEnum Autumn = RGBColormapFilterEnum::Autumn;
static constexpr RGBColormapFilterEnum Winter = RGBColormapFilterEnum::Winter;
static constexpr RGBColormapFilterEnum Copper = RGBColormapFilterEnum::Copper;
static constexpr RGBColormapFilterEnum Jet = RGBColormapFilterEnum::Jet;
static constexpr RGBColormapFilterEnum HSV = RGBColormapFilterEnum::HSV;
static constexpr RGBColormapFilterEnum OverUnder = RGBColormapFilterEnum::OverUnder;
#endif
/** Define how to print enumerations */
extern ITKColormap_EXPORT std::ostream &
                          operator<<(std::ostream & out, const ScalarToRGBColormapImageFilterEnums::RGBColormapFilter value);
/**
 *\class ScalarToRGBColormapImageFilter
 * \brief Implements pixel-wise intensity->rgb mapping operation on one image.
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.
 *
 * The input image's scalar pixel values are mapped into a color map.
 * The color map is specified by passing the SetColormap function one of the
 * predefined maps. The following selects the "RGBColormapFilterEnum::Hot" colormap:
   \code
   RGBFilterType::Pointer colormapImageFilter = RGBFilterType::New();
   colormapImageFilter->SetColormap( RGBFilterType::Hot );
   \endcode
 *
 * You can also specify a custom color map. This is done by creating
 * a CustomColormapFunction, and then creating lists of values for
 * the red, green, and blue channel. An example of setting the red channel
 * of a colormap with only 2 colors is given below. The blue and green channels
 * should be specified in the same manner.
 *
   \code
   // Create the custom colormap
   using ColormapType = itk::Function::CustomColormapFunction<RealImageType::PixelType,
   RGBImageType::PixelType>;
   ColormapType::Pointer colormap = ColormapType::New();
   // Setup the red channel of the colormap
   ColormapType::ChannelType redChannel;
   redChannel.push_back(0); redChannel.push_back(255);
   colormap->SetRedChannel( channel );
   \endcode
 *
 * The range of values present in the input image is the range that is mapped to the entire
 * range of colors.
 *
 * This code was contributed in the Insight Journal paper:
 * "Meeting Andy Warhol Somewhere Over the Rainbow: RGB Colormapping and ITK"
 * by Tustison N., Zhang H., Lehmann G., Yushkevich P., Gee J.
 * https://www.insight-journal.org/browse/publication/285
 *
 * \sa BinaryFunctionImageFilter TernaryFunctionImageFilter
 *
 * \ingroup   IntensityImageFilters     MultiThreaded
 * \ingroup ITKColormap
 *
 * \sphinx
 * \sphinxexample{Filtering/Colormap/CreateACustomColormap, Create A Custom Colormap}
 * \sphinxexample{Filtering/Colormap/ApplyAColormapToAnImage,Apply A Colormap To An Image}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT ScalarToRGBColormapImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ScalarToRGBColormapImageFilter);

  /** Standard class type aliases. */
  using Self = ScalarToRGBColormapImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarToRGBColormapImageFilter, ImageToImageFilter);

  /** Some type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  using ColormapType = Function::ColormapFunction<InputImagePixelType, OutputImagePixelType>;

  using RGBColormapFilterEnum = ScalarToRGBColormapImageFilterEnums::RGBColormapFilter;
#if !defined(ITK_LEGACY_REMOVE)
  /** Reverse compatibility for enum values */
  using ColormapEnumType = RGBColormapFilterEnum;
#endif

#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr RGBColormapFilterEnum Red = RGBColormapFilterEnum::Red;
  static constexpr RGBColormapFilterEnum Green = RGBColormapFilterEnum::Green;
  static constexpr RGBColormapFilterEnum Blue = RGBColormapFilterEnum::Blue;
  static constexpr RGBColormapFilterEnum Grey = RGBColormapFilterEnum::Grey;
  static constexpr RGBColormapFilterEnum Hot = RGBColormapFilterEnum::Hot;
  static constexpr RGBColormapFilterEnum Cool = RGBColormapFilterEnum::Cool;
  static constexpr RGBColormapFilterEnum Spring = RGBColormapFilterEnum::Spring;
  static constexpr RGBColormapFilterEnum Summer = RGBColormapFilterEnum::Summer;
  static constexpr RGBColormapFilterEnum Autumn = RGBColormapFilterEnum::Autumn;
  static constexpr RGBColormapFilterEnum Winter = RGBColormapFilterEnum::Winter;
  static constexpr RGBColormapFilterEnum Copper = RGBColormapFilterEnum::Copper;
  static constexpr RGBColormapFilterEnum Jet = RGBColormapFilterEnum::Jet;
  static constexpr RGBColormapFilterEnum HSV = RGBColormapFilterEnum::HSV;
  static constexpr RGBColormapFilterEnum OverUnder = RGBColormapFilterEnum::OverUnder;
#endif

  /** Set/Get the colormap object. */
  itkSetObjectMacro(Colormap, ColormapType);
  itkGetModifiableObjectMacro(Colormap, ColormapType);

  void SetColormap(RGBColormapFilterEnum);

  /** Set/Get UseInputImageExtremaForScaling. If true, the colormap uses the
   * min and max values from the image to scale appropriately. Otherwise,
   * these values can be set in the colormap manually. */
  itkSetMacro(UseInputImageExtremaForScaling, bool);
  itkGetConstMacro(UseInputImageExtremaForScaling, bool);
  itkBooleanMacro(UseInputImageExtremaForScaling);

protected:
  ScalarToRGBColormapImageFilter();
  ~ScalarToRGBColormapImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Overloaded method so that if the output image is a VectorImage, then
   * the correct number of components are set. */
  void
  GenerateOutputInformation() override
  {
    Superclass::GenerateOutputInformation();
    OutputImageType * output = this->GetOutput();

    if (!output)
    {
      return;
    }
    if (output->GetNumberOfComponentsPerPixel() != 3)
    {
      output->SetNumberOfComponentsPerPixel(3);
    }
  }

  /** Perform the pixel-wise mapping.
   * ScalarToRGBColormapImageFilter can be implemented as a multithreaded
   * filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * DynamicThreadedGenerateData(). DynamicThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread".
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;

  /** Process to execute before entering the multithreaded section. */
  void
  BeforeThreadedGenerateData() override;

private:
private:
  typename ColormapType::Pointer m_Colormap;

  bool m_UseInputImageExtremaForScaling;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkScalarToRGBColormapImageFilter.hxx"
#endif

#endif
