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
#ifndef itkScalarToRGBColormapImageFilter_h
#define itkScalarToRGBColormapImageFilter_h

#include "itkImageToImageFilter.h"

#include "itkColormapFunction.h"

namespace itk
{
/** \class ScalarToRGBColormapImageFilter
 * \brief Implements pixel-wise intensity->rgb mapping operation on one image.
 *
 * This class is parameterized over the type of the input image and
 * the type of the output image.
 *
 * The input image's scalar pixel values are mapped into a color map.
 * The color map is specified by passing the SetColormap function one of the
 * predefined maps. The following selects the "Hot" colormap:
 * \code
 * RGBFilterType::Pointer colormapImageFilter = RGBFilterType::New();
 * colormapImageFilter->SetColormap( RGBFilterType::Hot );
 * \endcode
 *
 * You can also specify a custom color map. This is done by creating
 * a CustomColormapFunction, and then creating lists of values for
 * the red, green, and blue channel. An example of setting the red channel
 * of a colormap with only 2 colors is given below. The blue and green channels
 * should be specified in the same manner.
 *
 * \code
 * // Create the custom colormap
 * typedef itk::Function::CustomColormapFunction<RealImageType::PixelType,
 * RGBImageType::PixelType> ColormapType;
 * ColormapType::Pointer colormap = ColormapType::New();
 * // Setup the red channel of the colormap
 * ColormapType::ChannelType redChannel;
 * redChannel.push_back(0); redChannel.push_back(255);
 * colormap->SetRedChannel( channel );
 * \endcode
 *
 * The range of values present in the input image is the range that is mapped to the entire
 * range of colors.
 *
 * This code was contributed in the Insight Journal paper:
 * "Meeting Andy Warhol Somewhere Over the Rainbow: RGB Colormapping and ITK"
 * by Tustison N., Zhang H., Lehmann G., Yushkevich P., Gee J.
 * https://hdl.handle.net/1926/1452
 * http://www.insight-journal.org/browse/publication/285
 *
 * \sa BinaryFunctionImageFilter TernaryFunctionImageFilter
 *
 * \ingroup   IntensityImageFilters     MultiThreaded
 * \ingroup ITKColormap
 *
 * \wiki
 * \wikiexample{SimpleOperations/ScalarToRGBColormapImageFilter,Apply a color map to an image}
 * \endwiki
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ScalarToRGBColormapImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ScalarToRGBColormapImageFilter                  Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarToRGBColormapImageFilter, ImageToImageFilter);

  /** Some typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::ConstPointer InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef TOutputImage                          OutputImageType;
  typedef typename OutputImageType::Pointer     OutputImagePointer;
  typedef typename OutputImageType::RegionType  OutputImageRegionType;
  typedef typename OutputImageType::PixelType   OutputImagePixelType;

  typedef Function::ColormapFunction< InputImagePixelType,
                                    OutputImagePixelType >                                ColormapType;

  /** Set/Get the colormap object. */
  itkSetObjectMacro(Colormap, ColormapType);
  itkGetModifiableObjectMacro(Colormap, ColormapType);

  /** Enum type that provides for an easy interface to existing colormaps. */
  typedef enum { Red, Green, Blue, Grey, Hot, Cool, Spring, Summer,
                 Autumn, Winter, Copper, Jet, HSV, OverUnder } ColormapEnumType;

  void SetColormap(ColormapEnumType);

  /** Set/Get UseInputImageExtremaForScaling. If true, the colormap uses the
   * min and max values from the image to scale appropriately. Otherwise,
   * these values can be set in the colormap manually. */
  itkSetMacro(UseInputImageExtremaForScaling, bool);
  itkGetConstMacro(UseInputImageExtremaForScaling, bool);
  itkBooleanMacro(UseInputImageExtremaForScaling);

protected:
  ScalarToRGBColormapImageFilter();
  virtual ~ScalarToRGBColormapImageFilter() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Overloaded method so that if the output image is a VectorImage, then
   * the correct number of components are set. */
  virtual void GenerateOutputInformation() ITK_OVERRIDE
  {
    Superclass::GenerateOutputInformation();
    OutputImageType* output = this->GetOutput();

    if ( !output )
      {
      return;
      }
    if ( output->GetNumberOfComponentsPerPixel() != 3 )
      {
      output->SetNumberOfComponentsPerPixel( 3 );
      }
  }

  /** Perform the pixel-wise mapping.
   * ScalarToRGBColormapImageFilter can be implemented as a multithreaded
   * filter.
   * Therefore, this implementation provides a ThreadedGenerateData() routine
   * which is called for each processing thread. The output image data is
   * allocated automatically by the superclass prior to calling
   * ThreadedGenerateData(). ThreadedGenerateData can only write to the
   * portion of the output image specified by the parameter
   * "outputRegionForThread".
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                            ThreadIdType threadId) ITK_OVERRIDE;

  /** Process to execute before entering the multithreaded section. */
  void BeforeThreadedGenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarToRGBColormapImageFilter);

private:
  typename ColormapType::Pointer m_Colormap;

  bool m_UseInputImageExtremaForScaling;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarToRGBColormapImageFilter.hxx"
#endif

#endif
