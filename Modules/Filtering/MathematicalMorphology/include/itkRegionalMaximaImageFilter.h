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
#ifndef itkRegionalMaximaImageFilter_h
#define itkRegionalMaximaImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class RegionalMaximaImageFilter
 * \brief Produce a binary image where foreground is the regional maxima of the
 * input image.
 *
 * Regional maxima are flat zones surrounded by pixels of lower value.
 *
 * If the input image is constant, the entire image can be considered as a
 * maxima or not.  The desired behavior can be selected with the
 * SetFlatIsMaxima() method.
 *
 * \author Gaetan Lehmann
 *
 * This class was contributed to the Insight Journal by author Gaetan Lehmann.
 * Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas,
 * France. The paper can be found at
 * https://www.insight-journal.org/browse/publication/65
 *
 * \sa ValuedRegionalMaximaImageFilter
 * \sa HConvexImageFilter
 * \sa RegionalMinimaImageFilter
 *
 * \ingroup MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 *
 * \sphinx
 * \sphinxexample{Filtering/MathematicalMorphology/RegionalMaximal,Regional Maximal}
 * \endsphinx
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT RegionalMaximaImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(RegionalMaximaImageFilter);

  /** Standard class type aliases. */
  using Self = RegionalMaximaImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(RegionalMaximaImageFilter, ImageToImageFilter);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

  /**
   * Set/Get the value in the output image to consider as "foreground".
   * Defaults to maximum value of PixelType.
   */
  itkSetMacro(ForegroundValue, OutputImagePixelType);
  itkGetConstMacro(ForegroundValue, OutputImagePixelType);

  /**
   * Set/Get the value used as "background" in the output image.
   * Defaults to NumericTraits<PixelType>::NonpositiveMin().
   */
  itkSetMacro(BackgroundValue, OutputImagePixelType);
  itkGetConstMacro(BackgroundValue, OutputImagePixelType);

  /**
   * Set/Get whether a flat image must be considered as a maxima or not.
   * Defaults to true.
   */
  itkSetMacro(FlatIsMaxima, bool);
  itkGetConstMacro(FlatIsMaxima, bool);
  itkBooleanMacro(FlatIsMaxima);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputHasPixelTraitsCheck, (Concept::HasPixelTraits<InputImagePixelType>));
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<InputImagePixelType>));
  // End concept checking
#endif

protected:
  RegionalMaximaImageFilter();
  ~RegionalMaximaImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** RegionalMaximaImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** RegionalMaximaImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void
  GenerateData() override;

private:
  bool                 m_FullyConnected{ false };
  bool                 m_FlatIsMaxima{ true };
  OutputImagePixelType m_ForegroundValue;
  OutputImagePixelType m_BackgroundValue;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRegionalMaximaImageFilter.hxx"
#endif

#endif
