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
#ifndef itkConvertPixelBuffer_h
#define itkConvertPixelBuffer_h
#include "ITKIOImageBaseExport.h"

#include "itkObject.h"
#include "itkNumericTraits.h"
#include "itkEnableIf.h"

namespace itk
{
/**
 * \class ConvertPixelBuffer
 *  \brief Class to convert blocks of data from one type to another.
 *
 * ConvertPixelBuffer has a static method Convert().  It is used by
 * itkImageFileReader to convert from an unknown type at run-time to the
 * compile-time template pixel type in itkImageFileReader.  To work with
 * complex pixel types like RGB and RGBA a traits class is used.
 * OutputConvertTraits() is the traits class.  The default one used is
 * DefaultConvertPixelTraits.
 *
 * \ingroup ITKIOImageBase
 */
template<
  typename InputPixelType,
  typename OutputPixelType,
  typename OutputConvertTraits
  >
class ITK_TEMPLATE_EXPORT ConvertPixelBuffer
{
public:
  /** Determine the output data type. */
  typedef typename OutputConvertTraits::ComponentType OutputComponentType;
  typedef ConvertPixelBuffer                          Self;
  /** General method converts from one type to another. */
  static void Convert(InputPixelType *inputData,
                      int inputNumberOfComponents,
                      OutputPixelType *outputData, size_t size);

  static void ConvertVectorImage(InputPixelType *inputData,
                                 int inputNumberOfComponents,
                                 OutputPixelType *outputData, size_t size);

protected:
  /** Convert to Gray output. */
  /** Input values are cast to output values. */
  static void ConvertGrayToGray(InputPixelType *inputData,
                                OutputPixelType *outputData, size_t size);

  /** Weights convert from linear RGB to CIE luminance assuming a
   *  modern monitor. See Charles Poynton's Colour FAQ
   *
   *  http://www.poynton.com/ColorFAQ.html */
  static void ConvertRGBToGray(InputPixelType *inputData,
                               OutputPixelType *outputData, size_t size);

  /** Weights convert from linear RGB to CIE luminance assuming a
   *  modern monitor. Values are attentuated by the Alpha channel. See
   *  Charles Poynton's Colour FAQ
   *  http://www.poynton.com/ColorFAQ.html */
  static void ConvertRGBAToGray(InputPixelType *inputData,
                                OutputPixelType *outputData, size_t size);

  static void ConvertMultiComponentToGray(InputPixelType *inputData,
                                          int inputNumberOfComponents,
                                          OutputPixelType *outputData,
                                          size_t size);

  /** Convert to RGB output. */
  /** Each RGB output component is set the the
   * input Gray value. */
  static void ConvertGrayToRGB(InputPixelType *inputData,
                               OutputPixelType *outputData, size_t size);

  /** Input values are cast component by component to output values. */
  static void ConvertRGBToRGB(InputPixelType *inputData,
                              OutputPixelType *outputData, size_t size);

  /** Input values are attenuated by the Alpha channel. */
  static void ConvertRGBAToRGB(InputPixelType *inputData,
                               OutputPixelType *outputData, size_t size);

  /** Conversion depends upon the number of components in the
   * input. If the number of input components is 2, the output
   * components are each set to the first input component attenuated
   * by the second input component. This assumes that a two input
   * pixel represents intensity and alpha. If the number of input
   * components is not 2, the first three output components a are set
   * to the first three input components. The remaining input
   * components are ignored. */
  static void ConvertMultiComponentToRGB(InputPixelType *inputData,
                                         int inputNumberOfComponents,
                                         OutputPixelType *outputData,
                                         size_t size);

  /** Convert to RGBA output. */
  static void ConvertGrayToRGBA(InputPixelType *inputData,
                                OutputPixelType *outputData, size_t size);

  static void ConvertRGBToRGBA(InputPixelType *inputData,
                               OutputPixelType *outputData, size_t size);

  static void ConvertRGBAToRGBA(InputPixelType *inputData,
                                OutputPixelType *outputData, size_t size);

  static void ConvertMultiComponentToRGBA(InputPixelType *inputData,
                                          int inputNumberOfComponents,
                                          OutputPixelType *outputData,
                                          size_t size);

  /** Convert tensor output. */
  /** Each input is made into a 6 component symmetric pixel */
  static void ConvertTensor6ToTensor6(InputPixelType *inputData,
                                      OutputPixelType *outputData, size_t size);

  static void ConvertTensor9ToTensor6(InputPixelType *inputData,
                                      OutputPixelType *outputData, size_t size);

  /** Convertions related to complex */
  static void ConvertGrayToComplex(InputPixelType *inputData,
                                   OutputPixelType *OutputData, size_t size);

  static void ConvertComplexToComplex(InputPixelType *inputData,
                                      OutputPixelType *outputData, size_t size);

  static void ConvertMultiComponentToComplex(InputPixelType *inputData,
                                             int inputNumberOfComponents,
                                             OutputPixelType *outputData, size_t size);

  /** the most common case, where InputComponentType == unsigned
   *  char, the alpha is in the range 0..255. I presume in the
   *  mythical world of rgba<X> for all integral scalar types X, alpha
   *  will be in the range 0..X::max().  In the even more fantastical
   *  world of rgb<float> or rgb<double> alpha would have to be 1.0
   */
  template <typename UComponentType>
  static typename DisableIfC<
    NumericTraits< UComponentType >::IsInteger, UComponentType>::Type
  DefaultAlphaValue( void );

  template <typename UComponentType>
  static typename EnableIfC<
    NumericTraits< UComponentType >::IsInteger, UComponentType>::Type
  DefaultAlphaValue( void );

private:
  ConvertPixelBuffer();
  ~ConvertPixelBuffer();

};
} //namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConvertPixelBuffer.hxx"
#endif

#endif // itkConvertPixelBuffer_h
