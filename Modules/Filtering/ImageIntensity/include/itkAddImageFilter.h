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
#ifndef itkAddImageFilter_h
#define itkAddImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkArithmeticOpsFunctors.h"
#include "itkNumericTraits.h"

namespace itk
{

/** \class AddImageFilter
 * \brief Pixel-wise addition of two images.
 *
 * This class is templated over the types of the two
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The pixel type of the input 1 image must have a valid definition of
 * the operator+ with a pixel type of the image 2. This condition is
 * required because internally this filter will perform the operation
 *
 * \code
 *        pixel_from_image_1 + pixel_from_image_2
 * \endcode
 *
 * Additionally the type resulting from the sum, will be cast to
 * the pixel type of the output image.
 *
 * The total operation over one pixel will be
 * \code
 * output_pixel = static_cast<OutputPixelType>( input1_pixel + input2_pixel )
 * \endcode
 *
 * For example, this filter could be used directly for adding images whose
 * pixels are vectors of the same dimension, and to store the resulting vector
 * in an output image of vector pixels.
 *
 * The images to be added are set using the methods:
 * \code
 * SetInput1( image1 );
 * SetInput2( image2 );
 * \endcode
 *
 * Additionally, this filter can be used to add a constant to every pixel of an
 * image by using
 * \code
 * SetInput1( image1 );
 * SetConstant2( constant );
 * \endcode
 *
 * \warning No numeric overflow checking is performed in this filter.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \wiki
 * \wikiexample{ImageProcessing/AddImageFilter,Add two images together}
 * \wikiexample{ImageProcessing/AddConstantToImageFilter,Add a constant to every pixel in an image}
 * \endwiki
 */
template< typename TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1 >
class ITK_TEMPLATE_EXPORT AddImageFilter:
  public
  BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage,
                            Functor::Add2<
                              typename TInputImage1::PixelType,
                              typename TInputImage2::PixelType,
                              typename TOutputImage::PixelType >   >

{
public:
  /** Standard class typedefs. */
  typedef AddImageFilter Self;
  typedef BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage,
                                    Functor::Add2<
                                      typename TInputImage1::PixelType,
                                      typename TInputImage2::PixelType,
                                      typename TOutputImage::PixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AddImageFilter,
               BinaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( Input1Input2OutputAdditiveOperatorsCheck,
                   ( Concept::AdditiveOperators< typename TInputImage1::PixelType,
                                                 typename TInputImage2::PixelType,
                                                 typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  AddImageFilter() {}
  virtual ~AddImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(AddImageFilter);
};
} // end namespace itk

#endif
