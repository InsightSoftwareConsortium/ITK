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
#ifndef itkSubtractImageFilter_h
#define itkSubtractImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkArithmeticOpsFunctors.h"

namespace itk
{
/** \class SubtractImageFilter
 * \brief Pixel-wise subtraction of two images.
 *
 * Subtract each pixel from image2 from its corresponding pixel in
 * image1:
 *
 * \code
 * Output = Input1 - Input2.
 * \endcode
 *
 * This is done using
 *
 * \code
 * SetInput1( image1 );
 * SetInput2( image2 );
 * \endcode
 *
 * This class is templated over the types of the two
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * Additionally, a constant can be subtracted from every pixel in an image using:
 *
 * \code
 * SetInput1( image1 );
 * SetConstant2( constant );
 * \endcode
 *
 * \note The result of AddImageFilter with a negative constant is not
 * necessarily the same as SubtractImageFilter. This would be the case when
 * the PixelType defines an operator-() that is not the inverse of operator+()
 *
 * \ingroup IntensityImageFilters MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \wiki
 * \wikiexample{ImageProcessing/SubtractImageFilter,Subtract two images}
 * \wikiexample{ImageProcessing/SubtractConstantFromImageFilter,Subtract a constant from every pixel in an image}
 * \endwiki
 */
template< typename TInputImage1, typename TInputImage2 = TInputImage1, typename TOutputImage = TInputImage1 >
class SubtractImageFilter:
  public
  BinaryFunctorImageFilter< TInputImage1, TInputImage2, TOutputImage,
                            Functor::Sub2<
                              typename TInputImage1::PixelType,
                              typename TInputImage2::PixelType,
                              typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef SubtractImageFilter Self;
  typedef BinaryFunctorImageFilter<
    TInputImage1, TInputImage2, TOutputImage,
    Functor::Sub2< typename TInputImage1::PixelType,
                   typename TInputImage2::PixelType,
                   typename TOutputImage::PixelType > >  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(SubtractImageFilter,
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
  SubtractImageFilter() {}
  virtual ~SubtractImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SubtractImageFilter);
};
} // end namespace itk

#endif
