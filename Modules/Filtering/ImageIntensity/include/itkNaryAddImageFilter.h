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
#ifndef itkNaryAddImageFilter_h
#define itkNaryAddImageFilter_h

#include "itkNaryFunctorImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
namespace Functor
{
/**
 * \class Add1
 * \brief
 * \ingroup ITKImageIntensity
 */
template< typename TInput, typename TOutput >
class Add1
{
public:
  typedef typename NumericTraits< TInput >::AccumulateType AccumulatorType;
  Add1() {}
  ~Add1() {}
  inline TOutput operator()(const std::vector< TInput > & B) const
  {
    AccumulatorType sum = NumericTraits< TOutput >::ZeroValue();

    for ( unsigned int i = 0; i < B.size(); i++ )
      {
      sum += static_cast< AccumulatorType >( B[i] );
      }
    return static_cast< TOutput >( sum );
  }

  bool operator==(const Add1 &) const
  {
    return true;
  }

  bool operator!=(const Add1 &) const
  {
    return false;
  }
};
}
/** \class NaryAddImageFilter
 * \brief Pixel-wise addition of N images.
 *
 * This class is templated over the types of the
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The pixel type of the input images must have a valid definition of
 * the operator+ with each other. This condition is
 * required because internally this filter will perform the operation
 *
 * \code
 *        pixel_from_image_N + pixel_from_image_(N+1)
 * \endcode
 *
 * Additionally the type resulting from the sum, will be cast to
 * the pixel type of the output image.
 *
 * The total operation over one pixel will be
 *
 * \code
 *  output_pixel = static_cast<OutputPixelType>( input_pixel_N + input_pixel_(N+1) )
 * \endcode
 *
 * For example, this filter could be used directly for adding images whose
 * pixels are vectors of the same dimension, and to store the resulting vector
 * in an output image of vector pixels.
 *
 * \warning No numeric overflow checking is performed in this filter.
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 */
template< typename TInputImage, typename TOutputImage >
class NaryAddImageFilter:
  public
  NaryFunctorImageFilter< TInputImage, TOutputImage,
                          Functor::Add1< typename TInputImage::PixelType,  typename TInputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef NaryAddImageFilter Self;
  typedef NaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::Add1< typename TInputImage::PixelType,
                   typename TInputImage::PixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(NaryAddImageFilter,
               NaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType,
                                           typename TOutputImage::PixelType > ) );
  itkConceptMacro( InputHasZeroCheck,
                   ( Concept::HasZero< typename TInputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  NaryAddImageFilter() {}
  virtual ~NaryAddImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(NaryAddImageFilter);
};
} // end namespace itk

#endif
