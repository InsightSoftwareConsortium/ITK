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
#ifndef itkAcosImageFilter_h
#define itkAcosImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkMath.h"

namespace itk
{
namespace Functor
{
/**
 * \class Acos
 * \brief Computes the Acos of a pixel.
 * \ingroup ITKImageIntensity
 */
template< typename TInput, typename TOutput >
class Acos
{
public:
  Acos() {}
  ~Acos() {}
  bool operator!=(const Acos &) const
  {
    return false;
  }

  bool operator==(const Acos & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    return static_cast< TOutput >( std::acos( static_cast< double >( A ) ) );
  }
};
}

/** \class AcosImageFilter
 * \brief Computes the inverse cosine of each pixel.
 *
 * This filter is templated over the pixel type of the input image
 * and the pixel type of the output image.
 *
 * The filter walks over all the pixels in the input image, and for
 * each pixel does do the following:
 *
 * \li cast the pixel value to \c double,
 * \li apply the \c std::acos() function to the \c double value
 * \li cast the \c double value resulting from \c std::acos() to the pixel type
 *     of the output image
 * \li store the casted value into the output image.
 *
 * The filter expects both images to have the same dimension (e.g. both 2D,
 * or both 3D, or both ND).
 *
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 */
template< typename TInputImage, typename TOutputImage >
class AcosImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::Acos<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef AcosImageFilter Self;
  typedef UnaryFunctorImageFilter< TInputImage, TOutputImage,
                                   Functor::Acos< typename TInputImage::PixelType,
                                                  typename TOutputImage::PixelType > >  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AcosImageFilter,
               UnaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputCovertibleToDoubleCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType, double > ) );
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  AcosImageFilter() {}
  virtual ~AcosImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(AcosImageFilter);
};
} // end namespace itk

#endif
