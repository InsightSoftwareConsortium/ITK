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
#ifndef itkVectorMagnitudeImageFilter_h
#define itkVectorMagnitudeImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
/** \class VectorMagnitudeImageFilter
 *
 * \brief Take an image of vectors as input and produce an image with the
 *  magnitude of those vectors.
 *
 * The filter expects the input image pixel type to be a vector and
 * the output image pixel type to be a scalar.
 *
 * This filter assumes that the PixelType of the input image is a VectorType
 * that provides a GetNorm() method.
 *
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \wiki
 * \wikiexample{VectorImages/VectorMagnitudeImageFilter,Compute the magnitude of each pixel in a vector image to produce a magnitude image}
 * \endwiki
 */

namespace Functor
{
template< typename TInput, typename TOutput >
class VectorMagnitude
{
public:
  VectorMagnitude() {}
  ~VectorMagnitude() {}

  bool operator!=(const VectorMagnitude &) const
  {
    return false;
  }

  bool operator==(const VectorMagnitude & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    return static_cast< TOutput >( A.GetNorm() );
  }
};
}

template< typename TInputImage, typename TOutputImage >
class VectorMagnitudeImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::VectorMagnitude< typename TInputImage::PixelType,
                                                     typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef VectorMagnitudeImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::VectorMagnitude< typename TInputImage::PixelType,
                              typename TOutputImage::PixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(VectorMagnitudeImageFilter,
               UnaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TInputImage::PixelType::ValueType > ) );
  // End concept checking
#endif

protected:
  VectorMagnitudeImageFilter() {}
  virtual ~VectorMagnitudeImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorMagnitudeImageFilter);
};
} // end namespace itk

#endif
