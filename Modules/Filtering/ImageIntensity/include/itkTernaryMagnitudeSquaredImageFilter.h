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
#ifndef itkTernaryMagnitudeSquaredImageFilter_h
#define itkTernaryMagnitudeSquaredImageFilter_h

#include "itkTernaryFunctorImageFilter.h"

namespace itk
{
namespace Functor
{
/**
 * \class ModulusSquare3
 * \brief
 * \ingroup ITKImageIntensity
 */
template< typename TInput1, typename TInput2, typename TInput3, typename TOutput >
class ModulusSquare3
{
public:
  ModulusSquare3() {}
  ~ModulusSquare3() {}
  bool operator!=(const ModulusSquare3 &) const
  {
    return false;
  }

  bool operator==(const ModulusSquare3 & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput1 & A,
                            const TInput2 & B,
                            const TInput3 & C) const
  { return static_cast<TOutput>( A * A + B * B + C * C ); }
};
}
/** \class TernaryMagnitudeSquaredImageFilter
 * \brief Compute the pixel-wise squared magnitude of three images.
 *
 * This class is templated over the types of the three
 * input images and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * \ingroup IntensityImageFilters
 * \ingroup ITKImageIntensity
 */
template< typename TInputImage1, typename TInputImage2,
          typename TInputImage3, typename TOutputImage >
class TernaryMagnitudeSquaredImageFilter:
  public
  TernaryFunctorImageFilter< TInputImage1, TInputImage2,
                             TInputImage3, TOutputImage,
                             Functor::ModulusSquare3<
                               typename TInputImage1::PixelType,
                               typename TInputImage2::PixelType,
                               typename TInputImage3::PixelType,
                               typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef TernaryMagnitudeSquaredImageFilter Self;
  typedef TernaryFunctorImageFilter<
    TInputImage1, TInputImage2,
    TInputImage3, TOutputImage,
    Functor::ModulusSquare3<
      typename TInputImage1::PixelType,
      typename TInputImage2::PixelType,
      typename TInputImage3::PixelType,
      typename TOutputImage::PixelType >   >  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(TernaryMagnitudeSquaredImageFilter,
               TernaryFunctorImageFilter);

protected:
  TernaryMagnitudeSquaredImageFilter() {}
  virtual ~TernaryMagnitudeSquaredImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TernaryMagnitudeSquaredImageFilter);
};
} // end namespace itk

#endif
