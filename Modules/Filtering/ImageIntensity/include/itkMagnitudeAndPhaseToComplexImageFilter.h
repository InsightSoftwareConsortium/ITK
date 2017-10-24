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
#ifndef itkMagnitudeAndPhaseToComplexImageFilter_h
#define itkMagnitudeAndPhaseToComplexImageFilter_h

#include "itkBinaryFunctorImageFilter.h"

namespace itk
{
/** \class MagnitudeAndPhaseToComplexImageFilter
 *
 * \brief Implements pixel-wise conversion of magnitude and phase data into
 * complex voxels.
 *
 * This filter is parametrized over the types of the two
 * input images and the type of the output image.
 *
 * The filter expect all images to have the same dimension
 * (e.g. all 2D, or all 3D, or all ND)
 *
 * \ingroup IntensityImageFilters MultiThreaded
 *
 * \weakgroup FourierTransform
 *
 * \author Simon K. Warfield simon.warfield\@childrens.harvard.edu
 *
 * \note Attribution Notice. This research work was made possible by Grant
 * Number R01 RR021885 (PI Simon K. Warfield, Ph.D.) from the National Center
 * for Research Resources (NCRR), a component of the National Institutes of
 * Health (NIH).  Its contents are solely the responsibility of the authors
 * and do not necessarily represent the official view of NCRR or NIH.
 *
 * This class was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/326
 *
 * \sa ComposeImageFilter
 *
 * \ingroup ITKImageIntensity
 */
namespace Functor
{
template< typename TInput1, typename TInput2, typename TOutput >
class MagnitudeAndPhaseToComplex
{
public:
  MagnitudeAndPhaseToComplex() {}
  ~MagnitudeAndPhaseToComplex() {}
  bool operator!=(const MagnitudeAndPhaseToComplex &) const
  {
    return false;
  }

  bool operator==(const MagnitudeAndPhaseToComplex & other) const
  {
    return !( *this != other );
  }

  inline std::complex< TOutput > operator()(const TInput1 & A, const TInput2 & B) const
  {
    return std::complex< TOutput >( std::polar( static_cast< TOutput >( A ),  static_cast< TOutput >( B ) ) );
  }
};
}

template< typename TInputImage1,
          typename TInputImage2 = TInputImage1,
          typename TOutputImage = itk::Image< std::complex< typename TInputImage1::PixelType >,
                                           TInputImage1::ImageDimension > >
class MagnitudeAndPhaseToComplexImageFilter:
  public BinaryFunctorImageFilter<
    TInputImage1,
    TInputImage2,
    TOutputImage,
    Functor::MagnitudeAndPhaseToComplex<
      typename TInputImage1::PixelType,
      typename TInputImage2::PixelType,
      typename TOutputImage::PixelType::value_type > >

{
public:
  /** Standard class typedefs. */
  typedef MagnitudeAndPhaseToComplexImageFilter Self;

  typedef BinaryFunctorImageFilter<
    TInputImage1,
    TInputImage2,
    TOutputImage,
    Functor::MagnitudeAndPhaseToComplex<
      typename TInputImage1::PixelType,
      typename TInputImage2::PixelType,
      typename TOutputImage::PixelType::value_type > > Superclass;

  typedef typename TInputImage1::PixelType InputPixel1Type;
  typedef typename TInputImage2::PixelType InputPixel2Type;
  typedef typename TOutputImage::PixelType OutputPixelType;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MagnitudeAndPhaseToComplexImageFilter, BinaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( Input1ConvertibleToDoubleCheck,
                   ( Concept::Convertible< InputPixel1Type, double > ) );
  itkConceptMacro( Input2ConvertibleToDoubleCheck,
                   ( Concept::Convertible< InputPixel2Type, double > ) );
  itkConceptMacro( DoubleConvertibleToOutputCheck,
                   ( Concept::Convertible< double, OutputPixelType > ) );
  // End concept checking
#endif

protected:
  MagnitudeAndPhaseToComplexImageFilter() {}
  virtual ~MagnitudeAndPhaseToComplexImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MagnitudeAndPhaseToComplexImageFilter);
};
} // end namespace itk

#endif
