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
#ifndef itkTensorFractionalAnisotropyImageFilter_h
#define itkTensorFractionalAnisotropyImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkImage.h"

namespace itk
{
// This functor class invokes the computation of fractional anisotropy from
// every pixel.
namespace Functor
{
template< typename TInput >
class TensorFractionalAnisotropyFunction
{
public:
  typedef typename TInput::RealValueType RealValueType;
  TensorFractionalAnisotropyFunction() {}
  ~TensorFractionalAnisotropyFunction() {}
  bool operator!=(const TensorFractionalAnisotropyFunction &) const
  {
    return false;
  }

  bool operator==(const TensorFractionalAnisotropyFunction & other) const
  {
    return !( *this != other );
  }

  inline RealValueType operator()(const TInput & x) const
  {
    return x.GetFractionalAnisotropy();
  }
};
}  // end namespace functor

/** \class TensorFractionalAnisotropyImageFilter
 * \brief Computes the Fractional Anisotropy for every pixel of a input tensor image.
 *
 * TensorFractionalAnisotropyImageFilter applies pixel-wise the invokation for
 * computing the fractional anisotropy of every pixel. The pixel type of the
 * input image is expected to implement a method GetFractionalAnisotropy(), and
 * to specify its return type as  RealValueType.
 *
 * \sa TensorRelativeAnisotropyImageFilter
 * \sa DiffusionTensor3D
 *
 * \ingroup IntensityImageFilters  MultiThreaded  TensorObjects
 *
 * \ingroup ITKDiffusionTensorImage
 */
template< typename  TInputImage,
  typename  TOutputImage = Image<
    typename NumericTraits< typename TInputImage::PixelType::ValueType >::RealType,
    TInputImage::Dimension > >
class TensorFractionalAnisotropyImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::TensorFractionalAnisotropyFunction<
                             typename TInputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef TensorFractionalAnisotropyImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::TensorFractionalAnisotropyFunction<
      typename TInputImage::PixelType > >         Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename TOutputImage::PixelType     OutputPixelType;
  typedef typename TInputImage::PixelType      InputPixelType;
  typedef typename InputPixelType::ValueType   InputValueType;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(TensorFractionalAnisotropyImageFilter, UnaryFunctorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Print internal ivars */
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  { this->Superclass::PrintSelf(os, indent); }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputValueType > ) );
  // End concept checking
#endif

protected:
  TensorFractionalAnisotropyImageFilter() {}
  virtual ~TensorFractionalAnisotropyImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TensorFractionalAnisotropyImageFilter);
};
} // end namespace itk

#endif
