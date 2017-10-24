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
#ifndef itkTensorRelativeAnisotropyImageFilter_h
#define itkTensorRelativeAnisotropyImageFilter_h

#include "itkUnaryFunctorImageFilter.h"
#include "itkImage.h"

namespace itk
{
// This functor class invokes the computation of relative anisotropy from
// every pixel.
namespace Functor
{
template< typename TInput >
class TensorRelativeAnisotropyFunction
{
public:
  typedef typename TInput::RealValueType RealValueType;
  TensorRelativeAnisotropyFunction() {}
  ~TensorRelativeAnisotropyFunction() {}
  bool operator!=(const TensorRelativeAnisotropyFunction &) const
  {
    return false;
  }

  bool operator==(const TensorRelativeAnisotropyFunction & other) const
  {
    return !( *this != other );
  }

  inline RealValueType operator()(const TInput & x) const
  {
    return x.GetRelativeAnisotropy();
  }
};
}  // end namespace functor

/** \class TensorRelativeAnisotropyImageFilter
 * \brief Computes the Relative Anisotropy for every pixel of a input tensor image.
 *
 * TensorRelativeAnisotropyImageFilter applies pixel-wise the invokation for
 * computing the relative anisotropy of every pixel. The pixel type of the
 * input image is expected to implement a method GetRelativeAnisotropy(), and
 * to specify its return type as  RealValueType.
 *
 * \sa TensorFractionalAnisotropyImageFilter
 * \sa DiffusionTensor3D
 *
 * \ingroup IntensityImageFilters  MultiThreaded  TensorObjects
 *
 * \ingroup ITKDiffusionTensorImage
 */
template< typename  TInputImage, typename  TOutputImage = Image<
    typename NumericTraits< typename TInputImage::PixelType::ValueType >::RealType,
    TInputImage::Dimension > >
class TensorRelativeAnisotropyImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::TensorRelativeAnisotropyFunction<
                             typename TInputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef TensorRelativeAnisotropyImageFilter Self;
  typedef UnaryFunctorImageFilter<
    TInputImage, TOutputImage,
    Functor::TensorRelativeAnisotropyFunction<
      typename TInputImage::PixelType > >       Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename TOutputImage::PixelType     OutputPixelType;
  typedef typename TInputImage::PixelType      InputPixelType;
  typedef typename InputPixelType::ValueType   InputValueType;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(TensorRelativeAnisotropyImageFilter, UnaryFunctorImageFilter);

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
  TensorRelativeAnisotropyImageFilter() {}
  virtual ~TensorRelativeAnisotropyImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(TensorRelativeAnisotropyImageFilter);
};
} // end namespace itk

#endif
