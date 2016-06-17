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
#ifndef itkCompose3DCovariantVectorImageFilter_h
#define itkCompose3DCovariantVectorImageFilter_h
#if !defined( ITK_LEGACY_REMOVE )

#include "itkTernaryFunctorImageFilter.h"
#include "itkCovariantVector.h"

namespace itk
{
namespace Functor
{
template< typename TInput >
class Compose3DCovariantVector
{
public:
  typedef CovariantVector< TInput, 3 > OutputType;
  Compose3DCovariantVector() {}
  ~Compose3DCovariantVector() {}
  bool operator!=(const Compose3DCovariantVector &) const
  {
    return false;
  }

  bool operator==(const Compose3DCovariantVector & other) const
  {
    return !( *this != other );
  }

  inline OutputType operator()(const TInput & s1,
                               const TInput & s2,
                               const TInput & s3) const
  {
    OutputType v;

    v[0] = s1;
    v[1] = s2;
    v[2] = s3;
    return v;
  }
};
}

/** \class Compose3DCovariantVectorImageFilter
 * \brief Implements pixel-wise composition of an 3D covariant vector pixel from three scalar images.
 *
 * This filter receives three scalar images as input. Each image
 * containing one of the 3D covariant vector components. The filter produces as
 * output a 3D covariant vector image in which the three components have been
 * unified. The Component type is preserved from the PixelType of the
 * input images.
 *
 * \deprecated
 * \ingroup ITKDeprecated
 *
 * \wiki
 * \wikiexample{ImageProcessing/Compose3DCovariantVectorImageFilter,Compose a vector image (with 3 components) from three scalar images}
 * \endwiki
 */

template< typename TInputImage,
          typename TOutputImage =
            Image< CovariantVector< typename TInputImage::PixelType, 3 >,
                   TInputImage::ImageDimension > >
class Compose3DCovariantVectorImageFilter:
  public
  TernaryFunctorImageFilter< TInputImage, TInputImage,
                             TInputImage, TOutputImage,
                             Functor::Compose3DCovariantVector< typename TInputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef Compose3DCovariantVectorImageFilter Self;
  typedef TernaryFunctorImageFilter<
    TInputImage, TInputImage, TInputImage,
    TOutputImage,
    Functor::Compose3DCovariantVector<
      typename TInputImage::PixelType > > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename Superclass::OutputImageType OutputImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(Compose3DCovariantVectorImageFilter,
               TernaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TInputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  Compose3DCovariantVectorImageFilter() {}
  virtual ~Compose3DCovariantVectorImageFilter() {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(Compose3DCovariantVectorImageFilter);
};
} // end namespace itk

#endif //#if !defined( ITK_LEGACY_REMOVE )
#endif
