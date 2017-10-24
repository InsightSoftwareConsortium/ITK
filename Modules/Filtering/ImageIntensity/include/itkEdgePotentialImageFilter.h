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
#ifndef itkEdgePotentialImageFilter_h
#define itkEdgePotentialImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
/** \class EdgePotentialImageFilter
 *
 * \brief Computes the edge potential of an image from the image gradient.
 *
 * Input to this filter should be a CovariantVector image representing
 * the image gradient.
 *
 * The filter expect both the input and output images to have the same
 * number of dimensions, and the output to be of a scalar image type.
 *
 * \ingroup ITKImageIntensity
 */
namespace Functor
{
template< typename TInput, typename TOutput >
class EdgePotential
{
public:
  EdgePotential() {}
  ~EdgePotential() {}
  bool operator!=(const EdgePotential &) const
  {
    return false;
  }

  bool operator==(const EdgePotential & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A) const
  {
    return static_cast< TOutput >( std::exp( -1.0 * A.GetNorm() ) );
  }
};
}

template< typename TInputImage, typename TOutputImage >
class EdgePotentialImageFilter:
  public
  UnaryFunctorImageFilter< TInputImage, TOutputImage,
                           Functor::EdgePotential<
                             typename TInputImage::PixelType,
                             typename TOutputImage::PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef EdgePotentialImageFilter Self;
  typedef UnaryFunctorImageFilter< TInputImage, TOutputImage,
                                   Functor::EdgePotential<
                                     typename TInputImage::PixelType,
                                     typename TOutputImage::PixelType >
                                   >                                 Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(EdgePotentialImageFilter,
               UnaryFunctorImageFilter);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< typename TInputImage::PixelType::ValueType > ) );
  // End concept checking
#endif

protected:
  EdgePotentialImageFilter() {}
  virtual ~EdgePotentialImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(EdgePotentialImageFilter);
};
} // end namespace itk

#endif
