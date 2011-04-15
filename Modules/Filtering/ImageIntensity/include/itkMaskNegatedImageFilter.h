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
#ifndef __itkMaskNegatedImageFilter_h
#define __itkMaskNegatedImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class MaskNegatedImageFilter
 * \brief Implements an operator for pixel-wise masking of the input
 * image with the negative of a mask.
 *
 * This class is parametrized over the types of the
 * input image type, the mask image type and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The pixel type of the input 2 image must have a valid defintion of the
 * operator != with zero. This condition is required because internally this
 * filter will perform the operation
 *
 *        if pixel_from_mask_image != 0
 *             pixel_output_image = output_value
 *        else
 *             pixel_output_image = pixel_input_image
 *
 * The pixel from the input 1 is cast to the pixel type of the output image.
 *
 * Note that the input and the mask images must be of the same size.
 *
 * \warning Any pixel value other than 0 will not be masked out.
 *
 * \sa MaskImageFilter
 * \ingroup IntensityImageFilters  Multithreaded
 * \ingroup ITK-ImageIntensity
 */
namespace Functor
{
template< class TInput, class TMask, class TOutput = TInput >
class MaskNegatedInput
{
public:
  typedef typename NumericTraits< TInput >::AccumulateType AccumulatorType;

  MaskNegatedInput():m_OutsideValue(NumericTraits< TOutput >::Zero) {}
  ~MaskNegatedInput() {}
  bool operator!=(const MaskNegatedInput &) const
  {
    return false;
  }

  bool operator==(const MaskNegatedInput & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A, const TMask & B) const
  {
    if ( B != NumericTraits< TMask >::ZeroValue() )
      {
      return m_OutsideValue;
      }
    else
      {
      return static_cast< TOutput >( A );
      }
  }

  /** Method to explicitly set the outside value of the mask */
  void SetOutsideValue(const TOutput & outsideValue)
  {
    m_OutsideValue = outsideValue;
  }

  /** Method to get the outside value of the mask */
  const TOutput & GetOutsideValue() const
  {
    return m_OutsideValue;
  }

private:
  TOutput m_OutsideValue;
};
}
template< class TInputImage, class TMaskImage, class TOutputImage = TInputImage >
class ITK_EXPORT MaskNegatedImageFilter:
  public
  BinaryFunctorImageFilter< TInputImage, TMaskImage, TOutputImage,
                            Functor::MaskNegatedInput<
                              typename TInputImage::PixelType,
                              typename TMaskImage::PixelType,
                              typename TOutputImage::PixelType >   >

{
public:
  /** Standard class typedefs. */
  typedef MaskNegatedImageFilter Self;
  typedef BinaryFunctorImageFilter< TInputImage, TMaskImage, TOutputImage,
                                    Functor::MaskNegatedInput<
                                      typename TInputImage::PixelType,
                                      typename TMaskImage::PixelType,
                                      typename TOutputImage::PixelType >
                                    >                                 Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MaskNegatedImageFilter,
               BinaryFunctorImageFilter);

  /** Method to explicitly set the outside value of the mask. Defaults to 0 */
  void SetOutsideValue(const typename TOutputImage::PixelType & outsideValue)
  {
    if ( this->GetOutsideValue() != outsideValue )
      {
      this->Modified();
      this->GetFunctor().SetOutsideValue(outsideValue);
      }
  }

  const typename TOutputImage::PixelType & GetOutsideValue() const
  {
    return this->GetFunctor().GetOutsideValue();
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( MaskEqualityComparableCheck,
                   ( Concept::EqualityComparable< typename TMaskImage::PixelType > ) );
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType,
                                           typename TOutputImage::PixelType > ) );
  /** End concept checking */
#endif
protected:
  MaskNegatedImageFilter() {}
  virtual ~MaskNegatedImageFilter() {}

  void PrintSelf(std::ostream & os, Indent indent) const
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "OutsideValue: "  << this->GetOutsideValue() << std::endl;
  }

private:
  MaskNegatedImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);         //purposely not implemented
};
} // end namespace itk

#endif
