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
#ifndef itkMaskImageFilter_h
#define itkMaskImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkNumericTraits.h"
#include "itkVariableLengthVector.h"
#include "itkMath.h"

namespace itk
{
namespace Functor
{
/**
 * \class MaskInput
 * \brief
 * \ingroup ITKImageIntensity
 */
template< typename TInput, typename TMask, typename TOutput = TInput >
class MaskInput
{
public:
  typedef typename NumericTraits< TInput >::AccumulateType AccumulatorType;

  MaskInput()
  {
    m_MaskingValue = NumericTraits< TMask >::ZeroValue();
    InitializeOutsideValue( static_cast<TOutput*>( ITK_NULLPTR ) );
  }
  ~MaskInput() {}
  bool operator!=(const MaskInput &) const
  {
    return false;
  }

  bool operator==(const MaskInput & other) const
  {
    return !( *this != other );
  }

  inline TOutput operator()(const TInput & A, const TMask & B) const
  {
    if ( B != m_MaskingValue )
      {
      return static_cast< TOutput >( A );
      }
    else
      {
      return m_OutsideValue;
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

  /** Method to explicitly set the masking value */
  void SetMaskingValue(const TMask & maskingValue)
  {
    m_MaskingValue = maskingValue;
  }
  /** Method to get the masking value */
  const TMask & GetMaskingValue() const
  {
    return m_MaskingValue;
  }

private:

  template < typename TPixelType >
  void InitializeOutsideValue( TPixelType * )
  {
    this->m_OutsideValue = NumericTraits< TPixelType >::ZeroValue();
  }

  template < typename TValue >
  void InitializeOutsideValue( VariableLengthVector<TValue> * )
  {
    // set the outside value to be of zero length
    this->m_OutsideValue = VariableLengthVector< TValue >(0);
  }

  TOutput m_OutsideValue;
  TMask   m_MaskingValue;
};
}
/** \class MaskImageFilter
 * \brief Mask an image with a mask.
 *
 * This class is templated over the types of the
 * input image type, the mask image type and the type of the output image.
 * Numeric conversions (castings) are done by the C++ defaults.
 *
 * The pixel type of the input 2 image must have a valid definition of the
 * operator != with zero. This condition is required because internally this
 * filter will perform the operation
 *
 * \code
 *        if pixel_from_mask_image != masking_value
 *             pixel_output_image = pixel_input_image
 *        else
 *             pixel_output_image = outside_value
 * \endcode
 *
 * The pixel from the input 1 is cast to the pixel type of the output image.
 *
 * Note that the input and the mask images must be of the same size.
 *
 * \warning Any pixel value other than masking value (0 by default) will not be masked out.
 *
 * \sa MaskNegatedImageFilter
 * \ingroup IntensityImageFilters
 * \ingroup MultiThreaded
 * \ingroup ITKImageIntensity
 *
 * \wiki
 * \wikiexample{ImageProcessing/MaskImageFilter,Apply a mask to an image}
 * \endwiki
 */
template< typename TInputImage, typename TMaskImage, typename TOutputImage = TInputImage >
class MaskImageFilter:
  public
  BinaryFunctorImageFilter< TInputImage, TMaskImage, TOutputImage,
                            Functor::MaskInput<
                              typename TInputImage::PixelType,
                              typename TMaskImage::PixelType,
                              typename TOutputImage::PixelType >   >

{
public:
  /** Standard class typedefs. */
  typedef MaskImageFilter Self;
  typedef BinaryFunctorImageFilter< TInputImage, TMaskImage, TOutputImage,
                                    Functor::MaskInput<
                                      typename TInputImage::PixelType,
                                      typename TMaskImage::PixelType,
                                      typename TOutputImage::PixelType >
                                    >                                 Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MaskImageFilter,
               BinaryFunctorImageFilter);

  /** Typedefs **/
  typedef TMaskImage MaskImageType;

  /** Set/Get the mask image. Pixels set in the mask image will retain
   *  the original value of the input image while pixels not set in
   *  the mask will be set to the "OutsideValue".
   */
  void SetMaskImage(const MaskImageType *maskImage)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput( 1, const_cast< MaskImageType * >( maskImage ) );
  }
  const MaskImageType * GetMaskImage()
  {
    return static_cast<const MaskImageType*>(this->ProcessObject::GetInput(1));
  }

  /** Method to explicitly set the outside value of the mask. Defaults to 0 */
  void SetOutsideValue(const typename TOutputImage::PixelType & outsideValue)
  {
    if ( Math::NotExactlyEquals(this->GetOutsideValue(), outsideValue) )
      {
      this->Modified();
      this->GetFunctor().SetOutsideValue(outsideValue);
      }
  }

  const typename TOutputImage::PixelType & GetOutsideValue() const
  {
    return this->GetFunctor().GetOutsideValue();
  }

  /** Method to explicitly set the masking value of the mask. Defaults to 0 */
  void SetMaskingValue(const typename TMaskImage::PixelType & maskingValue)
  {
    if ( this->GetMaskingValue() != maskingValue )
      {
      this->Modified();
      this->GetFunctor().SetMaskingValue(maskingValue);
      }
  }

  /** Method to get the masking value of the mask. */
  const typename TMaskImage::PixelType & GetMaskingValue() const
  {
    return this->GetFunctor().GetMaskingValue();
  }

  void BeforeThreadedGenerateData() ITK_OVERRIDE
  {
    typedef typename TOutputImage::PixelType PixelType;
    this->CheckOutsideValue( static_cast<PixelType*>(ITK_NULLPTR) );
  }

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( MaskEqualityComparableCheck,
                   ( Concept::EqualityComparable< typename TMaskImage::PixelType > ) );
  itkConceptMacro( InputConvertibleToOutputCheck,
                   ( Concept::Convertible< typename TInputImage::PixelType,
                                           typename TOutputImage::PixelType > ) );
  // End concept checking
#endif

protected:
  MaskImageFilter() {}
  virtual ~MaskImageFilter() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Superclass::PrintSelf(os, indent);
    os << indent << "OutsideValue: "  << this->GetOutsideValue() << std::endl;
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MaskImageFilter);

  template < typename TPixelType >
  void CheckOutsideValue( const TPixelType * ) {}

  template < typename TValue >
  void CheckOutsideValue( const VariableLengthVector< TValue > * )
  {
    // Check to see if the outside value contains only zeros. If so,
    // resize it to have the same number of zeros as the output
    // image. Otherwise, check that the number of components in the
    // outside value is the same as the number of components in the
    // output image. If not, throw an exception.
    VariableLengthVector< TValue > currentValue =
      this->GetFunctor().GetOutsideValue();
    VariableLengthVector< TValue > zeroVector( currentValue.GetSize() );
    zeroVector.Fill( NumericTraits< TValue >::ZeroValue() );

    if ( currentValue == zeroVector )
      {
      zeroVector.SetSize( this->GetOutput()->GetVectorLength() );
      zeroVector.Fill( NumericTraits< TValue >::ZeroValue() );
      this->GetFunctor().SetOutsideValue( zeroVector );
      }
    else if ( this->GetFunctor().GetOutsideValue().GetSize() !=
              this->GetOutput()->GetVectorLength() )
      {
      itkExceptionMacro(
        << "Number of components in OutsideValue: "
        <<  this->GetFunctor().GetOutsideValue().GetSize()
        << " is not the same as the "
        << "number of components in the image: "
        << this->GetOutput()->GetVectorLength());
      }
  }

};
} // end namespace itk

#endif
