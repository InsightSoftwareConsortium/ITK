/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaskImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMaskImageFilter_h
#define __itkMaskImageFilter_h

#include "itkBinaryFunctorImageFilter.h"
#include "itkNumericTraits.h"


namespace itk
{
  
/** \class MaskImageFilter
 * \brief Implements an operator for pixel-wise masking of the input 
 * image with the mask.
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
 *             pixel_output_image = pixel_input_image
 *        else
 *             pixel_output_image = 0
 *
 * The pixel from the input 1 is cast to the pixel type of the output image.
 *
 * Note that the input and the mask images must be of the same size.
 *
 * \warning Any pixel value other than 0 will not be masked out. 
 *
 * \sa MaskNegatedImageFilter
 * \ingroup IntensityImageFilters  Multithreaded
 */
namespace Functor {  
  
template< class TInput, class TMask, class TOutput >
class MaskInput
{
public:
  typedef typename NumericTraits< TInput >::AccumulateType AccumulatorType;

  MaskInput(): m_OutsideValue(NumericTraits< TOutput >::ZeroValue()) {};
  ~MaskInput() {};
  bool operator!=( const MaskInput & ) const
  {
    return false;
  }
  bool operator==( const MaskInput & other ) const
  {
    return !(*this != other);
  }

  inline TOutput operator()( const TInput & A, const TMask & B)
  {
    if (B != NumericTraits< TMask >::ZeroValue() ) 
      {
      return static_cast<TOutput>( A );
      }
    else
      {
      return m_OutsideValue;
      }
  }

  /** Method to explicitly set the outside value of the mask */
  void SetOutsideValue( const TOutput &outsideValue )
    {
    m_OutsideValue = outsideValue;
    }
  
  /** Method to get the outside value of the mask */
  const TOutput &GetOutsideValue() const
    {
    return m_OutsideValue;
    }
     
private:
  TOutput m_OutsideValue;
}; 

}
template <class TInputImage, class TMaskImage, class TOutputImage>
class ITK_EXPORT MaskImageFilter :
    public
BinaryFunctorImageFilter<TInputImage,TMaskImage,TOutputImage, 
                         Functor::MaskInput< 
  typename TInputImage::PixelType, 
  typename TMaskImage::PixelType,
  typename TOutputImage::PixelType>   >


{
public:
  /** Standard class typedefs. */
  typedef MaskImageFilter  Self;
  typedef BinaryFunctorImageFilter<TInputImage,TMaskImage,TOutputImage, 
                                   Functor::MaskInput< 
    typename TInputImage::PixelType, 
    typename TMaskImage::PixelType,
    typename TOutputImage::PixelType>   
  >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
 
  /** Method to explicitly set the outside value of the mask. Defaults to 0 */
  void SetOutsideValue( const typename TOutputImage::PixelType & outsudeValue ) 
    {
    this->GetFunctor().SetOutsideValue( outsudeValue );
    }

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(MaskEqualityComparableCheck,
    (Concept::EqualityComparable<typename TMaskImage::PixelType>));
  itkConceptMacro(InputConvertibleToOutputCheck,
    (Concept::Convertible<typename TInputImage::PixelType,
                          typename TOutputImage::PixelType>));
  /** End concept checking */
#endif

protected:
  MaskImageFilter() {}
  virtual ~MaskImageFilter() {}

private:
  MaskImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk


#endif
