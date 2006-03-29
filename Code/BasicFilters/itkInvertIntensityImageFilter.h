/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInvertIntensityImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkInvertIntensityImageFilter_h
#define __itkInvertIntensityImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{

namespace Functor {  
 
template< typename TInput, typename  TOutput>
class InvertIntensityTransform
{
public:
  typedef typename NumericTraits< TInput >::RealType RealType;
  InvertIntensityTransform() {}
  ~InvertIntensityTransform() {}

  void SetMaximum( TOutput max ) { m_Maximum = max; }

  bool operator!=( const InvertIntensityTransform & other ) const
  {
    if( m_Maximum != other.m_Maximum )
      {
      return true;
      }
    return false;
  }

  bool operator==( const InvertIntensityTransform & other ) const
  {
    return !(*this != other);
  }

  inline TOutput operator()( const TInput & x )
  {
    TOutput  result = static_cast<TOutput>( m_Maximum - x );
    return result;
  }
private:
  TInput  m_Maximum;
}; 

}  // end namespace functor


/** \class InvertIntensityImageFilter
 * \brief Invert intensity of an image 
 *
 * InvertIntensityImageFilter invert intensity of pixels by
 * subtracting pixel value to a maximum value. The maximum value can
 * be set with SetMaximum and defaults the maximum of input pixel
 * type. This filter can be used to invert, for example, a binary
 * image, a distance map, etc.
 *
 * \author Gaëtan Lehmann. Biologie du Développement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa IntensityWindowingImageFilter ShiftScaleImageFilter
 * \ingroup IntensityImageFilters  Multithreaded
 *
 */
template <typename  TInputImage, typename  TOutputImage=TInputImage>
class ITK_EXPORT InvertIntensityImageFilter :
    public
    UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                            Functor::InvertIntensityTransform< 
  typename TInputImage::PixelType, 
  typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef InvertIntensityImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::InvertIntensityTransform< 
    typename TInputImage::PixelType, 
    typename TOutputImage::PixelType> >  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename NumericTraits<InputPixelType>::RealType RealType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  itkSetMacro( Maximum, InputPixelType );
  itkGetConstReferenceMacro( Maximum, InputPixelType );

  /** Print internal ivars */
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData(void);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<InputPixelType>));
  /** End concept checking */
#endif

protected:
  InvertIntensityImageFilter();
  virtual ~InvertIntensityImageFilter() {};

private:
  InvertIntensityImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  InputPixelType        m_Maximum;
};


  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkInvertIntensityImageFilter.txx"
#endif
  
#endif
