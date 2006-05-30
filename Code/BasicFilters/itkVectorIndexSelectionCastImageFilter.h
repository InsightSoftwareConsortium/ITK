/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorIndexSelectionCastImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorIndexSelectionCastImageFilter_h
#define __itkVectorIndexSelectionCastImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
  
namespace Functor {  
  
template< class TInput, class TOutput>
class VectorIndexSelectionCast
{
public:
  VectorIndexSelectionCast() {m_Index = 0;}
  ~VectorIndexSelectionCast() {}

  unsigned int GetIndex() const { return m_Index; }
  void SetIndex(unsigned int i) { m_Index = i; }

  bool operator!=( const VectorIndexSelectionCast & other ) const
  {
    if( m_Index != other.m_Index )
      {
      return true;
      }
    return false;
  }
  bool operator==( const VectorIndexSelectionCast & other ) const
  {
    return !(*this != other);
  }
  inline TOutput operator()( const TInput & A )
  {
    return static_cast<TOutput>( A[m_Index] );
  }
      
private:
  unsigned int m_Index;   
}; 
}



 /** \class VectorIndexSelectionCastImageFilter
 *
 * \brief Extracts the selected index of the vector that is the input
 * pixel type
 *
 * This filter is templated over the input image type and 
 * output image type.
 * 
 * The filter expect the input image pixel type to be a vector and 
 * the output image pixel type to be a scalar. The only requirement on
 * the type used for representing the vector is that it must provide an
 * operator[].
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */


template <class TInputImage, class TOutputImage>
class ITK_EXPORT VectorIndexSelectionCastImageFilter :
    public
UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                        Functor::VectorIndexSelectionCast< typename TInputImage::PixelType, 
                                                           typename TOutputImage::PixelType>   >
{
public:
  /** Standard class typedefs. */
  typedef VectorIndexSelectionCastImageFilter Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
                                  Functor::VectorIndexSelectionCast< typename TInputImage::PixelType, 
                                                                     typename TOutputImage::PixelType> > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
    
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Get/Set methods for the index */
  void SetIndex(unsigned int i)
    {
      if (i != this->GetFunctor().GetIndex())
        {
        this->GetFunctor().SetIndex(i);
        this->Modified();
        }
    }
  unsigned int GetIndex(void) const 
    { 
    return this->GetFunctor().GetIndex(); 
    }

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck,
    (Concept::HasNumericTraits<typename TInputImage::PixelType::ValueType>));
  /** End concept checking */
#endif

protected:
  VectorIndexSelectionCastImageFilter() {}
  virtual ~VectorIndexSelectionCastImageFilter() {}
    
  virtual void BeforeThreadedGenerateData()
    {
    const unsigned int index = this->GetIndex();
    const TInputImage * image = this->GetInput();

    const unsigned int numberOfRunTimeComponents = 
               image->GetNumberOfComponentsPerPixel();
    
    typedef typename TInputImage::PixelType    PixelType;

    typedef typename itk::NumericTraits< PixelType >::RealType 
                                                         PixelRealType;

    typedef typename itk::NumericTraits< PixelType >::ScalarRealType 
                                                         PixelScalarRealType;

    const unsigned int numberOfCompileTimeComponents =
             sizeof( PixelRealType ) / sizeof( PixelScalarRealType );

    unsigned int numberOfComponents = numberOfRunTimeComponents;

    if( numberOfCompileTimeComponents > numberOfRunTimeComponents )
      {
      numberOfComponents = numberOfCompileTimeComponents;
      }
      
    if( index >= numberOfComponents )
      {
        itkExceptionMacro(
            << "Selected index = " << index 
            << " is greater than the number of components = "
            << numberOfComponents );
      }
    }
    

private:
  VectorIndexSelectionCastImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};
 
} // end namespace itk


#endif
