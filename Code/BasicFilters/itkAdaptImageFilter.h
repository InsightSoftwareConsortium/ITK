/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAdaptImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAdaptImageFilter_h
#define __itkAdaptImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{

namespace Functor {  
  
  template< class TInput, class TAccessor>
  class AccessorFunctor
  {
  public:
    AccessorFunctor() {};
    ~AccessorFunctor() {};
    
    typedef typename TAccessor::ExternalType OutputType;
    inline OutputType operator()( const TInput & A )
    {
      TAccessor accessor;
      return accessor.Get( A ) ;
    }
  }; 
}

/** \class AdaptImageFilter
 * \brief Convert an image to another pixel type using the specified data accessor.
 *
 * AdaptImageFilter converts an image to another pixel type using a
 * data accessor.  AdaptImageFilter can perform simple cast operations
 * (i.e. short to float) or can extract a subcomponent of a pixel
 * (i.e. extract the green component of an RGB pixel.
 * AdaptImageFilter could also be used for performing simple
 * arithmetic operations at a pixel (i.e. taking the sqrt() or sin()
 * of a pixel); however, these types of operations could also be
 * accomplished using the itk::UnaryImageFilter.
 *
 * The third template parameter for this filter is a DataAccessor
 * which performs the adaption or conversion of a pixel.  The
 * DataAccessor must provide a method called Get() which takes an
 * input pixel and returns an output pixel.  The input pixel can be
 * passed by reference but the output pixel is frequently returned by
 * value. However, a data accessor that returns a subcomponent of a
 * pixel will usually return that subcomponent by reference. For
 * instance, a data accessor that returns the green component of a RGB
 * pixel will simply return a reference to the proper element of the
 * RGB vector. See itk::DataAccessor for performing simple cast
 * operations.
 *
 * \ingroup IntensityImageFilters  Multithreaded
 */
template <class TInputImage, class TOutputImage, class TAccessor>
class ITK_EXPORT AdaptImageFilter:
    public UnaryFunctorImageFilter<TInputImage,TOutputImage,Functor::AccessorFunctor<TInputImage::PixelType, TAccessor> >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef AdaptImageFilter  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
               Functor::AccessorFunctor<TInputImage, TAccessor> > Superclass; 

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(AdaptImageFilter, UnaryFunctorImageFilter);

protected:

  AdaptImageFilter() {}
  virtual ~AdaptImageFilter() {}
  AdaptImageFilter(const Self&) {}
  void operator=(const Self&) {}

private:
};

  
} // end namespace itk
  
#endif
