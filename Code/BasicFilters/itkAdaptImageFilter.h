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
  
  /**
   * \class AccessorFunctor
   * \brief Convert an accessor to a functor so that it can be used in a UnaryFunctorImageFilter.
   *
   * AccessorFunctor converts a data accessor to a functor object.  This
   * allows an accessor to be used as functor in a UnaryFunctorImageFilter,
   * BinaryFunctorImageFilter, TernaryFunctorImageFilter, or
   * NaryFunctionImageFilter.
   */
  template <class TInput, class TAccessor>
  class AccessorFunctor
  {
  public:
    AccessorFunctor() {};
    ~AccessorFunctor() {};

    typedef AccessorFunctor Self;
    typedef TAccessor AccessorType;

    /**
     * operator().  This is the "call" method of the functor.
     */
    typedef typename TAccessor::ExternalType OutputType;
    inline OutputType operator()( const TInput & A )
    {
      return m_Accessor.Get( A ) ;
    }

    /**
     * Get the accessor. The accessor is returned by reference.
     * (Accessors do not have to derive from itk::LightObject, so they
     * do not necessarily have a reference count.  So we cannot return
     * a SmartPointer.)
     */
    AccessorType& GetAccessor() {return m_Accessor; };

    /**
     * Set the accessor object. This replaces the current accessor with
     * a copy of the specified accessor.  This allows the user to
     * specify an accessor that has ivars set differently that the default
     * accessor.
     */
    void SetAccessor(AccessorType& accessor) { m_Accessor = accessor; };

    /**
     * operator!=.  Needed to determine if two accessors are the same.
     * This is used by UnaryFunctorImageFilter and AdaptImageFilter.
     */
    bool operator!=( const Self& functor ) const
    {
      return (m_Accessor != functor.m_Accessor);
    }
    
  private:
    AccessorType m_Accessor;
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
    public UnaryFunctorImageFilter<TInputImage,TOutputImage,Functor::AccessorFunctor<ITK_TYPENAME TInputImage::PixelType, TAccessor> >
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
   * Typedef for the accessor type
   */
  typedef TAccessor AccessorType;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(AdaptImageFilter, UnaryFunctorImageFilter);

  /**
   * Get the accessor. This is a convenience method so the user
   * does not have to first get the functor and then get a copy
   * of the accessor.  This method is used when the user needs to change
   * an ivar on the accessor (like for the NthElementPixelAccessor).
   */
  AccessorType& GetAccessor() { return this->GetFunctor()->GetAccessor(); };

  /**
   * Set the accessor. This is a convenience method so the user does
   * not have to first get a copy of the functor and then get a copy
   * of the accessor.  This method is used when the user needs to
   * change an ivar on the accessor (like for the
   * NthElementPixelAccessor).  Calling this method requires an
   * operator!=() be defined on the accessor (or the compiler's
   * default implementation of operator!=() being appropriate).
   */
  void SetAccessor(AccessorType& accessor)
  {
    FunctorType functor;
    functor = this->GetFunctor();
    if (accessor != functor.GetAccessor())
      {
      functor.SetAccessor( accessor );
      this->SetFunctor( functor );
      this->Modified();
      }
  }

protected:
  AdaptImageFilter() {}
  virtual ~AdaptImageFilter() {}
  AdaptImageFilter(const Self&) {}
  void operator=(const Self&) {}

};

  
} // end namespace itk
  
#endif
