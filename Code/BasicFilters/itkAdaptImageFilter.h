/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAdaptImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkAdaptImageFilter_h
#define __itkAdaptImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{

namespace Functor {  
  
  /** \class AccessorFunctor
   * \brief Convert an accessor to a functor so that it can be used in a 
   * UnaryFunctorImageFilter.
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
    /** Standard class typedefs. */
    typedef AccessorFunctor Self;
    typedef TAccessor AccessorType;

    /** Constructor and destructor. */
    AccessorFunctor(): m_Accessor() {}
    ~AccessorFunctor() {}

    /** operator().  This is the "call" method of the functor. */
    typedef typename TAccessor::ExternalType OutputType;
    inline OutputType operator()( const TInput & A )
      { return m_Accessor.Get( A ); }

    /** Get the accessor. The accessor is returned by reference. */
    AccessorType& GetAccessor() 
      { return m_Accessor; }

    /** Set the accessor object. This replaces the current accessor with
     * a copy of the specified accessor.  This allows the user to
     * specify an accessor that has ivars set differently that the default
     * accessor.
     */
    void SetAccessor(AccessorType& accessor) 
      { m_Accessor = accessor; };

    /** operator!=.  Needed to determine if two accessors are the same. */
    bool operator!=( const Self& functor ) const
      { return (m_Accessor != functor.m_Accessor); }
    
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
  /** Standard class typedefs. */
  typedef AdaptImageFilter  Self;
  typedef UnaryFunctorImageFilter<TInputImage,TOutputImage, 
               Functor::AccessorFunctor<TInputImage, TAccessor> > Superclass; 
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedef for the accessor type */
  typedef TAccessor AccessorType;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(AdaptImageFilter, UnaryFunctorImageFilter);

  /** Get the accessor. This is a convenience method so the user */
  AccessorType& GetAccessor() { return this->GetFunctor()->GetAccessor(); };

  /** Set the accessor. This is a convenience method so the user does */
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

private:
  AdaptImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

  
} // end namespace itk
  
#endif
