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
#ifndef itkAdaptImageFilter_h
#define itkAdaptImageFilter_h

#include "itkUnaryFunctorImageFilter.h"

namespace itk
{
namespace Functor
{
/** \class AccessorFunctor
   * \brief Convert an accessor to a functor so that it can be used in a
   * UnaryFunctorImageFilter.
   *
   * AccessorFunctor converts a data accessor to a functor object.  This
   * allows an accessor to be used as functor in a UnaryFunctorImageFilter,
   * BinaryFunctorImageFilter, TernaryFunctorImageFilter, or
   * NaryFunctionImageFilter.
   * \ingroup ITKImageIntensity
   */
template< typename TInput, typename TAccessor >
class AccessorFunctor
{
public:
  /** Standard class typedefs. */
  typedef AccessorFunctor Self;
  typedef TAccessor       AccessorType;

  /** Constructor and destructor. */
  AccessorFunctor():m_Accessor() {}
  ~AccessorFunctor() {}

  /** operator().  This is the "call" method of the functor. */
  typedef typename TAccessor::ExternalType OutputType;
  inline OutputType operator()(const TInput & A) const
  {
    return m_Accessor.Get(A);
  }

  /** Get the accessor. The accessor is returned by reference. */
  AccessorType & GetAccessor()
  {
    return m_Accessor;
  }

  /** Assignment operator */
  AccessorFunctor & operator=(const AccessorFunctor & functor)
  {
    m_Accessor = functor.m_Accessor;
    return *this;
  }

  /** Set the accessor object. This replaces the current accessor with
   * a copy of the specified accessor.  This allows the user to
   * specify an accessor that has ivars set differently that the default
   * accessor.
   */
  void SetAccessor(AccessorType & accessor)
  {
    m_Accessor = accessor;
  }

  /** operator!=.  Needed to determine if two accessors are the same. */
  bool operator!=(const Self & functor) const
  {
    return ( m_Accessor != functor.m_Accessor );
  }

  bool operator==(const Self & other) const
  {
    return !( *this != other );
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
 * arithmetic operations at a pixel (i.e. taking the std::sqrt() or std::sin()
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
 * \ingroup IntensityImageFilters  MultiThreaded
 * \ingroup ITKImageIntensity
 */
template< typename TInputImage, typename TOutputImage, typename TAccessor >
class AdaptImageFilter:
  public UnaryFunctorImageFilter< TInputImage, TOutputImage,
                                  Functor::AccessorFunctor< typename TInputImage::PixelType, TAccessor > >
{
public:
  /** Standard class typedefs. */
  typedef AdaptImageFilter Self;

  typedef UnaryFunctorImageFilter< TInputImage,
                                   TOutputImage,
                                   Functor::AccessorFunctor<
                                     typename TInputImage::PixelType,
                                     TAccessor > >  Superclass;

  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;
  typedef typename Superclass::FunctorType FunctorType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedef for the accessor type */
  typedef TAccessor AccessorType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(AdaptImageFilter, UnaryFunctorImageFilter);

  /** Get the accessor. This is a convenience method so the user */
  AccessorType & GetAccessor() { return this->GetFunctor().GetAccessor(); }

  /** Set the accessor. This is a convenience method so the user does */
  void SetAccessor(AccessorType & accessor)
  {
    FunctorType functor;

    functor = this->GetFunctor();
    if ( accessor != functor.GetAccessor() )
      {
      functor.SetAccessor(accessor);
      this->SetFunctor(functor);
      this->Modified();
      }
  }

protected:
  AdaptImageFilter() {}
  virtual ~AdaptImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(AdaptImageFilter);
};
} // end namespace itk

#endif
