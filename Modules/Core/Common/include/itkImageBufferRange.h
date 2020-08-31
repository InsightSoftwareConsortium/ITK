/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkImageBufferRange_h
#define itkImageBufferRange_h

#include <cassert>
#include <cstddef>  // For ptrdiff_t.
#include <iterator> // For random_access_iterator_tag.
#include <limits>
#include <type_traits> // For conditional, is_same, and is_const.

#include "itkMacro.h" // For itkNotUsed.
#include "itkDefaultPixelAccessor.h"
#include "itkDefaultPixelAccessorFunctor.h"
#include "itkDefaultVectorPixelAccessor.h"
#include "itkDefaultVectorPixelAccessorFunctor.h"
#include "itkImageRegion.h"

namespace itk
{

/**
 * \class ImageBufferRange
 * Modern C++11 range to iterate over the pixels of an image.
 * Designed to conform to Standard C++ Iterator requirements,
 * so that it can be used in range-based for loop, and passed to
 * Standard C++ algorithms.
 *
 * The following example adds 42 to each pixel, using a range-based for loop:
   \code
   ImageBufferRange<ImageType> range{ *image };

   for (auto&& pixel : range)
   {
     pixel = pixel + 42;
   }
   \endcode
 *
 * The following example prints the values of the pixels:
   \code
   for (const auto pixel : range)
   {
     std::cout << pixel << std::endl;
   }
   \endcode
 *
 * \author Niels Dekker, LKEB, Leiden University Medical Center
 *
 * \see ImageIterator
 * \see ImageConstIterator
 * \see IndexRange
 * \see ShapedImageNeighborhoodRange
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <typename TImage>
class ImageBufferRange final
{
private:
  using ImageType = TImage;
  using PixelType = typename TImage::PixelType;
  using InternalPixelType = typename TImage::InternalPixelType;
  using AccessorFunctorType = typename TImage::AccessorFunctorType;

  // Tells whether or not this range supports direct pixel access. If it does,
  // iterator::operator*() returns a reference to the internally stored pixel,
  // otherwise iterator::operator*() returns a proxy, which internally uses the
  // AccessorFunctor of the image to access the pixel indirectly.
  constexpr static bool SupportsDirectPixelAccess =
    std::is_same<PixelType, InternalPixelType>::value &&
    std::is_same<typename TImage::AccessorType, DefaultPixelAccessor<PixelType>>::value &&
    std::is_same<AccessorFunctorType, DefaultPixelAccessorFunctor<typename std::remove_const<TImage>::type>>::value;

  // Tells whether or not this range is using a pointer as iterator.
  constexpr static bool UsingPointerAsIterator = SupportsDirectPixelAccess;

  struct EmptyAccessorFunctor
  {};

  using OptionalAccessorFunctorType =
    typename std::conditional<SupportsDirectPixelAccess, EmptyAccessorFunctor, AccessorFunctorType>::type;

  // PixelProxy: internal class that aims to act like a reference to a pixel:
  // It acts either like 'PixelType &' or like 'const PixelType &', depending
  // on its boolean template argument, VIsConst.
  // The proxy retrieves the pixel value using the AccessorFunctor from the image.
  // Note: the extra TDummy argument aims to fix AppleClang 6.0.0.6000056 error
  // "explicit specialization of 'PixelProxy'"and GCC 5.4.0 error "explicit
  // specialization in non-namespace scope".
  template <bool VIsConst, typename TDummy = void>
  class PixelProxy
  {};

  // PixelProxy specialization for const pixel types:
  // acts like 'const PixelType &'
  template <typename TDummy>
  class PixelProxy<true, TDummy> final
  {
  private:
    // Reference to the internal representation of the pixel, located in the image buffer.
    const InternalPixelType & m_InternalPixel;

    // The accessor functor of the image.
    const AccessorFunctorType m_AccessorFunctor;

  public:
    // Deleted member functions:
    PixelProxy() = delete;
    PixelProxy &
    operator=(const PixelProxy &) = delete;

    // Explicitly-defaulted member functions:
    PixelProxy(const PixelProxy &) ITK_NOEXCEPT = default;
    ~PixelProxy() = default;

    // Constructor, called directly by operator*() of the iterator class.
    PixelProxy(const InternalPixelType & internalPixel, const AccessorFunctorType & accessorFunctor) ITK_NOEXCEPT
      : m_InternalPixel{ internalPixel }
      , m_AccessorFunctor(accessorFunctor)
    {}

    // Allows implicit conversion from non-const to const proxy.
    PixelProxy(const PixelProxy<false> & pixelProxy) ITK_NOEXCEPT
      : m_InternalPixel{ pixelProxy.m_InternalPixel }
      , m_AccessorFunctor{ pixelProxy.m_AccessorFunctor }
    {}

    // Conversion operator.
    operator PixelType() const ITK_NOEXCEPT { return m_AccessorFunctor.Get(m_InternalPixel); }
  };


  // PixelProxy specialization for non-const pixel types:
  // acts like 'PixelType &'.
  template <typename TDummy>
  class PixelProxy<false, TDummy> final
  {
  private:
    // The const proxy is a friend, to ease implementing conversion from
    // a non-const proxy to a const proxy.
    friend class PixelProxy<true>;

    // Reference to the internal representation of the pixel, located in the image buffer.
    InternalPixelType & m_InternalPixel;

    // The accessor functor of the image.
    const AccessorFunctorType m_AccessorFunctor;

  public:
    // Deleted member functions:
    PixelProxy() = delete;

    // Explicitly-defaulted member functions:
    ~PixelProxy() = default;
    PixelProxy(const PixelProxy &) ITK_NOEXCEPT = default;

    // Constructor, called directly by operator*() of the iterator class.
    explicit PixelProxy(InternalPixelType & internalPixel, const AccessorFunctorType & accessorFunctor) ITK_NOEXCEPT
      : m_InternalPixel{ internalPixel }
      , m_AccessorFunctor(accessorFunctor)
    {}

    // Conversion operator.
    operator PixelType() const ITK_NOEXCEPT { return m_AccessorFunctor.Get(m_InternalPixel); }

    // Operator to assign a pixel value to the proxy.
    PixelProxy &
    operator=(const PixelType & pixelValue) ITK_NOEXCEPT
    {
      m_AccessorFunctor.Set(m_InternalPixel, pixelValue);
      return *this;
    }

    // Copy-assignment operator.
    PixelProxy &
    operator=(const PixelProxy & pixelProxy) ITK_NOEXCEPT
    {
      // Note that this assignment operator only copies the pixel value.
      // That is the normal behavior when a reference is assigned to another.
      const PixelType pixelValue = pixelProxy;
      *this = pixelValue;
      return *this;
    }


    friend void
    swap(PixelProxy lhs, PixelProxy rhs) ITK_NOEXCEPT
    {
      const auto lhsPixelValue = lhs.m_AccessorFunctor.Get(lhs.m_InternalPixel);
      const auto rhsPixelValue = rhs.m_AccessorFunctor.Get(rhs.m_InternalPixel);

      // Swap only the pixel values, not the image buffer pointers!
      lhs.m_AccessorFunctor.Set(lhs.m_InternalPixel, rhsPixelValue);
      rhs.m_AccessorFunctor.Set(rhs.m_InternalPixel, lhsPixelValue);
    }
  };


  /**
   * \class QualifiedIterator
   * Iterator class that is either 'const' or non-const qualified.
   * A non-const qualified instantiation of this template allows the pixel that
   * it points to, to be modified. A const qualified instantiation does not.
   *
   * \note The definition of this class is private. Please use its type alias
   * ImageBufferRange::iterator, or ImageBufferRange::const_iterator!
   * \see ImageBufferRange
   * \ingroup ImageIterators
   * \ingroup ITKCommon
   */
  template <bool VIsConst>
  class QualifiedIterator final
  {
  private:
    // Const and non-const iterators are friends, in order to implement the
    // constructor that allow conversion from non-const to const iterator.
    friend class QualifiedIterator<!VIsConst>;

    // ImageBufferRange is a friend, as it should be the only one that can
    // directly use the private constructor of the iterator.
    friend class ImageBufferRange;

    // Image type class that is either 'const' or non-const qualified, depending on QualifiedIterator and TImage.
    using QualifiedImageType = typename std::conditional<VIsConst, const ImageType, ImageType>::type;

    static constexpr bool IsImageTypeConst = std::is_const<QualifiedImageType>::value;

    using QualifiedInternalPixelType =
      typename std::conditional<IsImageTypeConst, const InternalPixelType, InternalPixelType>::type;

    // Pixel type class that is either 'const' or non-const qualified, depending on QualifiedImageType.
    using QualifiedPixelType = typename std::conditional<IsImageTypeConst, const PixelType, PixelType>::type;


    // Wraps a reference to a pixel.
    class PixelReferenceWrapper final
    {
    public:
      QualifiedPixelType & m_Pixel;

      // Wraps the pixel reference that is specified by the first argument.
      // Note: the second parameter is unused, but it is there just to support
      // the use case of iterator::operator*(), which uses either
      // PixelReferenceWrapper or PixelProxy, interchangeable, in a generic way.
      // (PixelProxy has an explicit constructor for which the second parameter
      // is its essential AccessorFunctor parameter!)
      explicit PixelReferenceWrapper(QualifiedPixelType & pixel,
                                     EmptyAccessorFunctor itkNotUsed(accessorFunctor)) ITK_NOEXCEPT : m_Pixel(pixel)
      {}

      // Converts implicitly to a reference to the pixel.
      operator QualifiedPixelType &() const ITK_NOEXCEPT { return m_Pixel; }
    };


    // QualifiedIterator data members (strictly private):

    // The accessor functor of the image.
    OptionalAccessorFunctorType m_OptionalAccessorFunctor;

    // Pointer to the current pixel.
    QualifiedInternalPixelType * m_InternalPixelPointer = nullptr;

    // Private constructor, used to create the begin and the end iterator of a range.
    // Only used by its friend class ImageBufferRange.
    QualifiedIterator(const OptionalAccessorFunctorType & accessorFunctor,
                      QualifiedInternalPixelType * const  internalPixelPointer) ITK_NOEXCEPT
      :
      // Note: Use parentheses instead of curly braces to initialize data members,
      // to avoid AppleClang 6.0.0.6000056 compilation error, "no viable conversion..."
      m_OptionalAccessorFunctor(accessorFunctor)
      , m_InternalPixelPointer{ internalPixelPointer }
    {}

  public:
    // Types conforming the iterator requirements of the C++ standard library:
    using difference_type = std::ptrdiff_t;
    using value_type = PixelType;
    using reference =
      typename std::conditional<SupportsDirectPixelAccess, QualifiedPixelType &, PixelProxy<IsImageTypeConst>>::type;
    using pointer = QualifiedPixelType *;
    using iterator_category = std::random_access_iterator_tag;


    /** Default-constructor, as required for any C++11 Forward Iterator. Offers
     * the guarantee added to the C++14 Standard: "value-initialized iterators
     * may be compared and shall compare equal to other value-initialized
     * iterators of the same type."
     * \note `QualifiedIterator<VIsConst>` follows the C++ "Rule of Zero" when
     * VIsConst is true: The other five "special member functions" of the class
     * are then implicitly defaulted. When VIsConst is false, its
     * copy-constructor is provided explicitly, but it still behaves the same as
     * a default implementation.
     */
    QualifiedIterator() = default;

    /** Constructor that allows implicit conversion from non-const to const
     * iterator. Also serves as copy-constructor of a non-const iterator.  */
    QualifiedIterator(const QualifiedIterator<false> & arg) ITK_NOEXCEPT
      :
      // Note: Use parentheses instead of curly braces to initialize data members,
      // to avoid AppleClang 6.0.0.6000056 compilation error, "no viable conversion..."
      m_OptionalAccessorFunctor(arg.m_OptionalAccessorFunctor)
      , m_InternalPixelPointer{ arg.m_InternalPixelPointer }
    {}


    /**  Returns a reference to the current pixel. */
    reference operator*() const ITK_NOEXCEPT
    {
      assert(m_InternalPixelPointer != nullptr);

      using PixelWrapper = typename std::conditional<SupportsDirectPixelAccess, PixelReferenceWrapper, reference>::type;

      return PixelWrapper{ *m_InternalPixelPointer, m_OptionalAccessorFunctor };
    }


    /** Prefix increment ('++it'). */
    QualifiedIterator &
    operator++() ITK_NOEXCEPT
    {
      assert(m_InternalPixelPointer != nullptr);
      ++m_InternalPixelPointer;
      return *this;
    }


    /** Postfix increment ('it++').
     * \note Usually prefix increment ('++it') is preferable. */
    QualifiedIterator
    operator++(int) ITK_NOEXCEPT
    {
      auto result = *this;
      ++(*this);
      return result;
    }


    /** Prefix decrement ('--it'). */
    QualifiedIterator &
    operator--() ITK_NOEXCEPT
    {
      assert(m_InternalPixelPointer != nullptr);
      --m_InternalPixelPointer;
      return *this;
    }


    /** Postfix increment ('it--').
     * \note  Usually prefix increment ('--it') is preferable. */
    QualifiedIterator
    operator--(int) ITK_NOEXCEPT
    {
      auto result = *this;
      --(*this);
      return result;
    }


    /** Returns (it1 == it2) for iterators it1 and it2. Note that these iterators
     * should be from the same range. This operator does not support comparing iterators
     * from different ranges. */
    friend bool
    operator==(const QualifiedIterator & lhs, const QualifiedIterator & rhs) ITK_NOEXCEPT
    {
      return lhs.m_InternalPixelPointer == rhs.m_InternalPixelPointer;
    }


    /** Returns (it1 != it2) for iterators it1 and it2. */
    friend bool
    operator!=(const QualifiedIterator & lhs, const QualifiedIterator & rhs) ITK_NOEXCEPT
    {
      // Implemented just like the corresponding std::rel_ops operator.
      return !(lhs == rhs);
    }


    /** Returns (it1 < it2) for iterators it1 and it2. */
    friend bool
    operator<(const QualifiedIterator & lhs, const QualifiedIterator & rhs) ITK_NOEXCEPT
    {
      return lhs.m_InternalPixelPointer < rhs.m_InternalPixelPointer;
    }


    /** Returns (it1 > it2) for iterators it1 and it2. */
    friend bool
    operator>(const QualifiedIterator & lhs, const QualifiedIterator & rhs) ITK_NOEXCEPT
    {
      // Implemented just like the corresponding std::rel_ops operator.
      return rhs < lhs;
    }


    /** Returns (it1 <= it2) for iterators it1 and it2. */
    friend bool
    operator<=(const QualifiedIterator & lhs, const QualifiedIterator & rhs) ITK_NOEXCEPT
    {
      // Implemented just like the corresponding std::rel_ops operator.
      return !(rhs < lhs);
    }


    /** Returns (it1 >= it2) for iterators it1 and it2. */
    friend bool
    operator>=(const QualifiedIterator & lhs, const QualifiedIterator & rhs) ITK_NOEXCEPT
    {
      // Implemented just like the corresponding std::rel_ops operator.
      return !(lhs < rhs);
    }


    /** Does (it += d) for iterator 'it' and integer value 'n'. */
    friend QualifiedIterator &
    operator+=(QualifiedIterator & it, const difference_type n) ITK_NOEXCEPT
    {
      it.m_InternalPixelPointer += n;
      return it;
    }

    /** Does (it -= d) for iterator 'it' and integer value 'n'. */
    friend QualifiedIterator &
    operator-=(QualifiedIterator & it, const difference_type n) ITK_NOEXCEPT
    {
      it += (-n);
      return it;
    }

    /** Returns (it1 - it2) for iterators it1 and it2. */
    friend difference_type
    operator-(const QualifiedIterator & lhs, const QualifiedIterator & rhs) ITK_NOEXCEPT
    {
      return lhs.m_InternalPixelPointer - rhs.m_InternalPixelPointer;
    }


    /** Returns (it + n) for iterator 'it' and integer value 'n'. */
    friend QualifiedIterator
    operator+(QualifiedIterator it, const difference_type n) ITK_NOEXCEPT
    {
      return it += n;
    }


    /** Returns (n + it) for iterator 'it' and integer value 'n'. */
    friend QualifiedIterator
    operator+(const difference_type n, QualifiedIterator it) ITK_NOEXCEPT
    {
      return it += n;
    }


    /** Returns (it - n) for iterator 'it' and integer value 'n'. */
    friend QualifiedIterator
    operator-(QualifiedIterator it, const difference_type n) ITK_NOEXCEPT
    {
      return it += (-n);
    }


    /** Returns it[n] for iterator 'it' and integer value 'n'. */
    reference operator[](const difference_type n) const ITK_NOEXCEPT { return *(*this + n); }


    /** Explicitly-defaulted assignment operator. */
    QualifiedIterator &
    operator=(const QualifiedIterator &) ITK_NOEXCEPT = default;
  };

  static constexpr bool IsImageTypeConst = std::is_const<TImage>::value;

  using QualifiedInternalPixelType =
    typename std::conditional<IsImageTypeConst, const InternalPixelType, InternalPixelType>::type;

  class AccessorFunctorInitializer final
  {
  private:
    ImageType & m_Image;

  public:
    explicit AccessorFunctorInitializer(ImageType & image) ITK_NOEXCEPT : m_Image(image) {}

    operator EmptyAccessorFunctor() const ITK_NOEXCEPT { return {}; }

    operator AccessorFunctorType() const ITK_NOEXCEPT
    {
      AccessorFunctorType result = {};
      result.SetPixelAccessor(m_Image.GetPixelAccessor());
      result.SetBegin(m_Image.ImageType::GetBufferPointer());
      return result;
    }
  };


  // Helper class for begin() and end(), to ease proper initialization of an
  // ImageBufferRange iterator (either a 'QualifiedIterator' or a raw pixel pointer).
  class IteratorInitializer final
  {
  private:
    OptionalAccessorFunctorType  m_OptionalAccessorFunctor;
    QualifiedInternalPixelType * m_InternalPixelPointer;

  public:
    explicit IteratorInitializer(OptionalAccessorFunctorType  optionalAccessorFunctor,
                                 QualifiedInternalPixelType * internalPixelPointer) ITK_NOEXCEPT
      : m_OptionalAccessorFunctor(optionalAccessorFunctor)
      , m_InternalPixelPointer(internalPixelPointer)
    {}

    // Converts to a 'QualifiedIterator' object.
    template <bool VIsConst>
    operator QualifiedIterator<VIsConst>() const ITK_NOEXCEPT
    {
      return QualifiedIterator<VIsConst>{ m_OptionalAccessorFunctor, m_InternalPixelPointer };
    }

    // Converts to a raw pixel pointer.
    operator QualifiedInternalPixelType *() const ITK_NOEXCEPT { return m_InternalPixelPointer; }
  };


  // ImageBufferRange data members (strictly private):

  // The accessor functor of the image.
  OptionalAccessorFunctorType m_OptionalAccessorFunctor;

  // Pointer to the buffer of the image.
  QualifiedInternalPixelType * m_ImageBufferPointer = nullptr;

  // Image size.
  SizeValueType m_NumberOfPixels = 0;

public:
  using const_iterator =
    typename std::conditional<UsingPointerAsIterator, const InternalPixelType *, QualifiedIterator<true>>::type;
  using iterator = typename std::
    conditional<UsingPointerAsIterator, QualifiedInternalPixelType *, QualifiedIterator<IsImageTypeConst>>::type;
  using reverse_iterator = std::reverse_iterator<iterator>;
  using const_reverse_iterator = std::reverse_iterator<const_iterator>;


  /** Explicitly-defaulted default-constructor. Constructs an empty range.
   * \note The other five "special member functions" (copy-constructor,
   * copy-assignment operator, move-constructor, move-assignment operator,
   * and destructor) are defaulted implicitly, following the C++ "Rule of Zero".
   */
  ImageBufferRange() = default;


  /** Specifies a range of the pixels of an image.
   */
  explicit ImageBufferRange(ImageType & image)
    : // Note: Use parentheses instead of curly braces to initialize data members,
      // to avoid AppleClang 6.0.0.6000056 compile errors, "no viable conversion..."
    m_OptionalAccessorFunctor(AccessorFunctorInitializer{ image })
    , m_ImageBufferPointer{ image.ImageType::GetBufferPointer() }
    , m_NumberOfPixels{ image.ImageType::GetBufferedRegion().GetNumberOfPixels() }
  {}


  /** Returns an iterator to the first pixel. */
  iterator
  begin() const ITK_NOEXCEPT
  {
    return IteratorInitializer{ m_OptionalAccessorFunctor, m_ImageBufferPointer };
  }

  /** Returns an 'end iterator' for this range. */
  iterator
  end() const ITK_NOEXCEPT
  {
    return IteratorInitializer{
      m_OptionalAccessorFunctor,
      m_ImageBufferPointer + m_NumberOfPixels,
    };
  }

  /** Returns a const iterator to the first pixel.
   * Provides only read-only access to the pixel data. */
  const_iterator
  cbegin() const ITK_NOEXCEPT
  {
    return this->begin();
  }

  /** Returns a const 'end iterator' for this range. */
  const_iterator
  cend() const ITK_NOEXCEPT
  {
    return this->end();
  }

  /** Returns a reverse 'begin iterator' for this range. */
  reverse_iterator
  rbegin() const ITK_NOEXCEPT
  {
    return reverse_iterator(this->end());
  }

  /** Returns a reverse 'end iterator' for this range. */
  reverse_iterator
  rend() const ITK_NOEXCEPT
  {
    return reverse_iterator(this->begin());
  }

  /** Returns a const reverse 'begin iterator' for this range. */
  const_reverse_iterator
  crbegin() const ITK_NOEXCEPT
  {
    return this->rbegin();
  }

  /** Returns a const reverse 'end iterator' for this range. */
  const_reverse_iterator
  crend() const ITK_NOEXCEPT
  {
    return this->rend();
  }


  /** Returns the size of the range, that is the number of pixels. */
  std::size_t
  size() const ITK_NOEXCEPT
  {
    return m_NumberOfPixels;
  }


  /** Tells whether the range is empty. */
  bool
  empty() const ITK_NOEXCEPT
  {
    return m_NumberOfPixels == 0;
  }


  /** Subscript operator. Allows random access, to the nth pixel.
   * \note The return type QualifiedIterator<false>::reference is equivalent to
   * iterator::reference.
   */
  typename QualifiedIterator<false>::reference operator[](const std::size_t n) const ITK_NOEXCEPT
  {
    assert(n < this->size());
    assert(n <= static_cast<std::size_t>(std::numeric_limits<std::ptrdiff_t>::max()));

    return this->begin()[static_cast<std::ptrdiff_t>(n)];
  }
};

/** Creates a range to iterate over the pixels of the specified image.
 * Returns an empty range when the specified argument is a nullptr (which
 * is a valid use case).
 */
template <typename TImage>
ImageBufferRange<TImage>
MakeImageBufferRange(TImage * const image)
{
  if (image == nullptr)
  {
    return {};
  }
  else
  {
    return ImageBufferRange<TImage>{ *image };
  }
}


} // namespace itk
#endif
