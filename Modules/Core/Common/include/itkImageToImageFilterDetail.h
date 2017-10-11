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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkImageToImageFilterDetail_h
#define itkImageToImageFilterDetail_h

#include "itkImageRegion.h"
#include "itkSmartPointer.h"

namespace itk
{
/** ImageToImageFilterDetail namespace to house implementations of
 * functions that are dependent on dimension. These functions are
 * overloaded based on type unique for each different dimension.
 * These functions cannot be member functions of a class like
 * ImageToImageFilter since explicit instantiation of an
 * ImageToImageFilter would force the instantiation of all versions of
 * these overloaded functions.
 */
namespace ImageToImageFilterDetail
{
/** \struct  DispatchBase
 * \brief Base class for a class used to dispatch to dimension specific implementations.
 *
 * DispatchBase is base class used as the default case when implementations
 * are dispatched to overloaded routines based on dimension.
 *
 * \sa Dispatch
 */
struct DispatchBase {};

/** \struct BooleanDispatch
 * \brief Templated class to produce a unique type "true" and "false".
 *
 * BooleanDispatch is a templated class that produce a unique type for
 * for "true" and for "false".  These types may be used to decide which
 * version of an overloaded function to call.
 */
template< bool >
struct BooleanDispatch {};

/** \struct IntDispatch
 * \brief Templated class to produce a unique type for each integer
 *
 * IntDispatch is a templated class that produces a unique
 * type for each integer.  IntDispatch is typically
 * used as a parameter to an overloaded function where a different
 * version of the routine would need to be called for each integer value.
 */
template< int >
struct IntDispatch:public DispatchBase {};

/** \struct UnsignedIntDispatch
 * \brief Templated class to produce a unique type for each unsigned integer (usually a dimension).
 *
 * UnsignedIntDispatch is a templated class that produces a unique
 * type for each unsigned integer.  UnsignedIntDispatch is typically
 * used as a parameter to an overloaded function where each version
 * of the overloaded function is for a unique dimension.  For
 * instance, an algorithm may provide two implementations: one
 * optimized for two-dimensional images and another for any of other
 * data dimension.  For instance:
 *
 *     void Calculate(const DispatchBase&);           // General ND version
 *     void Calculate(const UnsignedIntDispatch<2>&); // 2D optimized version
 */
template< unsigned int >
struct UnsignedIntDispatch:public DispatchBase {};

/** \struct BinaryBooleanDispatch
 * \brief Templated class to produce a unique type for a pairing of booleans.
 *
 * BinaryBooleanDispatch is a templated class that produces a unique type
 * for each pairing of two boolean values ((true, true), (true, false),
 * (false, true), (false, false)).
 */
template< bool B1, bool B2 >
struct BinaryBooleanDispatch {
  /** Typedefs to extract the unique types for the first and second
        template parameters (true/false) */
  typedef BooleanDispatch< B1 > FirstType;
  typedef BooleanDispatch< B2 > SecondType;
};

/** \struct BinaryIntDispatch
 * \brief Templated class to produce a unique type for a pairing of integers.
 *
 * IntBooleanDispatch is a templated class that produces a unique type
 * for each pairing of two integer values.
 */
template< int D1, int D2 >
struct BinaryIntDispatch {
  /** Typedefs to extract the unique types for the first and second
        template parameters (unique type for the integer value) */
  typedef IntDispatch< D1 > FirstType;
  typedef IntDispatch< D2 > SecondType;
};

/** \struct BinaryUnsignedIntDispatch
 * \brief Templated class to produce a unique type for a pairing of unsigned integers (usually two dimensions).
 *
 * BinaryUnsignedIntDispatch is a templated class that produces a
 * unique type for a pairing of unsigned integers.
 * BinaryUnsignedIntDispatch is typically used a parameter to an
 * overloaded function where each version of the overloaded function
 * is for a different pairing of unsigned integers.  This type may
 * be used to produce a unique type for an (input dimension, output
 * dimension) pairing.
 */
template< unsigned int D1, unsigned int D2 >
struct BinaryUnsignedIntDispatch:public DispatchBase {
  /** Typedefs to extract the unique types for the first and second
      template parameters (unique type for the unsigned integer value) */
  typedef UnsignedIntDispatch< D1 > FirstType;
  typedef UnsignedIntDispatch< D2 > SecondType;

  /** Helper types to determine whether the two integers are the same,
   * the first greater than the second, or the first less than the second.
   *
   * ComparisonType will be either IntDispatch<0>, IntDispatch<1>, or
   * IntDispatch<-1>.
   *
   * IntDispatch<0> means the two specified integers are the same.
   * IntDispatch<1> means the first integer is greater than the second
   * IntDispatch<-1> means the first integer is less than the second
   *
   * The FirstEqualsSecondType, FirstGreaterThanSecondType,
   * FirstLessThanSecondType typedefs are provided as convenience types
   * which can be used to declare arguments to functions.  They themselves
   * do not indicate the relationship between D1 and D2.
   */
  typedef IntDispatch < ( D1 > D2 ) - ( D1 < D2 ) > ComparisonType;
  typedef IntDispatch< 0 >                          FirstEqualsSecondType;
  typedef IntDispatch< 1 >                          FirstGreaterThanSecondType;
  typedef IntDispatch< -1 >                         FirstLessThanSecondType;
};

/**
 * Copy an image region (start index and size) for the case where
 * the source and destination region are the same dimension.  This
 * is a trivial copy of a region. This is an overloaded function.
 * The other versions of this functions handle the case where the
 * destination dimension is greater than the source dimension and
 * the case where the destination dimension is less than the source
 * dimension.
 *
 * Note that the use of source and destination reflect where where
 * is information is coming from and going to.  When used in the
 * pipeline mechanism, the region requested by the output of a
 * filter is used to define the region required on the input.  In
 * this case the output of the filter is the source region and the
 * input of the filter is the destination region.
 */
template< unsigned int D1, unsigned int D2 >
void ImageToImageFilterDefaultCopyRegion(const typename
                                         BinaryUnsignedIntDispatch< D1, D2 >::FirstEqualsSecondType &,
                                         ImageRegion< D1 > & destRegion,
                                         const ImageRegion< D2 > & srcRegion)
{
  destRegion = srcRegion;
}

/**
 * Copy an image region (start index and size) for the case where
 * the source region has a greater dimension than the destination
 * region.  This copies the first portion of the source into
 * destination. This is an overloaded function.  The other versions
 * of this functions handle the case where the source dimension is
 * less than the destination dimension and the case where the
 * destination and source are the same dimension.
 *
 * Note that the use of source and destination reflect where
 * where is information is coming from and going to.  When used
 * in the pipeline mechanism, the region requested by the output
 * of a filter is used to define the region required on the input.
 * In this case the output of the filter is the source and the
 * input of the filter is the destination.
 */
template< unsigned int D1, unsigned int D2 >
void ImageToImageFilterDefaultCopyRegion(const typename
                                         BinaryUnsignedIntDispatch< D1, D2 >::FirstLessThanSecondType &,
                                         ImageRegion< D1 > & destRegion,
                                         const ImageRegion< D2 > & srcRegion)
{
  // Source dimension is greater than the destination dimension, copy the
  // first part of the source into the destination
  unsigned int dim;

  Index< D1 >         destIndex;
  Size< D1 >          destSize;
  const Index< D2 > & srcIndex = srcRegion.GetIndex();
  const Size< D2 > &  srcSize = srcRegion.GetSize();

  // copy what we can
  for ( dim = 0; dim < D1; ++dim )
    {
    destIndex[dim] = srcIndex[dim];
    destSize[dim] = srcSize[dim];
    }

  destRegion.SetIndex(destIndex);
  destRegion.SetSize(destSize);
}

/**
 * Copy an image region (start index and size) for the case where
 * the source region has a lesser dimension than the destination
 * region.  This copies the source into the first part of the
 * destination. This is an overloaded function.  The other versions
 * of this functions handle the case where the source dimension is
 * less than the destination dimension and the case where the
 * destination and source are the same dimension.
 *
 * Note that the use of source and destination reflect where
 * where is information is coming from and going to.  When used
 * in the pipeline mechanism, the region requested by the output
 * of a filter is used to define the region required on the input.
 * In this case the output of the filter is the source and the
 * input of the filter is the destination.
 */
template< unsigned int D1, unsigned int D2 >
void ImageToImageFilterDefaultCopyRegion(const typename
                                         BinaryUnsignedIntDispatch< D1, D2 >::FirstGreaterThanSecondType &,
                                         ImageRegion< D1 > & destRegion,
                                         const ImageRegion< D2 > & srcRegion)
{
  // Source dimension is less than the destination dimension, copy source
  // into the first part of the destination and set zeros elsewhere.
  unsigned int dim;

  Index< D1 >         destIndex;
  Size< D1 >          destSize;
  const Index< D2 > & srcIndex = srcRegion.GetIndex();
  const Size< D2 > &  srcSize = srcRegion.GetSize();

  // copy what we can
  for ( dim = 0; dim < D2; ++dim )
    {
    destIndex[dim] = srcIndex[dim];
    destSize[dim] = srcSize[dim];
    }
  // fill in the rest of the dimensions with zero/one
  for (; dim < D1; ++dim )
    {
    destIndex[dim] = 0;
    destSize[dim] = 1;
    }

  destRegion.SetIndex(destIndex);
  destRegion.SetSize(destSize);
}

/** \class ImageRegionCopier
 * \brief A Function object used to dispatching to a routine to copy a region
 *        (start index and size).
 *
 * Function object used for dispatching to various routines to copy
 * a region (start index and size).  Most filters use this function
 * object as trivial copy because they require the input image
 * dimension to match the output image dimension.  However, some
 * filters like itk::ExtractImageFilter can support output images of
 * a lower dimension that the input.
 *
 * This function object is used by the default implementation of
 * ImageToImageFilter::GenerateInputRequestedRegion(). It can also
 * be used in routines like ImageSource::ThreadedGenerateData()
 * where a filter may need to map the the output region for a
 * particular thread to an input region.
 *
 * This copier uses a "dispatch pattern" to call one of three
 * overloaded functions depending on whether the source and
 * destination dimensions are the same, the source is of a higher
 * dimension than the destination, or the source is of a lower
 * dimension than the destination. The use of an overloaded function
 * is required for proper compilation of the various cases.
 *
 * For the latter two cases, trivial implementations are used.  If
 * the source dimension is a lower dimension than the destination,
 * the output region information is copied into the first portion of
 * the destination region and the rest of the input region is set to
 * zero.  If the source region is a higher dimension than the
 * destination, the first portion of the source region is copied to
 * the destination region.
 *
 * If a filter needs a different behavior is will need to override
 * the CallCopyOutputRegionToInputRegion() method or the
 * CallCopyInputRegionToOutputRegion() method and delegate to the
 * appropriate RegionCopier class.
 * \ingroup ITKCommon
 */
template< unsigned int D1, unsigned int D2 >
class ImageRegionCopier
{
public:
  virtual void operator()(ImageRegion< D1 > & destRegion,
                          const ImageRegion< D2 > & srcRegion) const
  {
    typedef typename BinaryUnsignedIntDispatch< D1, D2 >::ComparisonType ComparisonType;
    ImageToImageFilterDefaultCopyRegion< D1, D2 >(
      ComparisonType(),
      destRegion, srcRegion);
  }

  virtual ~ImageRegionCopier() {}
};

/** Stream operator for ImageRegionCopier objects. Just prints the RTTI
    typename. */
template< unsigned int D1, unsigned int D2 >
std::ostream & operator<<(std::ostream & os,
                          const ImageRegionCopier< D1, D2 > &)
{
  os << "ImageRegionCopier: "
     << typeid( ImageRegionCopier< D1, D2 > ).name() << std::endl;
  return os;
}

/** operator!= for ImageRegionCopier objects. */
template< unsigned int D1, unsigned int D2 >
bool operator!=(const ImageRegionCopier< D1, D2 > & c1,
                const ImageRegionCopier< D1, D2 > & c2)
{
  return &c1 != &c2;
}


template< unsigned int D1, unsigned int D2 >
void ImageToImageFilterDefaultCopyInformation(const typename
                                              BinaryUnsignedIntDispatch< D1, D2 >::FirstEqualsSecondType &,
                                              ImageBase< D1 >* destImage,
                                              const ImageBase< D2 >* srcImage)
{
  destImage->CopyInformation(srcImage);
}


template< unsigned int D1, unsigned int D2 >
void ImageToImageFilterDefaultCopyInformation(const typename
                                              BinaryUnsignedIntDispatch< D1, D2 >::FirstGreaterThanSecondType &,
                                              ImageBase< D1 >* destImage,
                                              const ImageBase< D2 >* srcImage)
{
  typedef ImageBase<D1> DestinationImageType;
  typedef ImageBase<D2> SourceImageType;

  // Copy what we can from the image from spacing and origin of the input
  // This logic needs to be augmented with logic that select which
  // dimensions to copy
  const typename SourceImageType::SpacingType &inputSpacing = srcImage->GetSpacing();
  const typename SourceImageType::PointType &inputOrigin = srcImage->GetOrigin();
  const typename SourceImageType::DirectionType &inputDirection = srcImage->GetDirection();

  typename DestinationImageType::SpacingType destSpacing;
  typename DestinationImageType::PointType destOrigin;
  typename DestinationImageType::DirectionType destDirection;

  // copy the input to the output and fill the rest of the
  // output with zeros.
  unsigned int i = 0;
  for (; i < SourceImageType::ImageDimension; ++i )
    {
    destSpacing[i] = inputSpacing[i];
    destOrigin[i] = inputOrigin[i];
    for ( unsigned int j = 0; j < DestinationImageType::ImageDimension; j++ )
      {
      if ( j < SourceImageType::ImageDimension )
        {
        destDirection[j][i] = inputDirection[j][i];
        }
      else
        {
        destDirection[j][i] = 0.0;
        }
      }
    }
  for (; i < DestinationImageType::ImageDimension; ++i )
    {
    destSpacing[i] = 1.0;
    destOrigin[i] = 0.0;
    for ( unsigned int j = 0; j < DestinationImageType::ImageDimension; j++ )
      {
      if ( j == i )
        {
        destDirection[j][i] = 1.0;
        }
      else
        {
        destDirection[j][i] = 0.0;
        }
      }
    }

  // set the spacing and origin
  destImage->SetSpacing(destSpacing);
  destImage->SetOrigin(destOrigin);
  destImage->SetDirection(destDirection);
  // propagate vector length info
  destImage->SetNumberOfComponentsPerPixel( srcImage->GetNumberOfComponentsPerPixel() );
}


/** \class ImageInformationCopier
 * \brief A Function object used to copy image meta-data of an image.
 *
 * This function objects dispatches to Specialized versions. When the
 * images match dimensionalality the ImageBase::CopyInformation method
 * is used. But for expanding dimensional of images, the
 * functor copies the common dimensions then fills the extra meta-data
 * with "identity" information.
 *
 * \ingroup ITKCommon
 */
template< unsigned int D1, unsigned int D2 >
class ImageInformationCopier
{
public:
  virtual void operator()(ImageBase< D1 > * destImage,
                          const ImageBase< D2 > * srcImage) const
  {
    typedef typename BinaryUnsignedIntDispatch< D1, D2 >::ComparisonType ComparisonType;
    ImageToImageFilterDefaultCopyInformation< D1, D2 >(
      ComparisonType(),
      destImage, srcImage);
  }

  virtual ~ImageInformationCopier() {}
};


} // end of namespace ImageToImageFilterDetail
} // end namespace itk

#endif
