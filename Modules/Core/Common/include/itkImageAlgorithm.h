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
#ifndef __itkImageAlgorithm_h
#define __itkImageAlgorithm_h

#include "itkImageRegionIterator.h"

#ifdef ITK_HAS_STLTR1_TR1_TYPE_TRAITS
#  include <tr1/type_traits>
#elif defined ITK_HAS_STLTR1_TYPE_TRAITS
#  include <type_traits>
#else
#  include "itkIsSame.h"
#endif

namespace itk
{

template <typename TPixelType, unsigned int VImageDimension > class VectorImage;


/** \class ImageAlgorithm
 * \brief A container of static functions which can operate on Images
 * with Iterators.
 *
 * These methods are modeled after the STL algorithms. They may use
 * special optimization techniques to implement enhanced versions of
 * the methods.
 *
 * \ingroup ITKCommon
 */
struct ImageAlgorithm
{

#if defined(ITK_HAS_STLTR1_TR1_TYPE_TRAITS) || defined(ITK_HAS_STLTR1_TYPE_TRAITS)
  typedef std::tr1::true_type  TrueType;
  typedef std::tr1::false_type FalseType;
#else
  typedef itk::TrueType  TrueType;
  typedef itk::FalseType FalseType;
#endif

/**
 * \brief This generic function copies a region from one image to
 * another. It may perform optimizations on the copy for efficiency.
 *
 * This method performs the equivalent to the following:
 * \code
 *     itk::ImageRegionConstIterator<TInputImage> it( inImage, inRegion );
 *     itk::ImageRegionIterator<TOutputImage> ot( outImage, outRegion );
 *
 *     while( !it.IsAtEnd() )
 *       {
 *       ot.Set( static_cast< typename TInputImage::PixelType >( it.Get() ) );
 *       ++ot;
 *       ++it;
 *       }
 * \endcode
 *
 * \note: It is important not to explicitly pass the template
 * arguments to this method as it may not result in an optimized
 * method being called.
 */
  template<typename InputImageType, typename OutputImageType>
  static void Copy( const InputImageType *inImage, OutputImageType *outImage,
                    const typename InputImageType::RegionType &inRegion,
                    const typename OutputImageType::RegionType &outRegion )
  {
    ImageAlgorithm::DispatchedCopy( inImage, outImage, inRegion, outRegion );
  }

/** \cond HIDE_SPECIALIZATION_DOCUMENTATION */
  template<class TPixel, unsigned int VImageDimension>
  static void Copy( const Image<TPixel, VImageDimension> * inImage,
                               Image<TPixel, VImageDimension> * outImage,
                               const typename Image<TPixel, VImageDimension>::RegionType &inRegion,
                               const typename Image<TPixel, VImageDimension>::RegionType &outRegion )
  {
    typedef Image<TPixel, VImageDimension> _ImageType;
    ImageAlgorithm::DispatchedCopy( inImage, outImage, inRegion, outRegion
#if defined(ITK_HAS_STLTR1_TR1_TYPE_TRAITS) || defined(ITK_HAS_STLTR1_TYPE_TRAITS)
                                    , std::tr1::is_pod<typename _ImageType::InternalPixelType>()
#endif
      );
  }

  template<class TPixel, unsigned int VImageDimension>
  static void Copy( const VectorImage<TPixel, VImageDimension> * inImage,
                               VectorImage<TPixel, VImageDimension> * outImage,
                               const typename VectorImage<TPixel, VImageDimension>::RegionType &inRegion,
                               const typename VectorImage<TPixel, VImageDimension>::RegionType &outRegion )
  {
    typedef Image<TPixel, VImageDimension> _ImageType;
    ImageAlgorithm::DispatchedCopy( inImage, outImage, inRegion, outRegion
#if defined(ITK_HAS_STLTR1_TR1_TYPE_TRAITS) || defined(ITK_HAS_STLTR1_TYPE_TRAITS)
                                    , std::tr1::is_pod<typename _ImageType::InternalPixelType>()
#endif
      );
  }

/** \endcond */


private:

  /** This is an optimized method which requires the input and
   * output images to be the same, and the pixel being POD (Plain Old
   * Data).
   */
  template<typename ImageType>
  static void DispatchedCopy( const ImageType *inImage, ImageType *outImage,
                              const typename ImageType::RegionType &inRegion,
                              const typename ImageType::RegionType &outRegion, TrueType isPod );

  /** this is the reference image iterator implementation */
  template<typename InputImageType, typename OutputImageType>
  static void DispatchedCopy( const InputImageType *inImage, OutputImageType *outImage,
                              const typename InputImageType::RegionType &inRegion,
                              const typename OutputImageType::RegionType &outRegion, FalseType isPod = FalseType() );


  /** A utility class which is used to obtain the number of bytes used
   * per-pixel on an image.
   */
  template <typename TImageType>
  struct PixelSize
  {
    static size_t Get( const TImageType *)
    {
      return sizeof( typename TImageType::PixelType );
    }
  };

/** \cond HIDE_SPECIALIZATION_DOCUMENTATION */
  template <typename TPixelType, unsigned int VImageDimension>
  struct PixelSize< VectorImage<TPixelType, VImageDimension> >
  {
    typedef VectorImage<TPixelType, VImageDimension> ImageType;
    static size_t Get( const  ImageType * i )
    {
      const size_t vectorLength = ImageType::AccessorFunctorType::GetVectorLength(i);
      const size_t componentSize = sizeof(TPixelType);
      return vectorLength*componentSize;
    }
  };
/** \endcond */

};
} // end namespace itk


#if ITK_TEMPLATE_TXX
#include "itkImageAlgorithm.hxx"
#endif


#endif //__itkImageAlgorithm_h
