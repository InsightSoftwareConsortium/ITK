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
#ifndef itkImageAlgorithm_h
#define itkImageAlgorithm_h

#include "itkImageRegionIterator.h"

#include <type_traits>

namespace itk
{

template <typename TPixelType, unsigned int VImageDimension>
class VectorImage;


/** \class ImageAlgorithm
 *  \brief A container of static functions which can operate on Images
 *  with Iterators.
 *
 *  These methods are modeled after the STL algorithms. They may use
 *  special optimization techniques to implement enhanced versions of
 *  the methods.
 *
 *  \ingroup ITKCommon
 */
struct ImageAlgorithm
{

  using TrueType = std::true_type;
  using FalseType = std::false_type;

  /**
   * \brief This generic function copies a region from one image to
   * another. It may perform optimizations on the copy for efficiency.
   *
   * This method performs the equivalent to the following:
     \code
         itk::ImageRegionConstIterator<TInputImage> it( inImage, inRegion );
         itk::ImageRegionIterator<TOutputImage> ot( outImage, outRegion );

         while( !it.IsAtEnd() )
           {
           ot.Set( static_cast< typename TInputImage::PixelType >( it.Get() ) );
           ++ot;
           ++it;
           }
     \endcode
   *
   * \note: It is important not to explicitly pass the template
   * arguments to this method as it may not result in an optimized
   * method being called.
   */
  template <typename InputImageType, typename OutputImageType>
  static void
  Copy(const InputImageType *                       inImage,
       OutputImageType *                            outImage,
       const typename InputImageType::RegionType &  inRegion,
       const typename OutputImageType::RegionType & outRegion)
  {
    ImageAlgorithm::DispatchedCopy(inImage, outImage, inRegion, outRegion);
  }

  /// \cond HIDE_SPECIALIZATION_DOCUMENTATION
  template <typename TPixel1, typename TPixel2, unsigned int VImageDimension>
  static void
  Copy(const Image<TPixel1, VImageDimension> *                      inImage,
       Image<TPixel2, VImageDimension> *                            outImage,
       const typename Image<TPixel1, VImageDimension>::RegionType & inRegion,
       const typename Image<TPixel2, VImageDimension>::RegionType & outRegion)
  {
    using _ImageType1 = Image<TPixel1, VImageDimension>;
    using _ImageType2 = Image<TPixel2, VImageDimension>;
    ImageAlgorithm::DispatchedCopy(
      inImage,
      outImage,
      inRegion,
      outRegion,
      std::is_convertible<typename _ImageType1::PixelType, typename _ImageType2::PixelType>());
  }

  template <typename TPixel1, typename TPixel2, unsigned int VImageDimension>
  static void
  Copy(const VectorImage<TPixel1, VImageDimension> *                      inImage,
       VectorImage<TPixel2, VImageDimension> *                            outImage,
       const typename VectorImage<TPixel1, VImageDimension>::RegionType & inRegion,
       const typename VectorImage<TPixel2, VImageDimension>::RegionType & outRegion)
  {
    using _ImageType1 = VectorImage<TPixel1, VImageDimension>;
    using _ImageType2 = VectorImage<TPixel2, VImageDimension>;
    ImageAlgorithm::DispatchedCopy(
      inImage,
      outImage,
      inRegion,
      outRegion,
      std::is_convertible<typename _ImageType1::PixelType, typename _ImageType2::PixelType>());
  }

  /// \endcond

  /**
   * \brief Sets the output region to the smallest
   * region of the output image that fully contains
   * the physical space covered by the input
   * region of the input image
   */
  template <typename InputImageType, typename OutputImageType>
  static typename OutputImageType::RegionType
  EnlargeRegionOverBox(const typename InputImageType::RegionType & inputRegion,
                       const InputImageType *                      inputImage,
                       const OutputImageType *                     outputImage);

  template <typename InputImageType, typename OutputImageType, typename TransformType>
  static typename OutputImageType::RegionType
  EnlargeRegionOverBox(const typename InputImageType::RegionType & inputRegion,
                       const InputImageType *                      inputImage,
                       const OutputImageType *                     outputImage,
                       const TransformType *                       transform);

private:
  /** This is an optimized method which requires the input and
   * output images to be the same, and the pixel being POD (Plain Old
   * Data).
   */
  template <typename InputImageType, typename OutputImageType>
  static void
  DispatchedCopy(const InputImageType *                       inImage,
                 OutputImageType *                            outImage,
                 const typename InputImageType::RegionType &  inRegion,
                 const typename OutputImageType::RegionType & outRegion,
                 TrueType                                     isSpecialized);

  /** this is the reference image iterator implementation */
  template <typename InputImageType, typename OutputImageType>
  static void
  DispatchedCopy(const InputImageType *                       inImage,
                 OutputImageType *                            outImage,
                 const typename InputImageType::RegionType &  inRegion,
                 const typename OutputImageType::RegionType & outRegion,
                 FalseType                                    isSpecialized = FalseType());


  /** A utility class to get the number of internal pixels to make up
   * a pixel.
   */
  template <typename TImageType>
  struct PixelSize
  {
    static size_t
    Get(const TImageType *)
    {
      return 1;
    }
  };

  /// \cond HIDE_SPECIALIZATION_DOCUMENTATION
  template <typename TPixelType, unsigned int VImageDimension>
  struct PixelSize<VectorImage<TPixelType, VImageDimension>>
  {
    using ImageType = VectorImage<TPixelType, VImageDimension>;
    static size_t
    Get(const ImageType * i)
    {
      const size_t vectorLength = ImageType::AccessorFunctorType::GetVectorLength(i);
      return vectorLength;
    }
  };
  /// \endcond

  /** Unary functor just for static_cast operator */
  template <typename TInputType, typename TOutputType>
  struct StaticCast
  {
    TOutputType
    operator()(const TInputType i)
    {
      return static_cast<TOutputType>(i);
    }
  };


  /** Function to dispatch to std::copy or std::transform. */
  template <typename TType>
  static TType *
  CopyHelper(const TType * first, const TType * last, TType * result)
  {
    // Note: On some MS compilers the following may generate a
    // warning. Please include itkMacro.h before <algorithm> or
    // another stl header to avoid.
    return std::copy(first, last, result);
  }

  /// \cond HIDE_SPECIALIZATION_DOCUMENTATION
  template <typename TInputType, typename TOutputType>
  static TOutputType *
  CopyHelper(const TInputType * first, const TInputType * last, TOutputType * result)
  {
    return std::transform(first, last, result, StaticCast<TInputType, TOutputType>());
  }
  /// \endcond
};
} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageAlgorithm.hxx"
#endif


#endif // itkImageAlgorithm_h
