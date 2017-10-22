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
#ifndef itkApproximateSignedDistanceMapImageFilter_h
#define itkApproximateSignedDistanceMapImageFilter_h

#include "itkFastChamferDistanceImageFilter.h"
#include "itkIsoContourDistanceImageFilter.h"

namespace itk
{
/** \class ApproximateSignedDistanceMapImageFilter
 * \brief Create a map of the approximate signed distance from the boundaries of
 * a binary image.
 *
 * The ApproximateSignedDistanceMapImageFilter takes as input a binary image and
 * produces a signed distance map. Each pixel value in the output contains the
 * approximate distance from that pixel to the nearest "object" in the binary
 * image. This filter differs from the DanielssonDistanceMapImageFilter in that
 * it calculates the distance to the "object edge" for pixels within the object.
 *
 * Negative values in the output indicate that the pixel at that position is
 * within an object in the input image. The absolute value of a negative pixel
 * represents the approximate distance to the nearest object boundary pixel.
 *
 * WARNING: This filter requires that the output type be floating-point. Otherwise
 * internal calculations will not be performed to the appropriate precision,
 * resulting in completely incorrect (read: zero-valued) output.
 *
 * The distances computed by this filter are Chamfer distances, which are only
 * an approximation to Euclidian distances, and are not as exact approximations
 * as those calculated by the DanielssonDistanceMapImageFilter. On the other hand,
 * this filter is faster.
 *
 * This filter requires that an "inside value" and "outside value" be set as
 * parameters. The "inside value" is the intensity value of the binary image
 * which corresponds to objects, and the "outside value" is the intensity of the
 * background. (A typical binary image often represents objects as black (0) and
 * background as white (usually 255), or vice-versa.) Note that this filter is
 * slightly faster if the inside value is less than the outside value. Otherwise
 * an extra iteration through the image is required.
 *
 * This filter uses the FastChamferDistanceImageFilter and the
 * IsoContourDistanceImageFilter internally to perform the distance calculations.
 *
 * \sa DanielssonDistanceMapImageFilter
 * \sa SignedDanielssonDistanceMapImageFilter
 * \sa SignedMaurerDistanceMapImageFilter
 * \sa FastChamferDistanceImageFilter
 * \sa IsoContourDistanceImageFilter
 *
 * \author Zach Pincus
 * \ingroup ITKDistanceMap
 *
 * \wiki
 * \wikiexample{ImageProcessing/ApproximateSignedDistanceMapImageFilter, Compute a distance map from objects in a binary image}
 * \endwiki
 */

template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT ApproximateSignedDistanceMapImageFilter:public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard typedefs */
  typedef ApproximateSignedDistanceMapImageFilter         Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ApproximateSignedDistanceMapImageFilter, ImageToImageFilter);

  /** standard New() method support */
  itkNewMacro(Self);

  /** Type for input image. */
  typedef TInputImage InputImageType;

  /** Type for the output image. */
  typedef TOutputImage OutputImageType;

  /** Type for the pixels of the input image. */
  typedef typename InputImageType::PixelType InputPixelType;

  /** Type for the pixels of the output image. */
  typedef typename OutputImageType::PixelType OutputPixelType;

  /** Type of input image size and size value */
  typedef typename OutputImageType::SizeType     OutputSizeType;
  typedef typename OutputSizeType::SizeValueType OutputSizeValueType;
  /** The dimension of the input image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** Pointer Type for input image. */
  typedef typename InputImageType::ConstPointer InputImagePointer;

  /** Pointer Type for the output image. */
  typedef typename OutputImageType::Pointer OutputImagePointer;

  /** Set/Get intensity value representing the interior of objects in the mask.
    */
  itkSetMacro(InsideValue, InputPixelType);
  itkGetConstMacro(InsideValue, InputPixelType);

  /** Set/Get intensity value representing non-objects in the mask. */
  itkSetMacro(OutsideValue, InputPixelType);
  itkGetConstMacro(OutsideValue, InputPixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputEqualityComparableCheck,
                   ( Concept::EqualityComparable< typename InputImageType::PixelType > ) );
  // End concept checking
#endif

protected:
  ApproximateSignedDistanceMapImageFilter();
  virtual ~ApproximateSignedDistanceMapImageFilter() ITK_OVERRIDE {}
  virtual void GenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ApproximateSignedDistanceMapImageFilter);

  typedef IsoContourDistanceImageFilter< InputImageType, OutputImageType >   IsoContourType;
  typedef FastChamferDistanceImageFilter< OutputImageType, OutputImageType > ChamferType;
  typename IsoContourType::Pointer m_IsoContourFilter;

  typename ChamferType::Pointer m_ChamferFilter;

  InputPixelType m_InsideValue;
  InputPixelType m_OutsideValue;
};
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkApproximateSignedDistanceMapImageFilter.hxx"
#endif

#endif
