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
#ifndef itkGrayscaleGrindPeakImageFilter_h
#define itkGrayscaleGrindPeakImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class GrayscaleGrindPeakImageFilter
 * \brief Remove local maxima not connected to the boundary of the image.
 *
 * GrayscaleGrindPeakImageFilter removes peaks in a grayscale image.
 * Peaks are local maxima in the grayscale topography that are not
 * connected to boundaries of the image. Gray level values adjacent to
 * a peak are extrapolated through the peak.
 *
 * This filter is used to smooth over local maxima without affecting
 * the values of local minima.  If you take the difference between the
 * output of this filter and the original image (and perhaps threshold
 * the difference above a small value), you'll obtain a map of the
 * local maxima.
 *
 * This filter uses the GrayscaleGeodesicDilateImageFilter.  It
 * provides its own input as the "mask" input to the geodesic
 * erosion.  The "marker" image for the geodesic erosion is
 * constructed such that boundary pixels match the boundary pixels of
 * the input image and the interior pixels are set to the minimum
 * pixel value in the input image.
 *
 * This filter is the dual to the GrayscaleFillholeImageFilter which
 * implements the Fillhole algorithm.  Since it is a dual, it is
 * somewhat superfluous but is provided as a convenience.
 *
 * Geodesic morphology and the Fillhole algorithm is described in
 * Chapter 6 of Pierre Soille's book "Morphological Image Analysis:
 * Principles and Applications", Second Edition, Springer, 2003.
 *
 * \sa GrayscaleGeodesicDilateImageFilter
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT GrayscaleGrindPeakImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GrayscaleGrindPeakImageFilter);

  /** Standard class type aliases. */
  using Self = GrayscaleGrindPeakImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;

  /** ImageDimension constants */
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GrayscaleGrindPeakImageFilter, ImageToImageFilter);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(InputOStreamWritableCheck, (Concept::OStreamWritable<InputImagePixelType>));
  // End concept checking
#endif

protected:
  GrayscaleGrindPeakImageFilter();
  ~GrayscaleGrindPeakImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** GrayscaleGrindPeakImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** GrayscaleGrindPeakImageFilter will produce the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicDilateImageFilter. */
  void
  GenerateData() override;

private:
  unsigned long m_NumberOfIterationsUsed{ 1 };

  bool m_FullyConnected;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGrayscaleGrindPeakImageFilter.hxx"
#endif

#endif
