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
#ifndef itkHMinimaImageFilter_h
#define itkHMinimaImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class HMinimaImageFilter
 * \brief Suppress local minima whose depth below the baseline is less than h.
 *
 * HMinimaImageFilter suppresses local minima that are less than h
 * intensity units below the (local) background. This has the effect
 * of smoothing over the "low" parts of the noise in the image
 * without smoothing over large changes in intensity (region
 * boundaries). See the HMaximaImageFilter to suppress the local
 * maxima whose height is less than h intensity units above the (local)
 * background.
 *
 * If original image is subtracted from the output of
 * HMinimaImageFilter, the signicant "valleys" in the image can be
 * identified.  This is what the HConcaveImageFilter provides.
 *
 * This filter uses the GrayscaleGeodesicErodeImageFilter.  It
 * provides its own input as the "mask" input to the geodesic
 * dilation.  The "marker" image for the geodesic dilation is
 * the input image plus the height parameter h.
 *
 * Geodesic morphology and the H-Minima algorithm is described in
 * Chapter 6 of Pierre Soille's book "Morphological Image Analysis:
 * Principles and Applications", Second Edition, Springer, 2003.
 *
 * \sa GrayscaleGeodesicDilateImageFilter, HMinimaImageFilter, HConvexImageFilter
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT HMinimaImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef HMinimaImageFilter                              Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                            InputImageType;
  typedef typename InputImageType::Pointer       InputImagePointer;
  typedef typename InputImageType::ConstPointer  InputImageConstPointer;
  typedef typename InputImageType::RegionType    InputImageRegionType;
  typedef typename InputImageType::PixelType     InputImagePixelType;
  typedef TOutputImage                           OutputImageType;
  typedef typename OutputImageType::Pointer      OutputImagePointer;
  typedef typename OutputImageType::ConstPointer OutputImageConstPointer;
  typedef typename OutputImageType::RegionType   OutputImageRegionType;
  typedef typename OutputImageType::PixelType    OutputImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(HMinimaImageFilter,
               ImageToImageFilter);

  /** Set/Get the height that a local maximum must be above the local
   * background (local contrast) in order to survive the
   * processing. Local maxima below this value are replaced with an
   * estimate of the local background. */
  itkSetMacro(Height, InputImagePixelType);
  itkGetConstMacro(Height, InputImagePixelType);

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
  itkConceptMacro( InputEqualityComparableCheck,
                   ( Concept::EqualityComparable< InputImagePixelType > ) );
  itkConceptMacro( IntConvertibleToInputCheck,
                   ( Concept::Convertible< int, InputImagePixelType > ) );
  itkConceptMacro( InputOStreamWritableCheck,
                   ( Concept::OStreamWritable< InputImagePixelType > ) );
  // End concept checking
#endif

protected:
  HMinimaImageFilter();
  ~HMinimaImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** HMinimaImageFilter needs the entire input be
   * available. Thus, it needs to provide an implementation of
   * GenerateInputRequestedRegion(). */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** HMinimaImageFilter will produce the entire output. */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) ) ITK_OVERRIDE;

  /** Single-threaded version of GenerateData.  This filter delegates
   * to GrayscaleGeodesicErodeImageFilter. */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(HMinimaImageFilter);

  InputImagePixelType m_Height;
  unsigned long       m_NumberOfIterationsUsed;
  bool                m_FullyConnected;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHMinimaImageFilter.hxx"
#endif

#endif
