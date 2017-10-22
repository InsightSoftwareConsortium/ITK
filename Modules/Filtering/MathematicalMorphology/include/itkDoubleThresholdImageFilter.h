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
#ifndef itkDoubleThresholdImageFilter_h
#define itkDoubleThresholdImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class DoubleThresholdImageFilter
 * \brief Binarize an input image using double thresholding.
 *
 * Double threshold addresses the difficulty in selecting a threshold
 * that will select the objects of interest without selecting
 * extraneous objects. Double threshold considers two threshold
 * ranges: a narrow range and a wide range (where the wide range
 * encompasses the narrow range). If the wide range was used for a
 * traditional threshold (where values inside the range map to the
 * foreground and values outside the range map to the background),
 * many extraneous pixels may survive the threshold operation. If the
 * narrow range was used for a traditional threshold, then too few
 * pixels may survive the threshold.
 *
 * Double threshold uses the narrow threshold image as a marker image
 * and the wide threshold image as a mask image in the geodesic
 * dilation. Essentially, the marker image (narrow threshold) is
 * dilated but constrained to lie within the mask image (wide
 * threshold). Thus, only the objects of interest (those pixels that
 * survived the narrow threshold) are extracted but the those objects
 * appear in the final image as they would have if the wide threshold
 * was used.
 *
 * \sa GrayscaleGeodesicDilateImageFilter
 * \sa MorphologyImageFilter, GrayscaleDilateImageFilter, GrayscaleFunctionDilateImageFilter, BinaryDilateImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT DoubleThresholdImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef DoubleThresholdImageFilter                      Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DoubleThresholdImageFilter, ImageToImageFilter);

  /** Types from the superclass */
  typedef typename Superclass::InputImagePointer InputImagePointer;
  typedef typename Superclass::InputImageType    InputImageType;

  /** Pixel types. */
  typedef typename TInputImage::PixelType  InputPixelType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  /** Set the "outside" pixel value. The default value
   * NumericTraits<OutputPixelType>::ZeroValue(). */
  itkSetMacro(OutsideValue, OutputPixelType);

  /** Get the "outside" pixel value. */
  itkGetConstMacro(OutsideValue, OutputPixelType);

  /** Set the "inside" pixel value. The default value
   * NumericTraits<OutputPixelType>::max() */
  itkSetMacro(InsideValue, OutputPixelType);

  /** Get the "inside" pixel value. */
  itkGetConstMacro(InsideValue, OutputPixelType);

  /** Set the thresholds. Four thresholds should be specified.  The
   * two lower thresholds default to
   * NumericTraits<InputPixelType>::NonpositiveMin(). The two upper
   * thresholds default NumericTraits<InputPixelType>::max.
   * Threshold1 <= Threshold2 <= Threshold3 <= Threshold4. */
  itkSetMacro(Threshold1, InputPixelType);
  itkSetMacro(Threshold2, InputPixelType);
  itkSetMacro(Threshold3, InputPixelType);
  itkSetMacro(Threshold4, InputPixelType);

  /** Get the threshold values. */
  itkGetConstMacro(Threshold1, InputPixelType);
  itkGetConstMacro(Threshold2, InputPixelType);
  itkGetConstMacro(Threshold3, InputPixelType);
  itkGetConstMacro(Threshold4, InputPixelType);

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
  itkConceptMacro( OutputEqualityComparableCheck,
                   ( Concept::EqualityComparable< OutputPixelType > ) );
  itkConceptMacro( InputComparableCheck,
                   ( Concept::Comparable< InputPixelType > ) );
  itkConceptMacro( InputOStreamWritableCheck,
                   ( Concept::OStreamWritable< InputPixelType > ) );
  itkConceptMacro( OutputOStreamWritableCheck,
                   ( Concept::OStreamWritable< OutputPixelType > ) );
  // End concept checking
#endif

protected:
  DoubleThresholdImageFilter();
  virtual ~DoubleThresholdImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** DoubleThresholdImageFilter needs all of the input. So it must
   * provide an implementation of GenerateInputRequestedRegion() */
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** DoubleThresholdImageFilter produces all of the output and must
   * provide an implementation of EnlargeOutputRequestedRegion() */
  void EnlargeOutputRequestedRegion( DataObject *itkNotUsed(output) ) ITK_OVERRIDE;

  /** Single threaded version of
   * GenerateData(). DoubleThresholdImageFilter delegates its
   * implementation to the GrayscaleGeodesicDilateImageFilter. */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DoubleThresholdImageFilter);

  InputPixelType m_Threshold1;
  InputPixelType m_Threshold2;
  InputPixelType m_Threshold3;
  InputPixelType m_Threshold4;

  OutputPixelType m_InsideValue;
  OutputPixelType m_OutsideValue;

  unsigned long m_NumberOfIterationsUsed;

  bool m_FullyConnected;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDoubleThresholdImageFilter.hxx"
#endif

#endif
