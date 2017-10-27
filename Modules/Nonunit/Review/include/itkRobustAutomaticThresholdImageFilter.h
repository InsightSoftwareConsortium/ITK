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
#ifndef itkRobustAutomaticThresholdImageFilter_h
#define itkRobustAutomaticThresholdImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkRobustAutomaticThresholdCalculator.h"

namespace itk
{
/** \class RobustAutomaticThresholdImageFilter
 * \brief Threshold an image using robust automatic threshold selection (RATS) method.
 *
 * RobustAutomaticThresholdImageFilter takes two inputs: the image to be thresholded
 * and a image of gradient magnitude of that image.
 * The threshold is computed as the mean of the pixel values in the input image weighted
 * by the pixel values in the gradient image.
 * The threshold computed that way should be the mean pixel value where the intensity
 * change the most.
 *
 *
 * This code was contributed in the Insight Journal paper:
 * "Robust Automatic Threshold Selection"
 * by Lehmann G.
 * https://hdl.handle.net/1926/370
 * http://www.insight-journal.org/browse/publication/134
 *
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup IntensityImageFilters
 * \ingroup ITKReview
 */

template< typename TInputImage, typename TGradientImage = TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT RobustAutomaticThresholdImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard Self typedef */
  typedef RobustAutomaticThresholdImageFilter             Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(RobustAutomaticThresholdImageFilter, ImageToImageFilter);

  /** Standard image type within this class. */
  typedef TInputImage    InputImageType;
  typedef TGradientImage GradientImageType;

  /** Image pixel value typedef. */
  typedef typename TInputImage::PixelType    InputPixelType;
  typedef typename TOutputImage::PixelType   OutputPixelType;
  typedef typename TGradientImage::PixelType GradientPixelType;

  /** Image related typedefs. */
  typedef typename TInputImage::Pointer    InputImagePointer;
  typedef typename TOutputImage::Pointer   OutputImagePointer;
  typedef typename TGradientImage::Pointer GradientImagePointer;

  typedef typename TInputImage::SizeType    InputSizeType;
  typedef typename TInputImage::IndexType   InputIndexType;
  typedef typename TInputImage::RegionType  InputImageRegionType;
  typedef typename TOutputImage::SizeType   OutputSizeType;
  typedef typename TOutputImage::IndexType  OutputIndexType;
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  typedef RobustAutomaticThresholdCalculator< TInputImage, TGradientImage >
  CalculatorType;

  /** Image related typedefs. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

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

  /** Get the computed threshold. */
  itkGetConstMacro(Threshold, InputPixelType);

  itkSetMacro(Pow, double);
  itkGetConstMacro(Pow, double);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( OutputComparableCheck,
                   ( Concept::Comparable< OutputPixelType > ) );
  itkConceptMacro( OutputOStreamWritableCheck,
                   ( Concept::OStreamWritable< OutputPixelType > ) );
  // End concept checking
#endif

  /** Set the gradient image */
  void SetGradientImage(GradientImageType *input)
  {
    // Process object is not const-correct so the const casting is required.
    this->SetNthInput( 1, const_cast< GradientImageType * >( input ) );
  }

  /** Get the gradient image */
  GradientImageType * GetGradientImage()
  {
    return static_cast< GradientImageType * >( const_cast< DataObject * >( this->ProcessObject::GetInput(1) ) );
  }

  /** Set the input image */
  void SetInput1(TInputImage *input)
  {
    this->SetInput(input);
  }

  /** Set the gradient image */
  void SetInput2(GradientImageType *input)
  {
    this->SetGradientImage(input);
  }

protected:
  RobustAutomaticThresholdImageFilter();
  ~RobustAutomaticThresholdImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RobustAutomaticThresholdImageFilter);

  double          m_Pow;
  InputPixelType  m_Threshold;
  OutputPixelType m_InsideValue;
  OutputPixelType m_OutsideValue;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRobustAutomaticThresholdImageFilter.hxx"
#endif

#endif
