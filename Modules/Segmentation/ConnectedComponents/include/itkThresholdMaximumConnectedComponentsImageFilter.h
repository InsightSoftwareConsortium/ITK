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
#ifndef itkThresholdMaximumConnectedComponentsImageFilter_h
#define itkThresholdMaximumConnectedComponentsImageFilter_h

#include "itkBinaryThresholdImageFilter.h"
#include "itkConnectedComponentImageFilter.h"
#include "itkRelabelComponentImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkCastImageFilter.h"


namespace itk
{
/**\class ThresholdMaximumConnectedComponentsImageFilter
 * \brief Finds the threshold value of an image based on  maximizing the number
 * of objects in the image that are larger than a given minimal size.
 *
 * \par
 * This method is based on Topological Stable State Thresholding to
 * calculate the threshold set point. This method is particularly
 * effective when there are a large number of objects in a microscopy
 * image. Compiling in Debug mode and enable the debug flag for this
 * filter to print debug information to see how the filter focuses in
 * on a threshold value. Please see the Insight Journal's MICCAI 2005
 * workshop for a complete description. References are below.
 *
 * \par Parameters
 * The MinimumObjectSizeInPixels parameter is controlled through the class
 * Get/SetMinimumObjectSizeInPixels() method. Similar to the standard
 * itk::BinaryThresholdImageFilter the Get/SetInside and Get/SetOutside values
 * of the threshold can be set. The GetNumberOfObjects() and
 * GetThresholdValue() methods return the number of objects above the
 * minimum pixel size and the calculated threshold value.
 *
 * \par Automatic Thresholding in ITK
 * There are multiple methods to automatically calculate the threshold
 * intensity value of an image. As of version 4.0, ITK has a
 * Thresholding ( ITKThresholding ) module which contains numerous
 * automatic thresholding methods.implements two of these. Topological
 * Stable State Thresholding works well on images with a large number
 * of objects to be counted.
 *
 * \par References:
 * 1) Urish KL, August J, Huard J. "Unsupervised segmentation for myofiber
 * counting in immunoflourescent images". Insight Journal.
 * ISC/NA-MIC/MICCAI Workshop on Open-Source Software (2005)
 * Dspace handle: https://hdl.handle.net/1926/48
 * 2) Pikaz A, Averbuch, A. "Digital image thresholding based on topological
 * stable-state". Pattern Recognition, 29(5): 829-843, 1996.
 *
 * \par
 * Questions: email Ken Urish at ken.urish(at)gmail.com
 * Please cc the itk list serve for archival purposes.
 *
 * \ingroup ITKConnectedComponents
 */
template< typename TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT ThresholdMaximumConnectedComponentsImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ThresholdMaximumConnectedComponentsImageFilter  Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThresholdMaximumConnectedComponentsImageFilter,
               ImageToImageFilter);

  /** Typedef to describe the type of pixel. */
  typedef typename TInputImage::PixelType  PixelType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  /** The pixel type must support comparison operators. */
  itkConceptMacro( PixelTypeComparable, ( Concept::Comparable< PixelType > ) );

  /**
   * Set the minimum pixel area used to count objects on the
   * image. Thus, only objects that have a pixel area greater than the
   * minimum pixel area will be counted as an object in the
   * optimization portion of this filter. Essentially, it eliminates
   * noise from being counted as an object. The default value is
   * zero. */
  itkSetMacro(MinimumObjectSizeInPixels, unsigned int);
  itkGetConstMacro(MinimumObjectSizeInPixels, unsigned int);

  /**
   * The following Set/Get methods are for the binary threshold
   * function. This class automatically calculates the lower threshold
   * boundary. The upper threshold boundary, inside value, and outside
   * value can be defined   by the user, however the standard values
   * are used as default if not set by the user.
   *  The default value of the: Inside value is the maximum pixel type
   *                              intensity.
   *                            Outside value is the minimum pixel
   *                              type intensity.
   *                            Upper threshold boundary is the
   *                              maximum pixel type intensity.
   */
  itkSetMacro(InsideValue, OutputPixelType);
  itkSetMacro(OutsideValue, OutputPixelType);
  itkSetMacro(UpperBoundary, PixelType);
  itkGetConstMacro(InsideValue, OutputPixelType);
  itkGetConstMacro(OutsideValue, OutputPixelType);
  itkGetConstMacro(UpperBoundary, PixelType);

  /**
   * Returns the number of objects in the image. This information is
   * only valid after the filter has executed. Useful if your counting
   * something */
  itkGetConstMacro(NumberOfObjects, SizeValueType);

  /**
   * Returns the automatic threshold setpoint. This information is
   * only valid after the filter has executed. */
  itkGetConstMacro(ThresholdValue, PixelType);

  /** Some additional typedefs.  */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::ConstPointer InputImagePointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  /** Some additional typedefs.  */
  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::Pointer    OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType  OutputImagePixelType;

protected:
  ThresholdMaximumConnectedComponentsImageFilter();
  ~ThresholdMaximumConnectedComponentsImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void GenerateData(void) ITK_OVERRIDE;

  /**
   * Runs a series of filters that thresholds the image,
   * dilates/erodes  for edge enhancement, and counts the number of
   * relabeled connected components */
  SizeValueType ComputeConnectedComponents();

private:

  /** Typedef for filter pixel type.  */
  typedef unsigned int FilterPixelType;

  itkStaticConstMacro(ImageDimension,
                      unsigned int,
                      TInputImage::ImageDimension);

  typedef itk::Image< FilterPixelType, itkGetStaticConstMacro(ImageDimension) >
  FilterImageType;

  typedef typename FilterImageType::Pointer FilterImagePointer;

  ITK_DISALLOW_COPY_AND_ASSIGN(ThresholdMaximumConnectedComponentsImageFilter);

  //
  // Binary Threshold Filter
  //
  typedef BinaryThresholdImageFilter< InputImageType, OutputImageType >
  ThresholdFilterType;

  //
  // Connected Components Filter
  //
  typedef ConnectedComponentImageFilter< OutputImageType, FilterImageType >
  ConnectedFilterType;

  //
  // Relabeled Components Filter
  //
  typedef RelabelComponentImageFilter< FilterImageType, FilterImageType >
  RelabelFilterType;

  //
  // Minimum maximum calculator
  //
  typedef MinimumMaximumImageCalculator< InputImageType > MinMaxCalculatorType;

  //
  // Declare member variables for the filters of the internal pipeline.
  //
  typename ThresholdFilterType::Pointer m_ThresholdFilter;
  typename ConnectedFilterType::Pointer m_ConnectedComponent;

  typename RelabelFilterType::Pointer m_LabeledComponent;

  typename MinMaxCalculatorType::Pointer m_MinMaxCalculator;

  // Variables defined by the user
  unsigned int m_MinimumObjectSizeInPixels;

  // Binary threshold variables
  OutputPixelType m_OutsideValue;
  OutputPixelType m_InsideValue;

  PixelType m_LowerBoundary;
  PixelType m_UpperBoundary;

  // Filter variables
  PixelType     m_ThresholdValue;
  SizeValueType m_NumberOfObjects;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkThresholdMaximumConnectedComponentsImageFilter.hxx"
#endif

#endif
