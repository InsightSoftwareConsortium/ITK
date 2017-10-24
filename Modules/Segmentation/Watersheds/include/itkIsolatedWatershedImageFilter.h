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
#ifndef itkIsolatedWatershedImageFilter_h
#define itkIsolatedWatershedImageFilter_h

#include "itkWatershedImageFilter.h"
#include "itkGradientMagnitudeImageFilter.h"

namespace itk
{
/** \class IsolatedWatershedImageFilter
 * \brief Isolate watershed basins using two seeds
 *
 * IsolatedWatershedImageFilter labels pixels with ReplaceValue1 that
 * are in the same watershed basin as Seed1 AND NOT the same as
 * Seed2. The filter adjusts the waterlevel until the two seeds are
 * not in different basins. The user supplies a Watershed
 * threshold. The algorithm uses a binary search to adjust the upper
 * waterlevel, starting at UpperValueLimit. UpperValueLimit defaults
 * to the 1.0.
 * \ingroup WatershedSegmentation
 * \ingroup ITKWatersheds
 */

template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT IsolatedWatershedImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef IsolatedWatershedImageFilter                    Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods).  */
  itkTypeMacro(IsolatedWatershedImageFilter,
               ImageToImageFilter);

  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;
  typedef typename InputImageType::IndexType    IndexType;
  typedef typename InputImageType::SizeType     SizeType;

  typedef TOutputImage                         OutputImageType;
  typedef typename OutputImageType::Pointer    OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename OutputImageType::PixelType  OutputImagePixelType;

  typedef typename NumericTraits<InputImagePixelType>::RealType      RealPixelType;
  typedef Image<RealPixelType, TInputImage::ImageDimension>          RealImageType;

  typedef WatershedImageFilter< RealImageType >                          WatershedType;
  typedef GradientMagnitudeImageFilter< InputImageType, RealImageType > GradientMagnitudeType;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Set seed point 1. This seed will be isolated from Seed2 (if
   *  possible). All pixels connected to this seed will be replaced
   *  with ReplaceValue1. */
  itkSetMacro(Seed1, IndexType);
  itkGetConstMacro(Seed1, IndexType);

  /** Set seed point 2. This seed will be isolated from Seed1 (if
   *  possible). All pixels connected to this seed will be replaced
   *  with ReplaceValue2. */
  itkSetMacro(Seed2, IndexType);
  itkGetConstMacro(Seed2, IndexType);

  /** Set/Get the Watershed threshold. The default is 0. */
  itkSetMacro(Threshold, double);
  itkGetConstMacro(Threshold, double);

  /** Set/Get the precision required for the intensity threshold
   *  value. The default is .001. */
  itkSetMacro(IsolatedValueTolerance, double);
  itkGetConstMacro(IsolatedValueTolerance, double);

  /** Set/Get the limit on the upper waterlevel value. The default is
   *  1.0. */
  itkSetMacro(UpperValueLimit, double);
  itkGetConstMacro(UpperValueLimit, double);

  /** Set/Get value to replace Seed1(Seed2) pixels, pixels that are
   *  within the basin that contains Seed1(Seed2) this  value. The
   *  default is 1(0). */
  itkSetMacro(ReplaceValue1, OutputImagePixelType);
  itkGetConstMacro(ReplaceValue1, OutputImagePixelType);
  itkSetMacro(ReplaceValue2, OutputImagePixelType);
  itkGetConstMacro(ReplaceValue2, OutputImagePixelType);

  /** Get value that isolates the two seeds. */
  itkGetConstMacro(IsolatedValue, double);

protected:
  IsolatedWatershedImageFilter();
  ~IsolatedWatershedImageFilter() ITK_OVERRIDE {}
  IndexType m_Seed1;
  IndexType m_Seed2;

  OutputImagePixelType m_ReplaceValue1;
  OutputImagePixelType m_ReplaceValue2;

  typename GradientMagnitudeType::Pointer m_GradientMagnitude;

  typename WatershedType::Pointer m_Watershed;

  double m_Threshold;
  double m_IsolatedValue;
  double m_IsolatedValueTolerance;
  double m_UpperValueLimit;

  // Override since the filter needs all the data for the algorithm
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  // Override since the filter produces the entire dataset
  void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

  void VerifyInputInformation() ITK_OVERRIDE;
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(IsolatedWatershedImageFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkIsolatedWatershedImageFilter.hxx"
#endif

#endif
