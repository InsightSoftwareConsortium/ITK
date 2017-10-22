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
#ifndef itkIsolatedConnectedImageFilter_h
#define itkIsolatedConnectedImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class IsolatedConnectedImageFilter
 * \brief Label pixels that are connected to one set of seeds but not
 * another.
 *
 * IsolatedConnectedImageFilter finds the optimal threshold to
 * separate two regions.  It has two modes, one to separate dark
 * regions surrounded by bright regions by automatically finding a
 * minimum isolating upper threshold, and another to separate bright
 * regions surrounded by dark regions by automatically finding a
 * maximum lower isolating threshold.  The mode can be chosen by
 * setting FindUpperThresholdOn()/Off().  In both cases, the isolating
 * threshold is retrieved with GetIsolatedValue().
 *
 * The algorithm labels pixels with ReplaceValue that are connected to
 * Seeds1 AND NOT connected to Seeds2.  When finding the threshold to
 * separate two dark regions surrounded by bright regions, given a
 * fixed lower threshold, the filter adjusts the upper threshold until
 * the two sets of seeds are not connected. The algorithm uses a
 * binary search to adjust the upper threshold, starting at Upper. The
 * reverse is true for finding the threshold to separate two bright
 * regions.  Lower defaults to the smallest possible value for the
 * InputImagePixelType, and Upper defaults to the largest possible
 * value for the InputImagePixelType.
 *
 * The user can also supply the Lower and Upper values to restrict the
 * search.  However, if the range is too restrictive, it could happen
 * that no isolating threshold can be found between the user specified
 * Lower and Upper values.  Therefore, unless the user is sure of the
 * bounds to set, it is recommended that the user set these values to
 * the lowest and highest intensity values in the image, respectively.
 *
 * The user can specify more than one seed for both regions to
 * separate.  The algorithm will try find the threshold that ensures
 * that all of the first seeds are contained in the resulting
 * segmentation and all of the second seeds are not contained in the
 * segmentation.
 *
 * It is possible that the algorithm may not be able to find the
 * isolating threshold because no such threshold exists.  The user can
 * check for this by querying the GetThresholdingFailed() flag.
 *
 *
 * \ingroup RegionGrowingSegmentation
 * \ingroup ITKRegionGrowing
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT IsolatedConnectedImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef IsolatedConnectedImageFilter                    Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods).  */
  itkTypeMacro(IsolatedConnectedImageFilter,
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

  typedef std::vector< IndexType > SeedsContainerType;

  typedef typename NumericTraits<
    InputImagePixelType >::RealType InputRealType;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Add seed point 1. This seed will be isolated from Seed2 (if possible).
   *  All pixels connected to this seed will be replaced with ReplaceValue. */
  void AddSeed1(const IndexType & seed);

#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
  /** \deprecated
   * Set seed point 1. This seed will be isolated from Seed2 (if possible).
   *  All pixels connected to this seed will be replaced with ReplaceValue.
   *  This method is deprecated, please use AddSeed1(). */
  void SetSeed1(const IndexType & seed);
#endif

  /** Clear all the seeds1. */
  void ClearSeeds1();

  /** Add seed point 2. This seed will be isolated from Seed1 (if possible). */
  void AddSeed2(const IndexType & seed);

#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
  /** \deprecated
   * Set seed point 2. This seed will be isolated from Seed1 (if possible).
   *  This method is deprecated, please use AddSeed2(). */
  void SetSeed2(const IndexType & seed);
#endif

  /** Clear all the seeds2. */
  void ClearSeeds2();

  /** Method to access seed container */
  virtual const SeedsContainerType &GetSeeds1() const;
  virtual const SeedsContainerType &GetSeeds2() const;

  /** Set/Get the limit on the lower threshold value. The default is
   * the NonpositiveMin() for the InputPixelType. */
  itkSetMacro(Lower, InputImagePixelType);
  itkGetConstReferenceMacro(Lower, InputImagePixelType);

  /** Set/Get the limit on the upper threshold value. The default is
   * the max() for the InputPixelType. */
  itkSetMacro(Upper, InputImagePixelType);
  itkGetConstReferenceMacro(Upper, InputImagePixelType);

#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
  /** \deprecated
   * Set/Get the limit on the upper threshold value. The default is
      the max() for the InputPixelType.  These methods have been
      deprecated.  Please use Set/Get Upper instead. */
  void SetUpperValueLimit(InputImagePixelType upperValue)
  {
    this->SetUpper(upperValue);
  }

  InputImagePixelType GetUpperValueLimit()
  {
    return this->GetUpper();
  }
#endif

  /** Set/Get the precision required for the intensity threshold
   * value. The default is 1. */
  itkSetMacro(IsolatedValueTolerance, InputImagePixelType);
  itkGetConstReferenceMacro(IsolatedValueTolerance, InputImagePixelType);

  /** Set/Get value to replace thresholded pixels. Pixels that lie
   *  within the thresholds will be replaced with this value. The
   *  default is 1. */
  itkSetMacro(ReplaceValue, OutputImagePixelType);
  itkGetConstReferenceMacro(ReplaceValue, OutputImagePixelType);

  /** Get value that isolates the two seeds. */
  itkGetConstReferenceMacro(IsolatedValue, InputImagePixelType);

  /** Set/Get whether to find an upper threshold (separating two dark
   * regions) or a lower threshold (separating two bright regions). */
  itkSetMacro(FindUpperThreshold, bool);
  itkBooleanMacro(FindUpperThreshold);
  itkGetConstReferenceMacro(FindUpperThreshold, bool);

  /** Get the flag that tells whether the algorithm failed to find a
   * threshold. */
  itkGetConstReferenceMacro(ThresholdingFailed, bool);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputImagePixelType > ) );
  // End concept checking
#endif

protected:
  IsolatedConnectedImageFilter();
  ~IsolatedConnectedImageFilter() ITK_OVERRIDE {}
  SeedsContainerType m_Seeds1;
  SeedsContainerType m_Seeds2;

  InputImagePixelType m_Lower;
  InputImagePixelType m_Upper;

  OutputImagePixelType m_ReplaceValue;

  InputImagePixelType m_IsolatedValue;
  InputImagePixelType m_IsolatedValueTolerance;

  bool m_FindUpperThreshold;
  bool m_ThresholdingFailed;

  // Override since the filter needs all the data for the algorithm
  void GenerateInputRequestedRegion() ITK_OVERRIDE;

  // Override since the filter produces the entire dataset
  void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(IsolatedConnectedImageFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkIsolatedConnectedImageFilter.hxx"
#endif

#endif
