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
#ifndef itkCannyEdgeDetectionImageFilter_h
#define itkCannyEdgeDetectionImageFilter_h

#include "itkConstNeighborhoodIterator.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkMultiThreader.h"
#include "itkDerivativeOperator.h"
#include "itkSparseFieldLayer.h"
#include "itkObjectStore.h"
#include "itkMath.h"

namespace itk
{
template< typename TValue >
class ITK_TEMPLATE_EXPORT ListNode
{
public:
  TValue m_Value;

  ListNode *Next;
  ListNode *Previous;
};

/** \class CannyEdgeDetectionImageFilter
 * \brief This filter is an implementation of a Canny edge detector for
 * scalar-valued images.
 *
 *  Based on John Canny's paper "A Computational Approach
 * to Edge Detection"(IEEE Transactions on Pattern Analysis and Machine
 * Intelligence, Vol. PAMI-8, No.6, November 1986),  there are four major steps
 * used in the edge-detection scheme:
 * (1) Smooth the input image with Gaussian filter.
 * (2) Calculate the second directional derivatives of the smoothed image.
 * (3) Non-Maximum Suppression: the zero-crossings of 2nd derivative are found,
 *     and the sign of third derivative is used to find the correct extrema.
 * (4) The hysteresis thresholding is applied to the gradient magnitude
 *      (multiplied with zero-crossings) of the smoothed image to find and
 *      link edges.
 *
 * \par Inputs and Outputs
 * The input to this filter should be a scalar, real-valued Itk image of
 * arbitrary dimension.  The output should also be a scalar, real-value Itk
 * image of the same dimensionality.
 *
 * \par Parameters
 * There are four parameters for this filter that control the sub-filters used
 * by the algorithm.
 *
 * \par
 * Variance and Maximum error are used in the Gaussian smoothing of the input
 * image.  See  itkDiscreteGaussianImageFilter for information on these
 * parameters.
 *
 * \par
 * Threshold is the lowest allowed value in the output image.  Its data type is
 * the same as the data type of the output image. Any values below the
 * Threshold level will be replaced with the OutsideValue parameter value, whose
 * default is zero.
 *
 * \todo Edge-linking will be added when an itk connected component labeling
 * algorithm is available.
 *
 * \sa DiscreteGaussianImageFilter
 * \sa ZeroCrossingImageFilter
 * \sa ThresholdImageFilter
 * \ingroup ITKImageFeature
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT CannyEdgeDetectionImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard "Self" & Superclass typedef.  */
  typedef CannyEdgeDetectionImageFilter                   Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /** Image typedef support   */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** SmartPointer typedef support  */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Define pixel types. */
  typedef typename TInputImage::PixelType     InputImagePixelType;
  typedef typename TOutputImage::PixelType    OutputImagePixelType;
  typedef typename TInputImage::IndexType     IndexType;
  typedef typename TInputImage::SizeValueType SizeValueType;

  /** The default boundary condition is used unless overridden
   *in the Evaluate() method. */
  typedef ZeroFluxNeumannBoundaryCondition< OutputImageType >
  DefaultBoundaryConditionType;

  /** The type of data structure that is passed to this function object
   * to evaluate at a pixel that does not lie on a data set boundary.
   */
  typedef ConstNeighborhoodIterator< OutputImageType,
                                     DefaultBoundaryConditionType > NeighborhoodType;

  typedef ListNode< IndexType >            ListNodeType;
  typedef ObjectStore< ListNodeType >      ListNodeStorageType;
  typedef SparseFieldLayer< ListNodeType > ListType;
  typedef typename ListType::Pointer       ListPointerType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;
  typedef typename TInputImage::RegionType  InputImageRegionType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(CannyEdgeDetectionImageFilter, ImageToImageFilter);

  /** ImageDimension constant. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Typedef of double containers. */
  typedef FixedArray< double, itkGetStaticConstMacro(ImageDimension) > ArrayType;

  /** Set/Get the variance of the Gaussian smoothing filter. */
  itkSetMacro(Variance, ArrayType);
  itkGetConstMacro(Variance, const ArrayType);

  /** Set/Get the maximum error of the Gaussian smoothing kernel in each dimensional
   *  direction. */
  itkSetMacro(MaximumError, ArrayType);
  itkGetConstMacro(MaximumError, const ArrayType);

  /** Set/Get the variance of the Gaussian smoothing filter. */
  void SetVariance(const typename ArrayType::ValueType v)
  {
    for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
      {
      if ( Math::NotExactlyEquals(m_Variance[i], v) )
        {
        m_Variance.Fill(v);
        this->Modified();
        break;
        }
      }
  }

  /** Set/Get the MaximumError parameter used by the Gaussian smoothing filter
   *  in this algorithm */
  void SetMaximumError(const typename ArrayType::ValueType v)
  {
    for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
      {
      if ( Math::NotExactlyEquals(m_MaximumError[i], v) )
        {
        m_MaximumError.Fill(v);
        this->Modified();
        break;
        }
      }
  }

  /** \brief Set the Threshold value for detected edges.
   *
   * TODO:  Document in the ITKv4 migration guide that
   * the SetThreshold member function was removed from
   * the CannyEdgeDetectionImageFilter, and that both
   * UpperThreshold and LowerThreshold need to be set.
   * To get the same results as with the SetThreshold method
   * change "myfilter->SetThrehsold" to "myfilter->SetUpperThreshold",
   * and add "myfilter->SetLowerThreshold(GetUpperThreshold()/2.0)"
   */
  itkSetMacro(UpperThreshold, OutputImagePixelType);
  itkGetConstMacro(UpperThreshold, OutputImagePixelType);

  itkSetMacro(LowerThreshold, OutputImagePixelType);
  itkGetConstMacro(LowerThreshold, OutputImagePixelType);

  OutputImageType * GetNonMaximumSuppressionImage()
  {
    return this->m_MultiplyImageFilter->GetOutput();
  }

  /** CannyEdgeDetectionImageFilter needs a larger input requested
   * region than the output requested region ( derivative operators, etc).
   * As such, CannyEdgeDetectionImageFilter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()  */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< InputImagePixelType > ) );
  itkConceptMacro( OutputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< OutputImagePixelType > ) );
  itkConceptMacro( SameDimensionCheck,
                   ( Concept::SameDimension< ImageDimension, OutputImageDimension > ) );
  itkConceptMacro( InputIsFloatingPointCheck,
                   ( Concept::IsFloatingPoint< InputImagePixelType > ) );
  itkConceptMacro( OutputIsFloatingPointCheck,
                   ( Concept::IsFloatingPoint< OutputImagePixelType > ) );
  // End concept checking
#endif

protected:
  CannyEdgeDetectionImageFilter();
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void GenerateData() ITK_OVERRIDE;

  typedef DiscreteGaussianImageFilter< InputImageType, OutputImageType >
  GaussianImageFilterType;
  typedef MultiplyImageFilter< OutputImageType,
                               OutputImageType, OutputImageType >       MultiplyImageFilterType;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CannyEdgeDetectionImageFilter);

  virtual ~CannyEdgeDetectionImageFilter() ITK_OVERRIDE {}

  /** Thread-Data structure. */
  struct CannyThreadStruct
  {
    CannyEdgeDetectionImageFilter *Filter;
  };

  /** Allocate storage for update buffers used during calculation of multiple steps. */
  void AllocateUpdateBuffer();

  /** Implement hysteresis thresholding. */
  void HysteresisThresholding();

  /** Edge linking function. */
  void FollowEdge(IndexType index, const OutputImageType *multiplyImageFilterOutput);

  /** Calculate the second derivative of the smoothed image, it writes the
   *  result to the update buffer using the ThreadedCompute2ndDerivative() method
   *  and multithreading mechanism. */
  void Compute2ndDerivative();

  /**
   * Split the input into "num" pieces, returning region "i" as
   * "splitRegion". This method is called "num" times to return non-overlapping
   * regions. The method returns the number of pieces that the input
   * can be split into by the routine. i.e. return value is less than or equal
   * to "num".
   * \sa ImageSource
   */
  //  virtual
  //  int SplitUpdateContainer(int i, int num, ThreadRegionType& splitRegion);

  /** Does the actual work of calculating of the 2nd derivative over a region
   *  supplied by the multithreading mechanism.
   *
   *  \sa Compute2ndDerivative
   *  \sa Compute2ndDerivativeThreaderCallBack   */
  void ThreadedCompute2ndDerivative(const OutputImageRegionType &
                                    outputRegionForThread, ThreadIdType threadId);

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire an
   * output region that it passes to ThreadedCompute2ndDerivative for
   * processing.  */
  static ITK_THREAD_RETURN_TYPE
  Compute2ndDerivativeThreaderCallback(void *arg);

  /** This method is used to calculate the 2nd derivative for
   * non-boundary pixels. It is called by the ThreadedCompute2ndDerivative
   * method. */
  OutputImagePixelType ComputeCannyEdge(const NeighborhoodType & it,
                                        void *globalData);

  /** Calculate the gradient of the second derivative of the smoothed image,
   *  it writes the result to m_UpdateBuffer1 using the
   *  ThreadedCompute2ndDerivativePos() method and multithreading mechanism.
   */
  void Compute2ndDerivativePos();

  /** Does the actual work of calculating of the 2nd derivative over a region
   *  supplied by the multithreading mechanism.
   *
   *  \sa Compute2ndDerivativePos
   *  \sa Compute3ndDerivativePosThreaderCallBack   */
  void ThreadedCompute2ndDerivativePos(const OutputImageRegionType &
                                       outputRegionForThread, ThreadIdType threadId);

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire an
   * output region that it passes to ThreadedCompute2ndDerivative for
   * processing. */
  static ITK_THREAD_RETURN_TYPE
  Compute2ndDerivativePosThreaderCallback(void *arg);

  ArrayType m_Variance;

  ArrayType m_MaximumError;

  OutputImagePixelType m_UpperThreshold;  //should be float here?

  OutputImagePixelType m_LowerThreshold; //should be float here?

  typename OutputImageType::Pointer m_UpdateBuffer1;

  /** Gaussian filter to smooth the input image. */
  typename GaussianImageFilterType::Pointer m_GaussianFilter;

  /** Multiply image filter to multiply with the zero crossings of the second
   *  derivative. */
  typename MultiplyImageFilterType::Pointer m_MultiplyImageFilter;

  /** Function objects that are used in the inner loops of derivatiVex
   *  calculations. */
  DerivativeOperator< OutputImagePixelType, itkGetStaticConstMacro(ImageDimension) >
  m_ComputeCannyEdge1stDerivativeOper;
  DerivativeOperator< OutputImagePixelType, itkGetStaticConstMacro(ImageDimension) >
  m_ComputeCannyEdge2ndDerivativeOper;

  std::slice m_ComputeCannyEdgeSlice[ImageDimension];

  SizeValueType m_Stride[ImageDimension];
  SizeValueType m_Center;

  typename ListNodeStorageType::Pointer m_NodeStore;
  ListPointerType                       m_NodeList;

  OutputImageType      *m_OutputImage;
};
} //end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCannyEdgeDetectionImageFilter.hxx"
#endif

#endif
