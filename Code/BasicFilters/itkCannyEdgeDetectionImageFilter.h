/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCannyEdgeDetectionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCannyEdgeDetectionImageFilter_h
#define __itkCannyEdgeDetectionImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkMultiThreader.h"
#include "itkDerivativeOperator.h"

namespace itk
{
/** \class CannyEdgeDetectionImageFilter
 *
 * This filter is an implementation of a Canny edge detector for scalar-valued
 * images.  Based on John Canny's paper "A Computational Approach to Edge 
 * Detection"(IEEE Transactions on Pattern Analysis and Machine Intelligence, 
 * Vol. PAMI-8, No.6, November 1986),  there are four major steps used in the 
 * edge-detection scheme:
 * (1) The input image is smoothed using a Gaussian filter.
 * (2) The zero-crossings of the second derivative of the smoothed image are
 *     found.
 * (3) The zero-crossing image is multiplied (pixel-wise) with the
 *     second-derivative gradient image of the smoothed image. 
 * (4) The resulting image is thresholded to eliminate uninteresting edges.
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
 * \sa ThresholdImageFilter */
template<class TInputImage, class TOutputImage>
class ITK_EXPORT CannyEdgeDetectionImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard "Self" & Superclass typedef.  */
  typedef CannyEdgeDetectionImageFilter    Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
   
  /** Image typedef support   */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
      
  /** SmartPointer typedef support  */    
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Define pixel types. */
  typedef typename TInputImage::PixelType  InputImagePixelType;
  typedef typename TOutputImage::PixelType  OutputImagePixelType;

  /** The default boundary condition is used unless overridden 
   *in the Evaluate() method. */
  typedef ZeroFluxNeumannBoundaryCondition<OutputImageType>
  DefaultBoundaryConditionType;

  /** The type of data structure that is passed to this function object
   * to evaluate at a pixel that does not lie on a data set boundary.
   */
  typedef ConstNeighborhoodIterator<OutputImageType,
    DefaultBoundaryConditionType> NeighborhoodType;

  /** Method for creation through the object factory.  */
  itkNewMacro(Self);  
    
  /** Typedef to describe the output image region type.*/
  typedef typename TOutputImage::RegionType OutputImageRegionType;
    
  /** Run-time type information (and related methods). */
  itkTypeMacro(CannyEdgeDetectionImageFilter, ImageToImageFilter);
  
  /** ImageDimension constant    */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  
  /** Standard get/set macros for filter parameters. */
  itkSetVectorMacro(Variance, double, ImageDimension); 
  itkGetVectorMacro(Variance, const double, ImageDimension); 
  itkSetVectorMacro(MaximumError, double, ImageDimension); 
  itkGetVectorMacro(MaximumError, const double, ImageDimension);
  
  /** Set/Get the Variance parameter used by the Gaussian smoothing
      filter in this algorithm */
  void SetVariance(const double v)
    {
      double vArray[ImageDimension];
      for (unsigned int i = 0; i<ImageDimension; ++i) { vArray[i] = v; }
      this->SetVariance(vArray);
    }
  
  /** Set/Get the MaximumError paramter used by the Gaussian smoothing filter
      in this algorithm */
  void SetMaximumError(const double v)
    {
      double vArray[ImageDimension];
      for (unsigned int i = 0; i<ImageDimension; ++i) { vArray[i] = v; }
      this->SetMaximumError(vArray);
    }
  
  /* Set the Threshold value for detected edges. */
  itkSetMacro(Threshold, OutputImagePixelType );
  itkGetMacro(Threshold, OutputImagePixelType);

    /* Set the Thresholdvalue for detected edges. */
  itkSetMacro(OutsideValue, OutputImagePixelType);
  itkGetMacro(OutsideValue, OutputImagePixelType);
  
    /* Set the value to be assigned to the detected edges. */
  itkSetMacro(OutputEdgeValue, OutputImagePixelType);
  itkGetMacro(OutputEdgeValue, OutputImagePixelType);
  
  /** CannyEdgeDetectionImageFilter needs a larger input requested
   * region than the output requested region ( derivative operators, etc).  
   * As such, CannyEdgeDetectionImageFilter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the 
   * pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()  */  
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

protected:
  CannyEdgeDetectionImageFilter();
  CannyEdgeDetectionImageFilter(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData();

private:
  virtual ~CannyEdgeDetectionImageFilter(){};

  /** Thread-Data Structure   */
  struct CannyThreadStruct
  {
    CannyEdgeDetectionImageFilter *Filter;
  };

  /** This allocate storage for m_UpdateBuffer, m_UpdateBuffer1 */
  void AllocateUpdateBuffer();


  /** Calculate the second derivative of the smoothed image, it writes the 
   *  result to m_UpdateBuffer using the ThreadedCompute2ndDerivative() method
   *  and multithreading mechanism.   */
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
  void ThreadedCompute2ndDerivative(const OutputImageRegionType&
                                    outputRegionForThread, int threadId);

  /** This callback method uses ImageSource::SplitRequestedRegion to acquire an 
   * output region that it passes to ThreadedCompute2ndDerivative for
   * processing.  */
  static ITK_THREAD_RETURN_TYPE
      Compute2ndDerivativeThreaderCallback( void * arg );

  /** This methos is used to calculate the 2nd derivative for 
   * non-boundary pixels. It is called by the ThreadedCompute2ndDerivative 
   * method. */  
  OutputImagePixelType ComputeCannyEdge(const NeighborhoodType &it,
                                        void *globalData );

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
  void ThreadedCompute2ndDerivativePos(const OutputImageRegionType&
                                       outputRegionForThread, int threadId);

  /**This callback method uses ImageSource::SplitRequestedRegion to acquire an
   * output region that it passes to ThreadedCompute2ndDerivative for
   * processing.   */
  static ITK_THREAD_RETURN_TYPE
  Compute2ndDerivativePosThreaderCallback( void *arg );

  /** The variance of the Gaussian Filter used in this filter */
  double m_Variance[ImageDimension];

  /** The maximum error of the gaussian blurring kernel in each dimensional
   * direction.  */
  double m_MaximumError[ImageDimension];  

  /** Threshold value for identifying edges. */
  OutputImagePixelType m_Threshold;

  /** "Background" value for use in thresholding. */
  OutputImagePixelType m_OutsideValue;

  /** Value to assign to the edges. */
  OutputImagePixelType m_OutputEdgeValue;

  /** Update buffers used during calculation of multiple steps */
  typename OutputImageType::Pointer  m_UpdateBuffer;
  typename OutputImageType::Pointer  m_UpdateBuffer1;

  /** Function objects that are used in the inner loops of derivative
      calculations. */
  DerivativeOperator<OutputImagePixelType,itkGetStaticConstMacro(ImageDimension)>
    m_ComputeCannyEdge1stDerivativeOper;
  DerivativeOperator<OutputImagePixelType,itkGetStaticConstMacro(ImageDimension)>
    m_ComputeCannyEdge2ndDerivativeOper;

  std::slice  m_ComputeCannyEdgeSlice[ImageDimension];

  unsigned long m_Stride[ImageDimension];
  unsigned long m_Center;
};

} //end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCannyEdgeDetectionImageFilter.txx"
#endif
  
#endif

