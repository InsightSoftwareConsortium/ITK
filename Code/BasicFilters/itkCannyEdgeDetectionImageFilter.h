
/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCannyEdgeDetectionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/

#ifndef __itkCannyEdgeDetectionImageFilter_h
#define __itkCannyEdgeDetectionImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkConstSmartNeighborhoodIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkMultiThreader.h"

namespace itk
{

/** \class CannyEdgeDetectionImageFilter
 * \brief Implement the canny edge detector for gray-scale images.
 *
 *  There are fours steps in calculating the canny edges:
 *  1) First smoothe the image by the Gaussian filter.
 *  2) Calculate the directional derivative normals.
 *  3) Aplly nonmaxima suppression(find local edges):calculate 2nd derivative
 *     of the smoothed image, and apply ZeroCrossingImageFilter to get edges.   *  4) Thesholding the resulting image to eliminate bogus edges. Here only
 *     single thresholding is implemented, edge-linking not done due to the
 *     the lack of connected-component labeling algorithms
 *
 * \sa DiscreteGaussianImageFilter
 * \sa ZeroCrossingImageFilter
 * \sa ThresholdImageFilter
 */

template<class TInputImage, class TOutputImage>
  class ITK_EXPORT CannyEdgeDetectionImageFilter: public ImageToImageFilter<TInputImage, TOutputImage>
  {

  public:
    /**
     * standard "Self" & Superclass typedef.
     */
    typedef CannyEdgeDetectionImageFilter    Self;
    typedef ImageToImageFilter<TInputImage, TOutputImage> Superclass;
 

    /**
      * Image typedef support
      */
    typedef TInputImage  InputImageType;
    typedef TOutputImage OutputImageType;

       
    /** 
     * SmartPointer typedef support 
     */    

    typedef SmartPointer<Self>  Pointer;
    typedef SmartPointer<const Self>  ConstPointer;

    /**
     * Define pixel type
     */
    typedef typename TInputImage::PixelType  InputImagePixelType;
    typedef typename TOutputImage::PixelType  OutputImagePixelType;


  /**
   * The default boundary condition is used unless overridden 
   *in the Evaluate() method.
   */
    typedef ZeroFluxNeumannBoundaryCondition<OutputImageType>
      DefaultBoundaryConditionType;


  /**
   * The type of data structure that is passed to this function object
   * to evaluate at a pixel that does not lie on a data set boundary.
   */
    typedef ConstNeighborhoodIterator<OutputImageType> NeighborhoodType;
    
  /**
   * The type of data structure that is passed to this function object
   * to evaluate at a pixel that lies on a data set boundary.
   */
    typedef ConstSmartNeighborhoodIterator<OutputImageType,
      DefaultBoundaryConditionType> BoundaryNeighborhoodType;


    /**
     * Method for creation through the object factory.
     */
    itkNewMacro(Self);  
    
    /**
     * Typedef to describe the output image region type.
     */
    typedef typename TOutputImage::RegionType OutputImageRegionType;
    
    
    /** 
     * Run-time type information (and related methods).
     */
    itkTypeMacro(CannyEdgeDetectionImageFilter, ImageToImageFilter);
    
    /**
     * ImageDimension enumeration
     */
    enum { ImageDimension = TInputImage::ImageDimension };

 /**
   * Standard get/set macros for filter parameters.
   */
  itkSetVectorMacro(Variance, float, ImageDimension);
  itkGetVectorMacro(Variance, const float, ImageDimension);
  itkSetVectorMacro(MaximumError, float, ImageDimension);
  itkGetVectorMacro(MaximumError, const float, ImageDimension);
  

  void SetVariance(const float v)
    {
      float vArray[ImageDimension];
      for (unsigned int i = 0; i<ImageDimension; ++i) { vArray[i] = v; }
      this->SetVariance(vArray);
    }
  void SetMaximumError(const float v)
    {
      float vArray[ImageDimension];
      for (unsigned int i = 0; i<ImageDimension; ++i) { vArray[i] = v; }
      this->SetMaximumError(vArray);
    }
  
  void SetThreshold(float thresh)
    {
      m_Threshold = thresh;
    }

  /**
   * CannyEdgeDetectionImageFilter needs a larger input requested
   * region than the output requested region ( derivative operators, etc).  
   * As such, CannyEdgeDetectionImageFilter needs to provide an implementation
   * for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()
   */
    
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);


  protected:
    CannyEdgeDetectionImageFilter()
      {
        this->SetVariance(0.0f);
        this->SetMaximumError(0.01f);
        m_Threshold = (OutputImagePixelType) 0;
        m_UpdateBuffer = OutputImageType::New();
        m_UpdateBuffer1 = OutputImageType::New();
      }
    ~CannyEdgeDetectionImageFilter(){}
    CannyEdgeDetectionImageFilter(const Self&) {}

    void GenerateData();


private:
   /**
   * The variance of the Gaussian Filter used in this filter
   */
  float m_Variance[ImageDimension];
  /**
   * The maximum error of the gaussian blurring kernel in each dimensional
   * direction.
   */

  float m_MaximumError[ImageDimension];  

  /*
   * Thresholds
   */
  OutputImagePixelType m_Threshold;

  /**
   * Update buffers used during calculation of multiple steps
   */
  typename OutputImageType::Pointer  m_UpdateBuffer;
  typename OutputImageType::Pointer  m_UpdateBuffer1;

  /**
   *  Thread-Data Structure 
   */
  struct CannyThreadStruct
  {
    CannyEdgeDetectionImageFilter *Filter;
  };

  /**
   * This allocate storage for m_UpdateBuffer, m_UpdateBuffer1
   */
  void AllocateUpdateBuffer();


  /**
   *  Calculate the second derivative of the smoothed image, it writes the 
   *  result to m_UpdateBuffer using the ThreadedCompute2ndDerivative() method
   *  and multithreading mechanism.
   */
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

  /**
   *  Does the actual work of calculating of the 2nd derivative over a region 
   *  supplied by the multithreading mechanism.  
   *
   *  \sa Compute2ndDerivative
   *  \sa Compute2ndDerivativeThreaderCallBack
   */ 

  void ThreadedCompute2ndDerivative(const OutputImageRegionType& outputRegionForThread, int threadId);


  /**
   * This callback method uses ImageSource::SplitRequestedRegion to acquire an
   * output region that it passes to ThreadedCompute2ndDerivative for processing.
   */
  static ITK_THREAD_RETURN_TYPE Compute2ndDerivativeThreaderCallback( void * arg );

  /**
   * This methos is used to calculate the 2nd derivative for 
   * non-boundary pixels. It is called by the ThreadedCompute2ndDerivative 
   * method.
   */  

  OutputImagePixelType ComputeCannyEdge(const NeighborhoodType &it,
                                        void *globalData );

  /**
   * This methos is used to calculate the 2nd derivative for 
   * boundary pixels. It is called by the ThreadedCompute2ndDerivative 
   * method.
   */  

  OutputImagePixelType ComputeCannyEdge(const BoundaryNeighborhoodType &bit,
                                        void *globalData );


  /**
   *  Calculate the gradient of the second derivative of the smoothed image, 
   *  it writes the result to m_UpdateBuffer1 using the 
   *  ThreadedCompute2ndDerivativePos() method and multithreading mechanism.
   */
  void Compute2ndDerivativePos();

  /**
   *  Does the actual work of calculating of the 2nd derivative over a region 
   *  supplied by the multithreading mechanism.  
   *
   *  \sa Compute2ndDerivativePos
   *  \sa Compute3ndDerivativePosThreaderCallBack
   */ 
  void ThreadedCompute2ndDerivativePos(const OutputImageRegionType& outputRegionForThread, int threadId);

  /**
   * This callback method uses ImageSource::SplitRequestedRegion to acquire an
   * output region that it passes to ThreadedCompute2ndDerivative for processing.
   */
  static ITK_THREAD_RETURN_TYPE Compute2ndDerivativePosThreaderCallback( void * arg );


  };

 

} //end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCannyEdgeDetectionImageFilter.txx"
#endif
  
#endif

