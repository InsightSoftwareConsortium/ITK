/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDifferenceImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDifferenceImageFilter_h
#define __itkDifferenceImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkArray.h"

namespace itk
{
  
/** \class DifferenceImageFilter
 * \brief Implements pixel-wise comparison of two images.
 *
 * This filter is used by the testing system to compute the difference
 * between a valid image and an image produced by the test.
 * 
 * \ingroup IntensityImageFilters   Multithreaded
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT DifferenceImageFilter :
    public ImageToImageFilter<TInputImage, TOutputImage> 
{
public:
  /** Standard class typedefs. */
  typedef DifferenceImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(DifferenceImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename OutputImageType::PixelType OutputPixelType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  typedef typename NumericTraits<OutputPixelType>::RealType RealType;
  typedef typename NumericTraits<RealType>::AccumulateType AccumulateType;  
  
  /** Set the valid image input.  This will be input 0.  */
  virtual void SetValidInput(const InputImageType* validImage);
  
  /** Set the test image input.  This will be input 1.  */
  virtual void SetTestInput(const InputImageType* testImage);
  
  /** Set/Get the minimum threshold for pixels to be different.
      Default is 0. */
  itkSetMacro(DifferenceThreshold, OutputPixelType);
  itkGetMacro(DifferenceThreshold, OutputPixelType);
  
  /** Get parameters of the difference image after execution.  */
  itkGetMacro(MeanDifference, RealType);
  itkGetMacro(TotalDifference, AccumulateType);
  
protected:
  DifferenceImageFilter();
  virtual ~DifferenceImageFilter() {}
  
  /** DifferenceImageFilter can be implemented as a multithreaded
   * filter.  Therefore, this implementation provides a
   * ThreadedGenerateData() routine which is called for each
   * processing thread. The output image data is allocated
   * automatically by the superclass prior to calling
   * ThreadedGenerateData().  ThreadedGenerateData can only write to
   * the portion of the output image specified by the parameter
   * "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  void ThreadedGenerateData(const OutputImageRegionType& threadRegion,
                            int threadId);
  
  void BeforeThreadedGenerateData();
  void AfterThreadedGenerateData();
  
  OutputPixelType m_DifferenceThreshold;
  RealType m_MeanDifference;
  AccumulateType m_TotalDifference;
  
  Array<AccumulateType> m_ThreadDifferenceSum;
  
private:
  DifferenceImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDifferenceImageFilter.txx"
#endif

#endif
