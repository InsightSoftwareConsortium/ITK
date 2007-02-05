/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDifferenceImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
 * \brief Implements comparison between two images.
 *
 * This filter is used by the testing system to compute the difference between
 * a valid image and an image produced by the test. The comparison value is
 * computed by visiting all the pixels in the baseline images and comparing
 * their values with the pixel values in the neighborhood of the homologous
 * pixel in the other image.
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
  
  /** Set/Get the maximum distance away to look for a matching pixel.
      Default is 0. */
  itkSetMacro(ToleranceRadius, int);
  itkGetMacro(ToleranceRadius, int);
  
  /** Set/Get the minimum threshold for pixels to be different.
      Default is 0. */
  itkSetMacro(DifferenceThreshold, OutputPixelType);
  itkGetMacro(DifferenceThreshold, OutputPixelType);
  
  /** Get parameters of the difference image after execution.  */
  itkGetMacro(MeanDifference, RealType);
  itkGetMacro(TotalDifference, AccumulateType);
  itkGetMacro(NumberOfPixelsWithDifferences, unsigned long);
  
protected:
  DifferenceImageFilter();
  virtual ~DifferenceImageFilter() {}
  
  void PrintSelf(std::ostream& os, Indent indent) const;

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
  unsigned long  m_NumberOfPixelsWithDifferences;
  int m_ToleranceRadius;
  
  Array<AccumulateType> m_ThreadDifferenceSum;
  Array<unsigned long>  m_ThreadNumberOfPixels;
  
private:
  DifferenceImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

/*
// Define instantiation macro for this template.
#define ITK_TEMPLATE_DifferenceImageFilter(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT DifferenceImageFilter< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef DifferenceImageFilter< ITK_TEMPLATE_2 x > \
                                            DifferenceImageFilter##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkDifferenceImageFilter+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkDifferenceImageFilter.txx"
#endif
*/

#ifndef ITK_MANUAL_INSTANTIATION
# include "itkDifferenceImageFilter.txx"
#endif

#endif
