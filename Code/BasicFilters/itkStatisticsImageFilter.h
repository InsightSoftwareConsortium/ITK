/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStatisticsImageFilter_h
#define __itkStatisticsImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"

namespace itk {

/** \class StatisticsImageFilter 
 * \brief Compute min. max, variance and mean of an Image.
 *
 * StatisticsImageFilter computes the minimum, maximum, mean, variance
 * and sigma of an image.  The filter needs all of its input image.  It
 * behaves as a filter with an input and output. Thus it can be inserted
 * in a pipline with other filters and the statistics will only be
 * recomputed if a downstream filter changes.
 *
 * The filter passes its input through unmodified.  The filter is
 * threaded. It computes statistics in each thread then combines them in
 * its AfterThreadedGenerate method.
 *
 * \ingroup MathematicalStatisticsImageFilters
 */
template<class TInputImage>
class ITK_EXPORT StatisticsImageFilter : 
  public ImageToImageFilter<TInputImage, TInputImage>
{
public:
  /** Standard Self typedef */
  typedef StatisticsImageFilter Self;
  typedef ImageToImageFilter<TInputImage,TInputImage>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(StatisticsImageFilter, ImageToImageFilter);
  
  /** Image related typedefs. */
  typedef typename TInputImage::Pointer InputImagePointer;

  typedef typename TInputImage::RegionType RegionType ;
  typedef typename TInputImage::SizeType SizeType ;
  typedef typename TInputImage::IndexType IndexType ;
  typedef typename TInputImage::PixelType PixelType ;
  
  /** Image related typedefs. */
  enum { ImageDimension = TInputImage::ImageDimension } ;

  /** Type to use form computations. */
  typedef typename NumericTraits<PixelType>::RealType RealType;
  
  /** Return the computed Minimum. */
  itkGetMacro(Minimum,RealType);

  /** Return the computed Maximum. */
  itkGetMacro(Maximum,RealType);

  /** Return the computed Mean. */
  itkGetMacro(Mean,RealType);

  /** Return the computed Standard Deviation. */
  itkGetMacro(Sigma,RealType);

  /** Return the computed Variance. */
  itkGetMacro(Variance,RealType);

protected:
  StatisticsImageFilter(): m_Sum(0), m_SumOfSquares(0), m_Count(0), m_ThreadMax(0), m_ThreadMin(0)
    {
      m_Minimum = NumericTraits<RealType>::max();
      m_Maximum = NumericTraits<RealType>::NonpositiveMin();
      m_Mean = NumericTraits<RealType>::max();
      m_Sigma = NumericTraits<RealType>::max();
      m_Variance = NumericTraits<RealType>::max();
    };
  
  ~StatisticsImageFilter()
    {
      if (m_Sum)
        {
        delete [] m_Sum;
        }
      if (m_SumOfSquares)
        {
        delete [] m_SumOfSquares;
        }
      if (m_Count)
        {
        delete [] m_Count;
        }
      if (m_ThreadMin)
        {
        delete [] m_ThreadMin;
        }
      if (m_ThreadMax)
        {
        delete [] m_ThreadMax;
        }
    };
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData ();
  
  /** Do final mean and variance computation from data accumulated in threads. */
  void AfterThreadedGenerateData ();
  
  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData (const RegionType& 
                              outputRegionForThread,
                              int threadId) ;

  // Override since the filter needs all the data for the algorithm
  void GenerateInputRequestedRegion();

 private:
  StatisticsImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  RealType m_Minimum;
  RealType m_Maximum;
  RealType m_Mean;
  RealType m_Sigma;
  RealType m_Variance;

  RealType *m_Sum;
  RealType *m_SumOfSquares;
  long      *m_Count;
  RealType *m_ThreadMin;
  RealType *m_ThreadMax;

} ; // end of class

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatisticsImageFilter.txx"
#endif

#endif
