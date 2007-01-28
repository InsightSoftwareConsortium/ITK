/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStatisticsImageFilter_h
#define __itkStatisticsImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkNumericTraits.h"
#include "itkArray.h"
#include "itkSimpleDataObjectDecorator.h"


namespace itk {

/** \class StatisticsImageFilter
 * \brief Compute min. max, variance and mean of an Image.
 *
 * StatisticsImageFilter computes the minimum, maximum, sum, mean, variance
 * sigma of an image.  The filter needs all of its input image.  It
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
  typedef StatisticsImageFilter                        Self;
  typedef ImageToImageFilter<TInputImage,TInputImage>  Superclass;
  typedef SmartPointer<Self>                           Pointer;
  typedef SmartPointer<const Self>                     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(StatisticsImageFilter, ImageToImageFilter);

  /** Image related typedefs. */
  typedef typename TInputImage::Pointer InputImagePointer;

  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::PixelType  PixelType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension );

  /** Type to use for computations. */
  typedef typename NumericTraits<PixelType>::RealType RealType;

  /** Smart Pointer type to a DataObject. */
  typedef typename DataObject::Pointer DataObjectPointer;

  /** Type of DataObjects used for scalar outputs */
  typedef SimpleDataObjectDecorator<RealType>  RealObjectType;
  typedef SimpleDataObjectDecorator<PixelType> PixelObjectType;

  /** Return the computed Minimum. */
  PixelType GetMinimum() const
    { return this->GetMinimumOutput()->Get(); }
  PixelObjectType* GetMinimumOutput();
  const PixelObjectType* GetMinimumOutput() const;

  /** Return the computed Maximum. */
  PixelType GetMaximum() const
    { return this->GetMaximumOutput()->Get(); }
  PixelObjectType* GetMaximumOutput();
  const PixelObjectType* GetMaximumOutput() const;

  /** Return the computed Mean. */
  RealType GetMean() const
    { return this->GetMeanOutput()->Get(); }
  RealObjectType* GetMeanOutput();
  const RealObjectType* GetMeanOutput() const;

  /** Return the computed Standard Deviation. */
  RealType GetSigma() const
    { return this->GetSigmaOutput()->Get(); }
  RealObjectType* GetSigmaOutput();
  const RealObjectType* GetSigmaOutput() const;

  /** Return the computed Variance. */
  RealType GetVariance() const
    { return this->GetVarianceOutput()->Get(); }
  RealObjectType* GetVarianceOutput();
  const RealObjectType* GetVarianceOutput() const;

  /** Return the compute Sum. */
  RealType GetSum() const
    { return this->GetSumOutput()->Get(); }
  RealObjectType* GetSumOutput();
  const RealObjectType* GetSumOutput() const;

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  virtual DataObjectPointer MakeOutput(unsigned int idx);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<PixelType>));
  /** End concept checking */
#endif

protected:
  StatisticsImageFilter();
  ~StatisticsImageFilter(){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Pass the input through unmodified. Do this by Grafting in the
   *  AllocateOutputs method.
   */
  void AllocateOutputs();

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData ();

  /** Do final mean and variance computation from data accumulated in threads.
   */
  void AfterThreadedGenerateData ();

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData (const RegionType&
                              outputRegionForThread,
                              int threadId);

  // Override since the filter needs all the data for the algorithm
  void GenerateInputRequestedRegion();

  // Override since the filter produces all of its output
  void EnlargeOutputRequestedRegion(DataObject *data);

private:
  StatisticsImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  Array<RealType>  m_ThreadSum;
  Array<RealType>  m_SumOfSquares;
  Array<long>      m_Count;
  Array<PixelType> m_ThreadMin;
  Array<PixelType> m_ThreadMax;

}; // end of class

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatisticsImageFilter.txx"
#endif

#endif
