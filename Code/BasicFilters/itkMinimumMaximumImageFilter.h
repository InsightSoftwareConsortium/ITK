/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinimumMaximumImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMinimumMaximumImageFilter_h
#define __itkMinimumMaximumImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"

#include <vector>

#include "itkNumericTraits.h"

namespace itk
{
/** \class MinimumMaximumImageFilter
 * \brief Computes the minimum and the maximum intensity values of
 * an image. 
 *
 * It is templated over input image type only.
 * This filter just copies the input image through this output to
 * be included within the pipeline. The implementation uses the
 * StatisticsImageFilter.
 *
 * \ingroup Operators
 * \sa StatisticsImageFilter
 */
template <class TInputImage>
class ITK_EXPORT MinimumMaximumImageFilter :
    public ImageToImageFilter< TInputImage, TInputImage>
{
public:
  /** Extract dimension from input image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef MinimumMaximumImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TInputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Image related typedefs. */
  typedef typename TInputImage::Pointer InputImagePointer;

  typedef typename TInputImage::RegionType RegionType ;
  typedef typename TInputImage::SizeType SizeType ;
  typedef typename TInputImage::IndexType IndexType ;
  typedef typename TInputImage::PixelType PixelType ;

  /** Smart Pointer type to a DataObject. */
  typedef typename DataObject::Pointer DataObjectPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MinimumMaximumImageFilter, ImageToImageFilter);
  
  /** Image typedef support. */
  typedef TInputImage InputImageType;

  /** Type of DataObjects used for scalar outputs */
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

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  virtual DataObjectPointer MakeOutput(unsigned int idx);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(LessThanComparableCheck,
                  (Concept::LessThanComparable<PixelType>));
  itkConceptMacro(GreaterThanComparableCheck,
                  (Concept::GreaterThanComparable<PixelType>));
  itkConceptMacro(OStreamWritableCheck,
                  (Concept::OStreamWritable<PixelType>));
  /** End concept checking */
#endif

protected:
  MinimumMaximumImageFilter();
  virtual ~MinimumMaximumImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Pass the input through unmodified. Do this by Grafting in the AllocateOutputs method. */
  void AllocateOutputs();      

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

  // Override since the filter produces all of its output
  void EnlargeOutputRequestedRegion(DataObject *data);

private:
  MinimumMaximumImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  std::vector<PixelType> m_ThreadMin;
  std::vector<PixelType> m_ThreadMax;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinimumMaximumImageFilter.txx"
#endif

#endif
