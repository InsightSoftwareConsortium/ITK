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
 * \ingroup ITKImageStatistics
 */
template< typename TInputImage >
class MinimumMaximumImageFilter:
  public ImageToImageFilter< TInputImage, TInputImage >
{
public:
  /** Extract dimension from input image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef MinimumMaximumImageFilter                      Self;
  typedef ImageToImageFilter< TInputImage, TInputImage > Superclass;
  typedef SmartPointer< Self >                           Pointer;
  typedef SmartPointer< const Self >                     ConstPointer;

  /** Image related typedefs. */
  typedef typename TInputImage::Pointer InputImagePointer;

  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::PixelType  PixelType;

  /** Smart Pointer type to a DataObject. */
  typedef typename DataObject::Pointer DataObjectPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MinimumMaximumImageFilter, ImageToImageFilter);

  /** Image typedef support. */
  typedef TInputImage InputImageType;

  /** Type of DataObjects used for scalar outputs */
  typedef SimpleDataObjectDecorator< PixelType > PixelObjectType;

  /** Return the computed Minimum. */
  PixelType GetMinimum() const
  { return this->GetMinimumOutput()->Get(); }
  PixelObjectType * GetMinimumOutput();

  const PixelObjectType * GetMinimumOutput() const;

  /** Return the computed Maximum. */
  PixelType GetMaximum() const
  { return this->GetMaximumOutput()->Get(); }
  PixelObjectType * GetMaximumOutput();

  const PixelObjectType * GetMaximumOutput() const;

  /** Make a DataObject of the correct type to be used as the specified
   * output. */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( LessThanComparableCheck,
                   ( Concept::LessThanComparable< PixelType > ) );
  itkConceptMacro( GreaterThanComparableCheck,
                   ( Concept::GreaterThanComparable< PixelType > ) );
  itkConceptMacro( OStreamWritableCheck,
                   ( Concept::OStreamWritable< PixelType > ) );
  // End concept checking
#endif

protected:
  MinimumMaximumImageFilter();
  virtual ~MinimumMaximumImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Pass the input through unmodified. Do this by Grafting in the
    AllocateOutputs method. */
  void AllocateOutputs();

  /** Initialize some accumulators before the threads run. */
  void BeforeThreadedGenerateData();

  /** Do final mean and variance computation from data accumulated in threads.
    */
  void AfterThreadedGenerateData();

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const RegionType &
                             outputRegionForThread,
                             ThreadIdType threadId);

  // Override since the filter needs all the data for the algorithm
  void GenerateInputRequestedRegion();

  // Override since the filter produces all of its output
  void EnlargeOutputRequestedRegion(DataObject *data);

private:
  MinimumMaximumImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);            //purposely not implemented

  std::vector< PixelType > m_ThreadMin;
  std::vector< PixelType > m_ThreadMax;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMinimumMaximumImageFilter.hxx"
#endif

#endif
