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
#ifndef itkScalarImageToCooccurrenceListSampleFilter_h
#define itkScalarImageToCooccurrenceListSampleFilter_h

#include <typeinfo>

#include "itkImageToListSampleFilter.h"
#include "itkSmartPointer.h"
#include "itkImageRegionIterator.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkConstantBoundaryCondition.h"
#include "itkFixedArray.h"

#include <vector>
#include <algorithm>
#include <iostream>

namespace itk
{
namespace Statistics
{
/** \class ScalarImageToCooccurrenceListSampleFilter
 *  \brief Converts pixel data into a list of pairs in order to compute a cooccurrence Histogram.
 *
 *  This class is intended to be used in combination with the ListToHistogramFilter class.
 *
 * \author Glenn Pierce
 *
 * \ingroup Statistics
 * \ingroup ITKStatistics
 */

template< typename TImage >
class ITK_TEMPLATE_EXPORT ScalarImageToCooccurrenceListSampleFilter:
  public ProcessObject
{
public:
  typedef TImage ImageType;

  typedef FixedArray< typename TImage::PixelType, 2 > MeasurementVectorType;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;
  typedef typename SampleType::MeasurementVectorSizeType       MeasurementVectorSizeType;

  /** Standard class typedefs */
  typedef ScalarImageToCooccurrenceListSampleFilter Self;
  typedef ProcessObject                             Superclass;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;

  /** Neighborhood iterator type. */
  typedef itk::ShapedNeighborhoodIterator<
    TImage,
    ConstantBoundaryCondition< TImage >
    > ShapedNeighborhoodIteratorType;

  /** Offset type used for Neighborhoods */
  typedef typename ShapedNeighborhoodIteratorType::OffsetType OffsetType;
  typedef std::vector< OffsetType >                           OffsetTable;

  void UseNeighbor(const OffsetType & offset);

  /** Method to set/get the image */
  using Superclass::SetInput;
  void SetInput(const ImageType *image);

  const ImageType * GetInput() const;

  /** method to get the List sample */
  const SampleType * GetOutput() const;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarImageToCooccurrenceListSampleFilter, ProcessObject);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** the number of components in a measurement vector */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int, 2);

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);

protected:
  ScalarImageToCooccurrenceListSampleFilter();
  virtual ~ScalarImageToCooccurrenceListSampleFilter() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  typedef DataObject::Pointer                           DataObjectPointer;
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

  /** This method causes the filter to generate its output. */
  virtual void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ScalarImageToCooccurrenceListSampleFilter);

  OffsetTable m_OffsetTable;
};  // end of class ScalarImageToListSampleFilter
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarImageToCooccurrenceListSampleFilter.hxx"
#endif

#endif
