/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageToCooccurrenceListSampleFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkScalarImageToCooccurrenceListSampleFilter_h
#define __itkScalarImageToCooccurrenceListSampleFilter_h

#include <typeinfo>

#include "itkImage.h"
#include "itkPixelTraits.h"
#include "itkImageToListSampleFilter.h"
#include "itkSmartPointer.h"
#include "itkImageRegionIterator.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkConstantBoundaryCondition.h"
#include "itkListSample.h"
#include "itkFixedArray.h"
#include "itkMacro.h"
#include "itkProcessObject.h"

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
 */

template< class TImage >
class ITK_EXPORT ScalarImageToCooccurrenceListSampleFilter:
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
  virtual ~ScalarImageToCooccurrenceListSampleFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  typedef DataObject::Pointer DataObjectPointer;
  virtual DataObjectPointer MakeOutput(unsigned int idx);

  /** This method causes the filter to generate its output. */
  virtual void GenerateData();

private:
  ScalarImageToCooccurrenceListSampleFilter(const Self &); //purposely not
                                                           // implemented
  void operator=(const Self &);                            //purposely not
                                                           // implemented

  OffsetTable m_OffsetTable;
};  // end of class ScalarImageToListSampleFilter
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarImageToCooccurrenceListSampleFilter.txx"
#endif

#endif
