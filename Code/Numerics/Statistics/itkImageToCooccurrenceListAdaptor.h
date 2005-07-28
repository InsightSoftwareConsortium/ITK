/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToCooccurrenceListAdaptor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkImageToCooccurrenceListAdaptor_h
#define __itkImageToCooccurrenceListAdaptor_h

#include <typeinfo>

#include "itkImage.h"
#include "itkPixelTraits.h"
#include "itkImageToListAdaptor.h"
#include "itkSmartPointer.h"
#include "itkImageRegionIterator.h"
#include "itkShapedNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkConstantBoundaryCondition.h"
#include "itkListSample.h"
#include "itkFixedArray.h"
#include "itkMacro.h"

#include <vector>
#include <algorithm>
#include <iostream>

namespace itk{ 
namespace Statistics{

/** \class ImageToCooccurrenceListAdaptor
 *  \brief Converts pixel data into a list of pairs in order to compute a cooccurrence Histogram.
 *
 *  This class is intended to be used in combination with the ListToHistogramGenerator class.
 *
 * \author Glenn Pierce
 *
 * \ingroup Statistics
 */
 
template < class TImage >
class ITK_EXPORT ImageToCooccurrenceListAdaptor
  : public ImageToListAdaptor< 
  TImage, 
  FixedArray< typename TImage::PixelType, 2 > >
{
public:
  typedef TImage ImageType;

  typedef FixedArray< typename TImage::PixelType, 2 > MeasurementVectorType ;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType ;
  typedef typename SampleType::MeasurementVectorSizeType MeasurementVectorSizeType;

  /** Standard class typedefs */
  typedef ImageToCooccurrenceListAdaptor Self;
  typedef ImageToListAdaptor< TImage, MeasurementVectorType > Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Neighborhood iterator type. */
  typedef itk::ShapedNeighborhoodIterator< 
                  TImage , 
                  ConstantBoundaryCondition<TImage> 
                                       > ShapedNeighborhoodIteratorType;

  /** Offset type used for Neighborhoods **/
  typedef typename ShapedNeighborhoodIteratorType::OffsetType OffsetType;
  typedef std::vector<OffsetType> OffsetTable;
  
  void UseNeighbor(const OffsetType & offset);

  /** Triggers the Computation of the co-occurence matrix */
  void Compute();
 
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageToCooccurrenceListAdaptor, ListSampleBase);
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** the number of components in a measurement vector */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int, 2);

  virtual void SetMeasurementVectorSize( const MeasurementVectorSizeType )
    {
    // Measurement vector size for this class is fixed as the pixel's 
    // dimension. This method should have no effect
    itkWarningMacro( << "This method does nothing! The MeasurementVectorSize is " 
        << MeasurementVectorSize );
    }

 unsigned int GetMeasurementVectorSize() const
   {
   return Superclass::MeasurementVectorSize;
   } 
  

  /** Superclass typedefs for Measurement vector, measurement, 
   * Instance Identifier, frequency, size, size element value */
  typedef typename Superclass::FrequencyType FrequencyType ;
  typedef typename Superclass::MeasurementType MeasurementType ;
  typedef typename Superclass::InstanceIdentifier InstanceIdentifier ;
  typedef MeasurementVectorType ValueType ;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TImage::ImageDimension);


protected:
  ImageToCooccurrenceListAdaptor();
  virtual ~ImageToCooccurrenceListAdaptor() {}
  void PrintSelf(std::ostream& os, Indent indent) const;  

private:
  ImageToCooccurrenceListAdaptor(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented
  OffsetTable m_OffsetTable;
  typename SampleType::Pointer sample;
} ; // end of class ScalarImageToListAdaptor

} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageToCooccurrenceListAdaptor.txx"
#endif

#endif
