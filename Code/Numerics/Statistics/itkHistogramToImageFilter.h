/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHistogramToImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkHistogramToImageFilter_h
#define __itkHistogramToImageFilter_h

#include "itkImageSource.h"
#include "itkConceptChecking.h"
#include "itkHistogram.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkSimpleDataObjectDecorator.h"


/** \class HistogramToImageFilter
 *  \brief This class takes a histogram as an input and returns an image of
 *  unsigned long. 
 *
 *  The dimension of the image is equal to the size of each measurement
 *  vector of the histogram. The size in the image along each dimension will be 
 *  equal to the number of bins along each dimension of the histogram.
 *
 *  The filter may be used in registration methods to plot the joint histogram
 *  after every iteration.
 *
 */

namespace itk
{

template <class THistogram>
class ITK_EXPORT HistogramToImageFilter :
  public ImageSource<Image<unsigned long, 
  ::itk::Statistics::GetHistogramDimension<THistogram>::HistogramDimension> >
{
public:

  /** Standard class typedefs. */
  typedef HistogramToImageFilter               Self;
  typedef ImageSource< Image<unsigned long, THistogram::MeasurementVectorSize> >            
    Superclass;
  typedef SmartPointer<Self>                   Pointer;
  typedef SmartPointer<const Self>             ConstPointer;
  
  typedef Image<unsigned long, THistogram::MeasurementVectorSize>  
    OutputImageType; 
  typedef typename Superclass::Pointer    OutputImagePointer;
  typedef typename OutputImageType::SpacingType SpacingType;
  typedef typename OutputImageType::PointType PointType;
  
  // Define an iterator to iterate through the image
  typedef itk::ImageRegionIteratorWithIndex< Image<unsigned long, 
          THistogram::MeasurementVectorSize> > ImageIteratorType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(HistogramToImageFilter,ImageSource);

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Some convenient typedefs. */
  typedef THistogram  HistogramType;
  typedef typename HistogramType::MeasurementVectorType  MeasurementVectorType;
  typedef typename HistogramType::SizeType               HistogramSizeType;
  typedef typename HistogramType::SizeType               SizeType;

  /** Since histograms are not dataobjects, we use the decorator to push
   *  them down the pipeline */ 
  typedef SimpleDataObjectDecorator< HistogramType* > InputHistogramObjectType;
  
  /** Determine the image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                     THistogram::MeasurementVectorSize  );
  
  /** Set/Get the input of this process object.  */
  virtual void SetInput( const HistogramType *histogram);
  virtual void SetInput( const InputHistogramObjectType *inputObject);
  const InputHistogramObjectType* GetInput(void);

                      
  /** Set the spacing (size of a pixel) of the image. 
   *  \sa GetSpacing() */
  itkSetMacro(Spacing,SpacingType);
  virtual void SetSpacing( const double values[ImageDimension] );


  /** Get the spacing (size of a pixel) of the image. 
   * For ImageBase and Image, the default data spacing is unity. */
  itkGetConstReferenceMacro(Spacing,SpacingType);

  /** Set the origin of the image. 
   * \sa GetOrigin() */
  itkSetMacro(Origin,PointType);
  virtual void SetOrigin( const double values[ImageDimension] );
 
 /** Get the origin of the image.  */
  itkGetConstReferenceMacro(Origin,PointType);

  /** Get the size of the histogram. */
  itkGetMacro(Size,HistogramSizeType);

protected:
  HistogramToImageFilter();
  ~HistogramToImageFilter();

  virtual void GenerateOutputInformation(){}; // do nothing
  virtual void GenerateData();

  SizeType        m_Size;
  SpacingType     m_Spacing;
  PointType       m_Origin;

  virtual void PrintSelf(std::ostream& os, Indent indent) const;

private:
  HistogramToImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented


};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHistogramToImageFilter.txx"
#endif

#endif
