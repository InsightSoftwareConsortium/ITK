/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageToHistogramGenerator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkScalarImageToHistogramGenerator_h
#define __itkScalarImageToHistogramGenerator_h


#include "itkScalarImageToListAdaptor.h"
#include "itkListSampleToHistogramGenerator.h"
#include "itkObject.h"


namespace itk {
namespace Statistics {

template< class TImageType >
class ScalarImageToHistogramGenerator : public Object
{
public:
  /** Standard typedefs */
  typedef ScalarImageToHistogramGenerator  Self ;
  typedef Object Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ScalarImageToHistogramGenerator, Object) ;

  /** standard New() method support */
  itkNewMacro(Self) ;

  typedef TImageType                                      ImageType;
  typedef itk::Statistics::ScalarImageToListAdaptor< 
                                              ImageType 
                                                      >   AdaptorType;
  typedef typename AdaptorType::Pointer                   AdaptorPointer;
  typedef typename ImageType::PixelType                   PixelType;

  typedef itk::Statistics::ListSampleToHistogramGenerator< 
                                  AdaptorType, 
                                  PixelType,
                                  DenseFrequencyContainer
                                                          > GeneratorType;

  typedef typename GeneratorType::Pointer                   GeneratorPointer;

  typedef typename GeneratorType::HistogramType             HistogramType;
  typedef typename HistogramType::Pointer                   HistogramPointer;
  typedef typename HistogramType::ConstPointer              HistogramConstPointer;

public:

  /** Triggers the Computation of the histogram */
  void Compute( void );

  /** Connects the input image for which the histogram is going to be computed */
  void SetInput( const ImageType * );
  
  /** Return the histogram. o
   \warning This output is only valid after the Compute() method has been invoked 
   \sa Compute */
  const HistogramType * GetOutput() const;
  
  /** Set number of histogram bins */
  void SetNumberOfBins( unsigned int numberOfBins );
 
  /** Set marginal scale value to be passed to the histogram generator */
  void SetMarginalScale( double marginalScale );


protected:
  ScalarImageToHistogramGenerator();
  virtual ~ScalarImageToHistogramGenerator() {};
  void PrintSelf(std::ostream& os, Indent indent) const;


private:

  AdaptorPointer      m_ImageToListAdaptor;

  GeneratorPointer    m_HistogramGenerator;
  
  ScalarImageToHistogramGenerator(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};


} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarImageToHistogramGenerator.txx"
#endif

#endif
