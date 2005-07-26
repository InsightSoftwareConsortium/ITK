/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMeanCalculator_h
#define __itkMeanCalculator_h

#include "itkSampleAlgorithmBase.h"

#include "itkArray.h"

namespace itk{ 
  namespace Statistics{
  
/** \class MeanCalculator
 * \brief calculates sample mean
 *
 * You plug in the target sample data using SetSample method. Then call
 * the GenerateData method to run the alogithm.
 *
 * The return value that the GetOutput method 
 * \f$ = \frac{1}{n}\sum^{n}_{i=1}x_{i}\f$ where \f$n\f$ is the
 * number of measurement vectors in the target 
 * 
 * Recent API changes:
 * The static const macro to get the length of a measurement vector,
 * 'MeasurementVectorSize'  has been removed to allow the length of a measurement
 * vector to be specified at run time. It is now obtained from the input sample.
 * Please use the function GetMeasurementVectorSize() to obtain the length. 
 * The mean output is an Array rather than a Vector.
 */

template< class TSample >
class MeanCalculator :
      public SampleAlgorithmBase< TSample >
{
public:
  /**Standard class typedefs. */
  typedef MeanCalculator Self;
  typedef SampleAlgorithmBase< TSample >  Superclass ;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**Standard Macros */
  itkTypeMacro(MeanCalculator, SampleAlgorithmBase);
  itkNewMacro(Self) ;

  /** Length of a measurement vector */
  typedef typename Superclass::MeasurementVectorSizeType MeasurementVectorSizeType;

  /** Measurement vector type */
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType;
  
  /** Typedef for the mean output */
  typedef Array< double >           OutputType;

  /** Returns the mean vector */
  OutputType* GetOutput() ;

protected:
  MeanCalculator() ;
  virtual ~MeanCalculator() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Calculates the mean and save it */
  void GenerateData() ;

private:
  /** Internal mean value storage */
  OutputType m_Output ;
} ; // end of class
    
  } // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanCalculator.txx"
#endif

#endif

