/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMeanCalculator_h
#define __itkMeanCalculator_h

#include "itkSampleAlgorithmBase.h"

#include "itkVector.h"

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
 */

template< class TSample >
class MeanCalculator :
      public SampleAlgorithmBase< TSample >
{
public:
  /**Standard class typedefs. */
  typedef MeanCalculator Self;
  typedef Object Superclass ;
  typedef SmartPointer<Self> Pointer;

  /**Standard Macros */
  itkTypeMacro(MeanCalculator, Object);
  itkNewMacro(Self) ;
  
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);
  
  /** Typedef for the mean output */
  typedef Vector< double,
                  itkGetStaticConstMacro(MeasurementVectorSize) > OutputType ;

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

