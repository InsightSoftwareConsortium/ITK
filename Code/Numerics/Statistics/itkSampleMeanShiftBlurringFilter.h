/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleMeanShiftBlurringFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSampleMeanShiftBlurringFilter_h
#define __itkSampleMeanShiftBlurringFilter_h

#include "itkSampleAlgorithmBase.h"
#include "itkListSample.h"
#include "itkMeanShiftModeSeekerBase.h"

namespace itk{ 
namespace Statistics{
  
/** \class SampleMeanShiftBlurringFilter
 * \brief Calculates the covariance matrix of the target sample data.
 *
 */

template< class TSample >
class SampleMeanShiftBlurringFilter :
    public SampleAlgorithmBase< TSample >
{
public:
  /** Standard class typedefs. */
  typedef SampleMeanShiftBlurringFilter Self ;
  typedef SampleAlgorithmBase< TSample > Superclass ;
  typedef SmartPointer<Self> Pointer ;

  /** Standard Macros */
  itkTypeMacro(SampleMeanShiftBlurringFilter, SampleAlgorithmBase);
  itkNewMacro(Self) ;
  
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize) ;

  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;
  typedef typename Superclass::InputSampleType InputSampleType ;
  typedef ListSample< MeasurementVectorType > OutputType ;
  typedef MeanShiftModeSeekerBase< TSample > MeanShiftModeSeekerType ;

  /** Sets the mean shift evolving function */
  void SetMeanShiftModeSeeker(MeanShiftModeSeekerType* function) ;

  /** Sets the mean shift evolving function */
  MeanShiftModeSeekerType* GetMeanShiftModeSeeker()
  { return m_ModeSeeker ; }

  /** Returns the covariance matrix of the target sample data */ 
  OutputType* GetOutput() ;

protected:
  SampleMeanShiftBlurringFilter() ;
  virtual ~SampleMeanShiftBlurringFilter() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Blurring the input sample and creates a ListSample with blurred data */
  void GenerateData() ;

private:
  typename OutputType::Pointer m_Output ;
  MeanShiftModeSeekerType* m_ModeSeeker ;
} ; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSampleMeanShiftBlurringFilter.txx"
#endif

#endif

