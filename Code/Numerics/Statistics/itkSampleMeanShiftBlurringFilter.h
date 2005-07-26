/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleMeanShiftBlurringFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
 * \brief This filter blurs the input sample data using mean shift
 * algorithm.
 *
 * The mode seeking part is done by any class derived from the
 * MeanShiftModeSeekerBase. Users should set the input sample using
 * the SetInputSample method of the superclass and set the mean shift
 * mode seeker using the SetMeanShiftModeSeeker method.
 *
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * \c MeasurementVectorSize  has been removed to allow the length of a measurement
 * vector to be specified at run time. It is now obtained at run time from the
 * sample set as input. Please use the function 
 * GetMeasurementVectorSize() to get the length.
 *
 * \sa SampleMeanShiftClusteringFilter, MeanShiftModeSeekerBase
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
  typedef SmartPointer<const Self> ConstPointer;

  /** Standard Macros */
  itkTypeMacro(SampleMeanShiftBlurringFilter, SampleAlgorithmBase);
  itkNewMacro(Self) ;
  

  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;
  typedef typename Superclass::InputSampleType InputSampleType ;
  typedef ListSample< MeasurementVectorType > OutputType ;
  typedef MeanShiftModeSeekerBase< TSample > MeanShiftModeSeekerType ;

  /** Set/Gets the mean shift evolving function */
  void SetMeanShiftModeSeeker(MeanShiftModeSeekerType* function) ;
  MeanShiftModeSeekerType* GetMeanShiftModeSeeker()
  { return m_ModeSeeker ; }

  /** Returns the blurred sample data in a ListSample object */
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

