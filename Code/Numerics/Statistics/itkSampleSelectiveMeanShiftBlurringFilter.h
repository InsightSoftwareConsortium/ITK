/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleSelectiveMeanShiftBlurringFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSampleSelectiveMeanShiftBlurringFilter_h
#define __itkSampleSelectiveMeanShiftBlurringFilter_h

#include "itkSampleMeanShiftBlurringFilter.h"

namespace itk{ 
namespace Statistics{
  
/** \class SampleSelectiveMeanShiftBlurringFilter
 * \brief Calculates the covariance matrix of the target sample data.
 *
 */

template< class TSample >
class SampleSelectiveMeanShiftBlurringFilter :
    public SampleMeanShiftBlurringFilter< TSample >
{
public:
  /** Standard class typedefs. */
  typedef SampleSelectiveMeanShiftBlurringFilter Self;
  typedef SampleMeanShiftBlurringFilter< TSample > Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Standard Macros */
  itkTypeMacro(SampleSelectiveMeanShiftBlurringFilter, 
               SampleMeanShiftBlurringFilter);
  itkNewMacro(Self) ;
  
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize) ;

  typedef typename Superclass::MeasurementVectorType MeasurementVectorType ;
  typedef typename Superclass::OutputType OutputType ;
  typedef typename Superclass::MeanShiftModeSeekerType 
  MeanShiftModeSeekerType ;
  
  typedef Vector< bool, itkGetStaticConstMacro(MeasurementVectorSize) >
  ComponentSelectionsType ;

  void SetComponentSelections(ComponentSelectionsType selections) ;

  void GetComponentSelections()
  { return m_ComponentSelections ; }

protected:
  SampleSelectiveMeanShiftBlurringFilter() ;
  virtual ~SampleSelectiveMeanShiftBlurringFilter() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Blurring the input sample and creates a ListSample with blurred data */
  void GenerateData() ;

private:
  ComponentSelectionsType m_ComponentSelections ;
} ; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSampleSelectiveMeanShiftBlurringFilter.txx"
#endif

#endif

