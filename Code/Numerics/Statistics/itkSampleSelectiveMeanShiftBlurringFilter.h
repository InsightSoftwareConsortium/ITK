/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleSelectiveMeanShiftBlurringFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSampleSelectiveMeanShiftBlurringFilter_h
#define __itkSampleSelectiveMeanShiftBlurringFilter_h

#include "itkSampleMeanShiftBlurringFilter.h"
#include "itkArray.h"
#include <vector>

namespace itk{ 
namespace Statistics{
  
/** \class SampleSelectiveMeanShiftBlurringFilter
 * \brief This filter blurs the input sample data using mean shift
 * algorithm selectively.
 *
 * The blurring process is identical to that of the
 * SampleMeanShiftBlurringFilter except that users can specify which
 * components of a measurement vector is affected by the blurring.
 * The mean mode seeking behavior is not changed. The component
 * selection affects only the generation of the final mode point. To
 * specify the components to be blurred, users use the
 * SetComponentSelections method. The size of the selection vector
 * should be same as the size of a measurement vector. Only the
 * components of the selection vector whose flag value is true will be
 * blurred.
 * 
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * \c MeasurementVectorSize  has been removed to allow the length of a measurement
 * vector to be specified at run time. Please use the function 
 * GetMeasurementVectorSize() instead. It is now obtained at run time from the
 * sample set as input. The typedef for \c ComponentSelectionsType
 * has changed from FixedArray to \c std::vector.
 *
 * \sa MeanModeSeekerBase, SampleMeanShiftBlurringFilter
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
  typedef SmartPointer<const Self> ConstPointer;

  /** Standard Macros */
  itkTypeMacro(SampleSelectiveMeanShiftBlurringFilter, 
               SampleMeanShiftBlurringFilter);
  itkNewMacro(Self) ;
  
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType ;
  typedef typename Superclass::OutputType OutputType ;
  typedef typename Superclass::MeanShiftModeSeekerType 
  MeanShiftModeSeekerType ;
  

  /** Typedef for selecting components to be blurred */
  typedef std::vector< bool > ComponentSelectionsType; 

  /** Set/Gets the vector of flags that indicate which components
   * are selected for blurring */
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

