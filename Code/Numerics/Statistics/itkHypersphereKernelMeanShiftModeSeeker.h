/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkHypersphereKernelMeanShiftModeSeeker.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkHypersphereKernelMeanShiftModeSeeker_h
#define __itkHypersphereKernelMeanShiftModeSeeker_h

#include "itkMeanShiftModeSeekerBase.h"

#include "itkFixedArray.h"
#include "itkNumericTraits.h"

namespace itk{ 
namespace Statistics{
  
/** \class HypersphereKernelMeanShiftModeSeeker
 * \brief Calculates the covariance matrix of the target sample data.
 *
 */

template< class TSample >
class HypersphereKernelMeanShiftModeSeeker :
    public MeanShiftModeSeekerBase< TSample >
{
public:
  /** Standard class typedefs. */
  typedef HypersphereKernelMeanShiftModeSeeker Self;
  typedef MeanShiftModeSeekerBase< TSample > Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Standard Macros */
  itkTypeMacro(HypersphereKernelMeanShiftModeSeeker, 
               MeanShiftModeSeekerBase);
  itkNewMacro(Self) ;
  
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType ;
  typedef typename Superclass::SearchResultVectorType SearchResultVectorType ;
  typedef typename Superclass::MeasurementType MeasurementType ;

  itkStaticConstMacro( MeasurementVectorSize, unsigned int,
                       MeasurementVectorType::Length ) ;

  typedef double RealMeasurementType ;
  typedef FixedArray< RealMeasurementType,
                      itkGetStaticConstMacro( MeasurementVectorSize ) > 
  MeasurementVectorSumType ;

  void SetSearchRadius(double radius) ;
  
  double GetSearchRadius()
  { return m_SearchRadius ; }

  /** Returns the covariance matrix of the target sample data */ 
  MeasurementVectorType Evolve(MeasurementVectorType instance) ;
  
protected:
  HypersphereKernelMeanShiftModeSeeker() ;
  virtual ~HypersphereKernelMeanShiftModeSeeker() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

  inline void ComputeMode(MeasurementVectorType queryPoint,
                   MeasurementVectorType& newPoint) ;

private:
  double m_SearchRadius ;
  MeasurementVectorSumType m_TempVectorSum ;
  MeasurementVectorType m_TempVector ;
} ; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHypersphereKernelMeanShiftModeSeeker.txx"
#endif

#endif

