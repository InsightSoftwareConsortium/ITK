/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanShiftModeSeekerBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMeanShiftModeSeekerBase_h
#define __itkMeanShiftModeSeekerBase_h

#include "itkObject.h"
#include "itkVector.h"
#include "itkMatrix.h"
#include "itkNumericTraits.h"
#include "itkMeanShiftModeCacheMethod.h"

namespace itk{ 
namespace Statistics{
  
/** \class MeanShiftModeSeekerBase
 * \brief Calculates the covariance matrix of the target sample data.
 *
 */

template< class TSample >
class MeanShiftModeSeekerBase :
    public Object
{
public:
  /** Standard class typedefs. */
  typedef MeanShiftModeSeekerBase Self;
  typedef Object Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Standard Macros */
  itkTypeMacro(MeanShiftModeSeekerBase, Object);
  itkNewMacro(Self) ;
  
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize) ;

  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;
  typedef typename TSample::MeasurementType MeasurementType ;
  typedef typename TSample::InstanceIdentifier InstanceIdentifier ;
  typedef std::vector< InstanceIdentifier > SearchResultVectorType ;
  typedef MeanShiftModeCacheMethod< MeasurementVectorType > CacheMethodType ;

  void SetInputSample(TSample* sample) ;

  TSample* GetInputSample()
  { return m_InputSample.GetPointer() ; }

  void SetMaximumIteration(unsigned int number)
  { m_MaximumIteration = number ; }

  unsigned int GetMaximumIteration()
  { return m_MaximumIteration ; }

  void SetCacheMethod(CacheMethodType* method) ;

  CacheMethodType* GetCacheMethod()
  { return m_CacheMethod.GetPointer() ; }

  /** Returns the covariance matrix of the target sample data */ 
  MeasurementVectorType Evolve(MeasurementVectorType instance) ;

protected:
  MeanShiftModeSeekerBase() ;
  virtual ~MeanShiftModeSeekerBase() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

  inline virtual void ComputeMode(MeasurementVectorType queryPoint,
                           MeasurementVectorType& newPoint) = 0 ;

  
private:
  typename TSample::Pointer m_InputSample ;
  unsigned int m_MaximumIteration ;
  typename CacheMethodType::Pointer m_CacheMethod ;
} ; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanShiftModeSeekerBase.txx"
#endif

#endif

