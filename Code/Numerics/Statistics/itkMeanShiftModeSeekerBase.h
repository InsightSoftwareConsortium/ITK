/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanShiftModeSeekerBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
 * \brief Evolves the mode. This is the base class for any mean shift
 * mode seeking algorithm classes.
 *
 * Any subclass of this class should implement the ComputeMode
 * method. That is the only one requirement. To use this class, user
 * should plug in the input sample using SetInputSample, then call
 * Evolve function with a measurement vector (query point).
 *
 * There are some operational options. Users can set a cache method to
 * accelates the evolving process. If the cache method already has a
 * pair of a query point and its new mode point, the ComputeMode uses
 * the cached value insteady recalculating the mode. By setting the
 * maximum iteration number (SetMaximumIteration method), when the
 * evolving process exceedes, the process will stop and return the
 * current mode point as the result. With this option turned off (by
 * setting it to 0 or leave it alone after instantiating this class), the
 * evolving process runs until it converges.
 *
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * \c MeasurementVectorSize  has been removed to allow the length of a measurement
 * vector to be specified at run time. It is now obtained at run time from the
 * sample set as input. 
 *
 * \sa MeanShiftModeCacheMethod, SampleMeanShiftBlurringFilter,
 * SampleSelectiveMeanShiftBlurringFilter, SampleMeanShiftClusteringFilter
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
  typedef SmartPointer<const Self> ConstPointer;

  /** Standard Macros */
  itkTypeMacro(MeanShiftModeSeekerBase, Object);
  
  /** Typedefs from the TSample template argument */
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;
  typedef typename TSample::MeasurementVectorSizeType MeasurementVectorSizeType ;
  typedef typename TSample::MeasurementType MeasurementType ;
  typedef typename TSample::InstanceIdentifier InstanceIdentifier ;

  typedef std::vector< InstanceIdentifier > SearchResultVectorType ;
  typedef MeanShiftModeCacheMethod< MeasurementVectorType > CacheMethodType ;

  void SetInputSample(const TSample* sample) ;

  const TSample* GetInputSample() const
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

  /** Get the length of a measurement vector */
  virtual MeasurementVectorSizeType GetMeasurementVectorSize() const
    {
    if( m_InputSample.GetPointer() )
      {
      return m_InputSample.GetPointer()->GetMeasurementVectorSize();
      }
    else
      {
      return 0;
      }
    }

protected:
  MeanShiftModeSeekerBase() ;
  virtual ~MeanShiftModeSeekerBase() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

  virtual bool ComputeMode(MeasurementVectorType queryPoint,
                           MeasurementVectorType& newPoint) = 0 ;

  
private:
  typename TSample::ConstPointer m_InputSample ;
  unsigned int m_MaximumIteration ;
  typename CacheMethodType::Pointer m_CacheMethod ;
} ; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanShiftModeSeekerBase.txx"
#endif

#endif

