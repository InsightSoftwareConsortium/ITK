/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanShiftModeCacheMethod.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMeanShiftModeCacheMethod_h
#define __itkMeanShiftModeCacheMethod_h

#include <map>
#include "itkMacro.h"
#include "itkObject.h"

namespace itk{ 
namespace Statistics{
  
/** \class MeanShiftModeCacheMethod
 * \brief Calculates the covariance matrix of the target sample data.
 *
 */

template< class TMeasurementVector >
class MeanShiftModeCacheMethod :
    public Object
{
public:
  /** Standard class typedefs. */
  typedef MeanShiftModeCacheMethod Self;
  typedef Object Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Standard Macros */
  itkTypeMacro(MeanShiftModeCacheMethod, Object);
  itkNewMacro(Self) ;
  
  typedef TMeasurementVector MeasurementVectorType ;

  itkStaticConstMacro(MeasurementVectorSize, unsigned int, 
                      MeasurementVectorType::Length) ;

  struct LessMeasurementVector
  {
    bool operator()(const MeasurementVectorType& mv1, 
                    const MeasurementVectorType& mv2) const
    {
      for ( unsigned int i = 0 ; 
            i < itkGetStaticConstMacro( MeasurementVectorSize ) ;
            ++i )
        {
        if (mv1[i] < mv2[i])
          {
          return true ;
          }
        }
      return false ;
    }
  } ; // end of struct

  typedef std::map< MeasurementVectorType, MeasurementVectorType, LessMeasurementVector > CacheTableType ;

  void SetMaximumConsecutiveFailures(unsigned int number)
  { m_MaximumConsecutiveFailures = number ; }

  unsigned int GetMaximumConsecutiveFailures()
  { return m_MaximumConsecutiveFailures ; }
  
  void SetHitRatioThreshold(float threshold)
  { m_HitRatioThreshold = threshold ; }

  void SetMaximumEntries(unsigned int number)
  { m_MaximumEntries = number ; }

  unsigned int GetMaximumEntries()
  { return m_MaximumEntries ; }

  bool SetMeasurementVector(MeasurementVectorType& source, 
                            MeasurementVectorType& target) ;

  bool GetMeasurementVector(MeasurementVectorType& source,
                            MeasurementVectorType& target) ;

  bool IsFull() ;

  void DestroyCacheTable() ;

protected:
  MeanShiftModeCacheMethod() ;
  virtual ~MeanShiftModeCacheMethod() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  unsigned int m_MaximumEntries ;
  float m_HitRatioThreshold ;
  unsigned int m_MaximumConsecutiveFailures ;

  unsigned int m_NumberOfRequests ;
  unsigned int m_ConsecutiveFailures ;
  unsigned int m_HitsSuccess ;

  unsigned long m_TotalHitsSuccess ;
  unsigned long m_TotalHitsFailure ;

  unsigned long m_TotalTableSize ;
  unsigned int m_TimesOfRebuilding ;

  unsigned int m_TimesOfRebuildingByHitRatio ;
  unsigned int m_TimesOfRebuildingByConsecutiveFailures ;

  CacheTableType m_CacheTable ;
} ; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMeanShiftModeCacheMethod.txx"
#endif

#endif

