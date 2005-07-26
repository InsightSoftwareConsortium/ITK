/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeanShiftModeCacheMethod.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
#include "itkMeasurementVectorTraits.h"

namespace itk{ 
namespace Statistics{
  
/** \class MeanShiftModeCacheMethod
 * \brief This class stores mappings between a query point and its
 * resulting mode point.
 *
 * To increase the mean shift mode search performance, this class
 * stores mappings between a query point (starting position of search)
 * and the result so that if there is a mapping stored for a specific
 * query point, mode seeker can return the resulting mode point stored
 * in this class.
 *
 * You can specify how many mappings stored in this class using the
 * SetMaximumEntries method. The cache is destroyed and the rebuild
 * starts when the hit ratio (the number of successful mapping found
 * divided by the number of failure) is below the hit ratio threshold
 * set by the SetHitRatioThreshold method or the number of consecutive
 * failure exceeds the limit set by the SetMaximumConsecutiveFailures
 * method.
 *
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * \c MeasurementVectorSize  has been removed to allow the length of a measurement
 * vector to be specified at run time. It is now obtained at run time from the
 * measurement vectors.
 * 
 * \sa MeanShiftModeSeekerBase
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
  typedef SmartPointer<const Self> ConstPointer;

  /** Standard Macros */
  itkTypeMacro(MeanShiftModeCacheMethod, Object);
  itkNewMacro(Self) ;
  
  typedef TMeasurementVector MeasurementVectorType ;

  struct LessMeasurementVector
  {
    bool operator()(const MeasurementVectorType& mv1, 
                    const MeasurementVectorType& mv2) const
    {
      // It is assumed that mv1 and mv2 are of the same length. For efficieny,
      // no checking is performed here.
      for ( unsigned int i = 0 ; 
            i < MeasurementVectorTraits::GetLength( &mv1 );
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

