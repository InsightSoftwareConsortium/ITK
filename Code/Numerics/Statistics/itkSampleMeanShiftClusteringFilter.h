/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleMeanShiftClusteringFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSampleMeanShiftClusteringFilter_h
#define __itkSampleMeanShiftClusteringFilter_h

#include <vector>
#include "itkSampleAlgorithmBase.h"

namespace itk{ 
namespace Statistics{
  
/** \class SampleMeanShiftClusteringFilter
 * \brief Calculates the covariance matrix of the target sample data.
 *
 */

template< class TSample >
class SampleMeanShiftClusteringFilter :
    public SampleAlgorithmBase< TSample >
{
public:
  /** Standard class typedefs. */
  typedef SampleMeanShiftClusteringFilter Self;
  typedef SampleAlgorithmBase< TSample > Superclass ;
  typedef SmartPointer<Self> Pointer;

  /** Standard Macros */
  itkTypeMacro(SampleMeanShiftClusteringFilter, 
               SampleAlgorithmBase);
  itkNewMacro(Self) ;
  
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;
  typedef typename TSample::MeasurementType MeasurementType ;
  typedef typename TSample::InstanceIdentifier InstanceIdentifier ;

  typedef std::vector< InstanceIdentifier > SearchResultVectorType ;
  typedef std::vector< int > ClusterLabelsType ;

  itkSetMacro(Threshold, double) ;
  itkGetMacro(Threshold, double) ;
  itkSetMacro(MinimumClusterSize, unsigned long) ;
  itkGetMacro(MinimumClusterSize, unsigned long) ;

  /** Returns the covariance matrix of the target sample data */ 
  MeasurementVectorType Evolve(MeasurementVectorType instance) ;
  
  ClusterLabelsType& GetOutput()
  { return m_Output ; }

protected:
  SampleMeanShiftClusteringFilter() ;
  virtual ~SampleMeanShiftClusteringFilter() ;
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData() ;

private:
  double m_Threshold ;
  ClusterLabelsType m_Output ;
  MeasurementVectorType m_TempQueryPoint ;
  SearchResultVectorType m_TempSearchResult ;
  unsigned long m_MinimumClusterSize ;
} ; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSampleMeanShiftClusteringFilter.txx"
#endif

#endif

