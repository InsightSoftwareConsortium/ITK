/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleMeanShiftClusteringFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkSampleMeanShiftClusteringFilter_h
#define __itkSampleMeanShiftClusteringFilter_h

#include <vector>
#include "itkSampleAlgorithmBase.h"

namespace itk { 
namespace Statistics {
  
/** \class SampleMeanShiftClusteringFilter
 * \brief This filter create a cluster map from an input sample.
 *
 * The clustering process is done by linking measurement vectors whose
 * distance is below the threshold value set by the SetThreshold
 * method. However, if the resulting cluster size is below the minimum
 * number of measurement vectors set by the MinimumClusterSize
 * method. Such measurement vectors are clustered together and
 * labelled zero. 
 *
 * \sa SampleMeanShiftBlurringFilter, SampleSelectiveMeanShiftBlurringFilter
 */

template< class TSample >
class SampleMeanShiftClusteringFilter :
    public SampleAlgorithmBase< TSample >
{
public:
  /** Standard class typedefs. */
  typedef SampleMeanShiftClusteringFilter Self;
  typedef SampleAlgorithmBase< TSample >  Superclass;
  typedef SmartPointer<Self>              Pointer;
  typedef SmartPointer<const Self>        ConstPointer;

  /** Standard Macros */
  itkTypeMacro(SampleMeanShiftClusteringFilter, 
               SampleAlgorithmBase);
  itkNewMacro(Self);
  
  typedef typename TSample::MeasurementVectorType MeasurementVectorType;
  typedef typename TSample::MeasurementType       MeasurementType;
  typedef typename TSample::InstanceIdentifier    InstanceIdentifier;

  typedef std::vector< InstanceIdentifier > SearchResultVectorType;
  typedef std::vector< int >                ClusterLabelsType;

  itkSetMacro(Threshold, double);
  itkGetMacro(Threshold, double);
  itkSetMacro(MinimumClusterSize, unsigned long);
  itkGetMacro(MinimumClusterSize, unsigned long);

  ClusterLabelsType& GetOutput()
    { return m_Output; }

protected:
  SampleMeanShiftClusteringFilter();
  virtual ~SampleMeanShiftClusteringFilter();
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData();

private:
  double                 m_Threshold;
  ClusterLabelsType      m_Output;
  MeasurementVectorType  m_TempQueryPoint;
  SearchResultVectorType m_TempSearchResult;
  unsigned long          m_MinimumClusterSize;
}; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSampleMeanShiftClusteringFilter.txx"
#endif

#endif
