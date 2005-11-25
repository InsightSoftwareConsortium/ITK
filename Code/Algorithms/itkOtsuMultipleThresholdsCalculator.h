/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOtsuMultipleThresholdsCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkOtsuMultipleThresholdsCalculator_h
#define __itkOtsuMultipleThresholdsCalculator_h

#include "itkHistogramAlgorithmBase.h"
#include "itkHistogram.h"

namespace itk
{

/** \class OtsuMultipleThresholdsCalculator
 * \brief Computes Otsu's thresholds for a histogram.
 * 
 * You plug in the target histogram using SetInputHistogram method and 
 * specify the number of thresholds you want to be computed. Then call
 * the GenerateData method to run the alogithm. 
 *
 * The thresholds are computed so that the between-class variance is
 * maximized.
 *
 * \ingroup Calculators
 */

template< class TInputHistogram >
class OtsuMultipleThresholdsCalculator :
      public HistogramAlgorithmBase< TInputHistogram >
{
public:
  /**Standard class typedefs. */
  typedef OtsuMultipleThresholdsCalculator Self;
  typedef HistogramAlgorithmBase<TInputHistogram> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  typedef typename TInputHistogram::MeasurementType MeasurementType;
  typedef typename TInputHistogram::FrequencyType FrequencyType;

  typedef typename NumericTraits<MeasurementType>::RealType MeanType;
  typedef typename NumericTraits<MeasurementType>::RealType VarianceType;

  typedef std::vector<MeanType> MeanVectorType;
  typedef std::vector<FrequencyType> FrequencyVectorType;

  typedef typename TInputHistogram::InstanceIdentifier InstanceIdentifierType;
  typedef std::vector<InstanceIdentifierType> InstanceIdentifierVectorType;

  /**Standard Macros */
  itkTypeMacro(OtsuMultipleThresholdsCalculator, HistogramAlgorithmsBase);
  itkNewMacro(Self) ;
                                                                                                                                      
  /** Typedef for the thresholds output */
  typedef std::vector<MeasurementType> OutputType ;
                                                                                                                                      
  /** Returns the thresholds vector */
  const OutputType& GetOutput() ;

  /** Set/Get the number of thresholds. */
  itkSetClampMacro(NumberOfThresholds, unsigned long, 1, NumericTraits<unsigned long>::max() );
  itkGetMacro(NumberOfThresholds,unsigned long);
                                             
protected:
  OtsuMultipleThresholdsCalculator() ;
  virtual ~OtsuMultipleThresholdsCalculator() {}
  void PrintSelf(std::ostream& os, Indent indent) const;
                                                                                                                                      
  /** Calculates the thresholds and save them */
  void GenerateData() ;
                         
  /** Increment the thresholds of one position */
  bool IncrementThresholds(InstanceIdentifierVectorType& thresholdIds, MeanType totalMean, MeanVectorType& classMean, FrequencyVectorType& classFrequency);

private:
  /** Internal thresholds storage */
  unsigned long m_NumberOfThresholds;
  OutputType m_Output ;

} ; // end of class
                                                                                                                                      
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOtsuMultipleThresholdsCalculator.txx"
#endif

#endif
