/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeightedMeanCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkWeightedMeanCalculator_h
#define __itkWeightedMeanCalculator_h

#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>

#include "itkArray.h"
#include "itkVector.h"
#include "itkSampleAlgorithmBase.h"

namespace itk{ 
namespace Statistics{
  
/** \class WeightedMeanCalculator
 * \brief calculates sample mean where each measurement vector has
 * associated weight value
 *
 * You plug in the target sample data using SetSample method. Then call
 * the Update method to run the alogithm.
 *
 * \sa MeanCalculator SampleAlgorithmBase
 */

template< class TSample >
class WeightedMeanCalculator :
    public SampleAlgorithmBase< TSample >
{
public:
  /**Standard class typedefs. */
  typedef WeightedMeanCalculator Self;
  typedef SampleAlgorithmBase< TSample > Superclass ;
  typedef SmartPointer<Self> Pointer;

  /**Standard Macros */
  itkTypeMacro(WeightedMeanCalculator, SampleAlgorithmBase);
  itkNewMacro(Self) ;
  
  itkStaticConstMacro(MeasurementVectorSize, unsigned int,
                      TSample::MeasurementVectorSize);

  typedef Array< double > WeightArrayType ;
  /** Typedef for the mean output */
  typedef Vector< double, itkGetStaticConstMacro(MeasurementVectorSize) > OutputType ;
  
  void SetWeights(WeightArrayType* array) ;
  WeightArrayType* GetWeights() ;

  OutputType* GetOutput() ;

protected:
  WeightedMeanCalculator() ;
  virtual ~WeightedMeanCalculator() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Calculates the mean and save it */
  void GenerateData() ;

private:
  WeightArrayType* m_Weights ;
  OutputType m_Output ;
} ; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWeightedMeanCalculator.txx"
#endif

#endif

