/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeightedMeanCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
#include "itkFunctionBase.h"
#include "itkSampleAlgorithmBase.h"

namespace itk{ 
namespace Statistics{
  
/** \class WeightedMeanCalculator
 * \brief calculates sample mean where each measurement vector has
 * associated weight value
 *
 * To run this algorithm, you have plug in the target sample data 
 * using SetInputSample method and provides weight by an array or function.
 *. Then call the Update method to run the alogithm.
 * 
 * Recent API changes:
 * The static const macro to get the length of a measurement vector,
 * 'MeasurementVectorSize'  has been removed to allow the length of a measurement
 * vector to be specified at run time. It is now obtained from the input sample.
 * Please use the function GetMeasurementVectorSize() to obtain the length. 
 * The mean output is an Array rather than a Vector.

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
  typedef SmartPointer<const Self> ConstPointer;

  /**Standard Macros */
  itkTypeMacro(WeightedMeanCalculator, SampleAlgorithmBase);
  itkNewMacro(Self) ;
  
  /** Length of a measurement vector */
  typedef typename Superclass::MeasurementVectorSizeType MeasurementVectorSizeType;
  
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;

  /** Typedef for the mean output */
  typedef Array< double >           OutputType;
  
  /** Array typedef for weights */
  typedef Array< double > WeightArrayType ;

  /** Sets the weights using an array */
  void SetWeights(WeightArrayType* array) ;
  
  /** Gets the weights array */
  WeightArrayType* GetWeights() ;

  /** Weight calculation function typedef */
  typedef FunctionBase< MeasurementVectorType, double > WeightFunctionType ;

  /** Sets the wiehts using an function
   * the function should have a method, 
   * Evaluate(MeasurementVectorType&) */
  void SetWeightFunction(WeightFunctionType* func) ;

  /** Gets the weight function */
  WeightFunctionType* GetWeightFunction() ;

  /** Returns the mean vector as the result */
  OutputType* GetOutput() ;

protected:
  WeightedMeanCalculator() ;
  virtual ~WeightedMeanCalculator() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Calculates the mean and save it */
  void GenerateData() ;

private:
  WeightArrayType* m_Weights ;
  WeightFunctionType* m_WeightFunction ;
  OutputType m_Output ;
} ; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWeightedMeanCalculator.txx"
#endif

#endif

