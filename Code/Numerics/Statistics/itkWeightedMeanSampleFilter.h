/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeightedMeanSampleFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkWeightedMeanSampleFilter_h
#define __itkWeightedMeanSampleFilter_h

#include "itkMeanSampleFilter.h"
#include "itkFunctionBase.h"
#include "itkDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/** \class WeightedMeanSampleFilter
 * \brief Given a sample where each measurement vector has
 * associated weight value, this filter computes the sample mean
 *
 * To run this algorithm, you have plug in the target sample data
 * using SetInput method and provides weight by an array or function.
 *. Then call the Update method to run the alogithm.
 *
 * \sa MeanSampleFilter
 *
 */
template< class TSample >
class ITK_EXPORT WeightedMeanSampleFilter:public MeanSampleFilter< TSample >
{
public:
  /**Standard class typedefs. */
  typedef WeightedMeanSampleFilter    Self;
  typedef MeanSampleFilter< TSample > Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /**Standard Macros */
  itkTypeMacro(WeightedMeanSampleFilter, MeanSampleFilter);
  itkNewMacro(Self);

  /** Traits derived from the base class */
  itkSuperclassTraitMacro(SampleType)
  itkSuperclassTraitMacro(MeasurementType)
  itkSuperclassTraitMacro(MeasurementVectorType)
  itkSuperclassTraitMacro(MeasurementVectorSizeType)
  itkSuperclassTraitMacro(MeasurementVectorDecoratedType)
  itkSuperclassTraitMacro(OutputType)

  /** Array typedef for weights */
  typedef Array< double > WeightArrayType;

  /** Type of DataObjects to use for the weight array type */
  typedef SimpleDataObjectDecorator< WeightArrayType > InputWeightArrayObjectType;

  /** Method to set the input value of the weight array */
  itkSetDecoratedInputMacro(Weights, WeightArrayType, 1);

  /** Weight calculation function typedef */
  typedef FunctionBase< MeasurementVectorType, double > WeightingFunctionType;

  /** Type of DataObjects to use for Weight function */
  typedef DataObjectDecorator< WeightingFunctionType > InputWeightingFunctionObjectType;

  /** Method to set the weighting function */
  itkSetDecoratedObjectInputMacro(WeightingFunction, WeightingFunctionType, 2);
protected:
  WeightedMeanSampleFilter();
  virtual ~WeightedMeanSampleFilter();
  void PrintSelf(std::ostream & os, Indent indent) const;

  void GenerateData();

  // compute mean with weight array
  void ComputeMeanWithWeights();

  // compute mean using a weighting function
  void ComputeMeanWithWeightingFunction();

private:
  WeightedMeanSampleFilter(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented
};                                        // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWeightedMeanSampleFilter.txx"
#endif

#endif
