/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianMixtureModelComponent.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkGaussianMixtureModelComponent_h
#define __itkGaussianMixtureModelComponent_h

#include "itkMixtureModelComponentBase.h"
#include "itkGaussianDensityFunction.h"
#include "itkWeightedMeanCalculator.h"
#include "itkWeightedCovarianceCalculator.h"

namespace itk{ 
namespace Statistics{
  
/** \class GaussianMixtureModelComponent
 * \brief is a component (derived from MixtureModelComponentBase) for
 * Gaussian class. This class is used in
 * ExpectationMaximizationMixtureModelEstimator. 
 *
 * On every iteration of EM estimation, this class's GenerateData
 * method is called to compute the new distribution parameters.
 *
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * \c MeasurementVectorSize  has been removed to allow the length of a measurement
 * vector to be specified at run time. It is now obtained at run time from the
 * sample set as input. Please use the function 
 * GetMeasurementVectorSize() to get the length. 
 * 
 * \sa MixtureModelComponentBase, ExpectationMaximizationMixtureModelEstimator
 */

template< class TSample >
class GaussianMixtureModelComponent :
    public MixtureModelComponentBase< TSample >
{
public:
  /**Standard class typedefs. */
  typedef GaussianMixtureModelComponent Self;
  typedef MixtureModelComponentBase< TSample > Superclass ;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**Standard Macros */
  itkTypeMacro(GaussianMixtureModelComponent, MixtureModelComponentBase);
  itkNewMacro(Self) ;


  /** Typedefs from the superclass */
  typedef typename Superclass::MeasurementVectorType MeasurementVectorType ;
  typedef typename Superclass::MeasurementVectorSizeType MeasurementVectorSizeType ;
  typedef typename Superclass::MembershipFunctionType MembershipFunctionType ;
  typedef typename Superclass::WeightArrayType WeightArrayType ;
  typedef typename Superclass::ParametersType ParametersType ;

  /** Type of the membership function. Gaussian density function */
  typedef GaussianDensityFunction< MeasurementVectorType > 
  NativeMembershipFunctionType ;
  
  /** Types of the mean and the covariance calculator that will update
   *  this component's distribution parameters */
  typedef WeightedMeanCalculator< TSample > MeanEstimatorType ;
  typedef WeightedCovarianceCalculator< TSample > CovarianceEstimatorType ;

  /** Type of the mean vector */
  typedef typename MeanEstimatorType::OutputType MeanType ;

  /** Type of the covariance matrix */
  typedef typename CovarianceEstimatorType::OutputType CovarianceType ;

  /** Sets the input sample */
  void SetSample(const TSample* sample) ;

  /** Sets the component's distribution parameters. */
  void SetParameters(const ParametersType &parameters) ;
  
protected:
  GaussianMixtureModelComponent() ;
  virtual ~GaussianMixtureModelComponent() {}
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Returns the sum of squared changes in parameters between
   * iterations */
  double CalculateParametersChange() ;

  /** Computes the new distribution parameters */
  void GenerateData() ;

private:
  typename NativeMembershipFunctionType::Pointer m_GaussianDensityFunction ;
  MeanType m_Mean ;
  CovarianceType m_Covariance ;
  typename MeanEstimatorType::Pointer m_MeanEstimator ;
  typename CovarianceEstimatorType::Pointer m_CovarianceEstimator ;
} ; // end of class
    
} // end of namespace Statistics 
} // end of namespace itk 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianMixtureModelComponent.txx"
#endif

#endif

