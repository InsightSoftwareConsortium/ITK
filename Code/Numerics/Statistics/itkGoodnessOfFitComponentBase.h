/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGoodnessOfFitComponentBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGoodnessOfFitComponentBase_h
#define __itkGoodnessOfFitComponentBase_h

#include "itkObject.h"
#include "itkArray.h"
#include "itkHistogram.h"
#include "itkFunctionBase.h"
#include "itkNeighborhoodSampler.h"
#include "itkSampleToHistogramProjectionFilter.h"

namespace itk{ 
namespace Statistics{

/** \class GoodnessOfFitComponentBase 
 *  \brief provides component (module) type specific functionalities 
 *  for GoodnessOfFitMixtureModelCostFunction.
 *
 * This defines common iterfaces for each subclasses and provides common
 * functionalities across different types of components 
 * (e.g. GaussianGoodnessOfFitComponent)
 *
 * The primary role of a GoodnessOfFitComponent is to create an 1D histogram
 * (called an observed histogram ) of the input sample 
 * after resampling the input using spherical kernel and projecting the resampled
 * sample along base axes, and to create corresponding 1D histogram 
 * (expected histogram) that has the same histogram configuration as 
 * the observed histogram but has the expected frequencies from the given 
 * component parameters.
 *
 * You can set up the two histograms' configuration by calling 
 * SetHistogramNumberOfBins, SetHistogramUseEquiProbableBins, 
 * SetHistogramBingOverlap, SetHistogramExtent. After you change
 * the histogram configuration, call the CreateHistograms method to
 * allocate histograms. If you set the UseExpectedHistogram flag to false
 * by callsing SetUseExpectedHistogram(false), the CreateHistograms method
 * won't create the expected histogram. The decision should be made by the
 * GoodnessOfFitFunction object that will be plugged-in to the 
 * GoodnessOfFitMixtureModelCostFunction. So the method shouldn't be called
 * manually.
 *
 * This base class provides default implementations for the resampling and 
 * projection using helper classes (NeighborhoodSampler, 
 * SampleToHistogramProjectionFilter).
 *
 * To determine the base axis, call the CalculateProjectionAxes method.
 * each subclass should implement it.
 *
 * Another group of functions includes probability function such as
 * GetProbabilityDensity(MeasurementVectorType) 
 *     multivariate probability density function of the subclass  
 * GetCumulativeProbability(MeasurementType)
 *     univariate cumulative probabilty function
 * 
 * To see how all this methods are used in order, take a look at the
 * implementation of the GetValue method of the 
 * GoodnessOfFitMixtureModelCostFunction class.
 * 
 * \sa GoodnessOfFitMixtureModelCostFunction, GoodnessOfFitFunctionBase, 
 * GaussianGoodnessOfFitComponent, NeighborhoodSampler, 
 * SampleToHistogramProjectionFilter
 */

template< class TInputSample >
class GoodnessOfFitComponentBase 
  : public Object 
{
public:
  /** Standard class typedefs */
  typedef GoodnessOfFitComponentBase Self;
  typedef Object Superclass;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(GoodnessOfFitComponentBase, Object) ;
  
  typedef TInputSample InputSampleType ;
  enum { MeasurementVectorSize = TInputSample::MeasurementVectorSize } ;
  typedef typename TInputSample::MeasurementType MeasurementType ;
  typedef typename TInputSample::MeasurementVectorType MeasurementVectorType ;

  /** Resample() output type */
  typedef Subsample< TInputSample > ResampledSampleType ;


  /** Histogram type that will be used for observed and expected histogram*/
  typedef Histogram< float, 1 > HistogramType ;

  typedef Array< double > ParametersType ;

  typedef FixedArray< double, MeasurementVectorSize > CenterType ;
  typedef Vector< double, MeasurementVectorSize > MeanType ;
  typedef double RadiusType ;
  typedef double StandardDeviationType ;

  virtual void SetInputSample(TInputSample* sample) ;

  TInputSample* GetInputSample() ;

  virtual unsigned int GetNumberOfParameters() const = 0 ;

  virtual void SetParameters(const ParametersType &parameters) ;

  ParametersType* GetParameters()
  { return m_Parameters ; }

  void SetUseExpectedHistogram(bool flag) ;

  void SetHistogramNumberOfBins(int numberOfBins) ;

  int GetHistogramNumberOfBins()
  { return m_HistogramNumberOfBins ; }

  void SetHistogramUseEquiProbableBins(bool flag) ;

  bool GetHistogramUseEquiProbableBins()
  { return m_HistogramUseEquiProbableBins ; }

  void SetHistogramBinOverlap(double overlap) ;

  double GetHistogramBinOverlap()
  { return m_HistogramBinOverlap ; }

  void SetHistogramExtent(double extent) ;

  double GetHistogramExtent()
  { return m_HistogramExtent ; }

  virtual CenterType* GetCenter() = 0 ;
  
  virtual RadiusType* GetRadius() = 0 ;

  virtual MeanType* GetMean() = 0 ;

  virtual RadiusType* GetStandardDeviation() = 0 ;

  virtual void CreateHistograms() ;

  virtual void Resample() ;

  ResampledSampleType* GetResampledSample()
  { return m_Resampler->GetOutput() ; }

  virtual unsigned int GetResampledSampleSize() ;

  virtual void CalculateProjectionAxes() = 0 ;

  virtual void Project(int projectionAxisIndex) ;

  virtual void UpdateExpectedHistogram() ;

  double* GetTotalObservedScale()
  { return &m_TotalObservedScale ; } 

  virtual double GetCumulativeProbability(double x) 
    const = 0 ;

  virtual double GetProbabilityDensity(MeasurementVectorType &measurements) 
    const = 0 ;

  virtual double GetProportion() const
  { return m_Proportion ; }

  /** accessing methods for the projected sample */
  HistogramType* GetObservedHistogram() ;
  
  HistogramType* GetExpectedHistogram() ;

  virtual void PrintParameters(std::ostream &os) const = 0 ;

  virtual ParametersType GetFullParameters() const = 0 ;

protected:
  GoodnessOfFitComponentBase() ;
  virtual ~GoodnessOfFitComponentBase() ;
  virtual void PrintSelf(std::ostream& os, Indent indent) const ;

  /** default resampler type and realted types */
  typedef NeighborhoodSampler< TInputSample > ResamplerType ;

  /** default projection filter type*/
  typedef SampleToHistogramProjectionFilter< ResampledSampleType, float > 
  ProjectorType ;

  /** projection axis array type. The type of output from 
   * CalculateProjectionAxis(). The number of projection axis are fixed 
   * equal to the number of components of a measurement vector*/
  typedef FixedArray< double, MeasurementVectorSize > ProjectionAxisType ;
  typedef FixedArray< ProjectionAxisType, MeasurementVectorSize > 
  ProjectionAxisArrayType;

  ProjectionAxisArrayType* GetProjectionAxes()
  { return &m_ProjectionAxes ; }

  virtual void CreateEquiRangeBins() ;
  virtual void CreateEquiProbableBins() ;

private:
  TInputSample* m_InputSample ;
  ParametersType m_Parameters ;

  /** helper classes */
  ResamplerType::Pointer m_Resampler ;
  ProjectorType::Pointer m_Projector ;

  ProjectionAxisArrayType m_ProjectionAxes ;

  /** Histogram parameters */
  unsigned int m_HistogramNumberOfBins ;
  bool m_HistogramUseEquiProbableBins ;
  double m_HistogramExtent ;
  double m_HistogramBinOverlap ;
  bool m_HistogramSizeChanged ;

  /** Histogram statistics */
  double m_TotalObservedScale ;
  double m_HistogramMean ;
  double m_HistogramStandardDeviation  ;

  double m_Proportion ;

  /** resampled sample projected to a histogram */
  HistogramType::Pointer m_ObservedHistogram ;
  HistogramType::Pointer m_ExpectedHistogram ;
  bool m_UseExpectedHistogram ;

} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGoodnessOfFitComponentBase.txx"
#endif

#endif

