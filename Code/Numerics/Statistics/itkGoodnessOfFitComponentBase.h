/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGoodnessOfFitComponentBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
#include "vnl/vnl_matrix.h"
#include "itkVariableSizeMatrix.h"

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
 * <b>Recent API changes</b>
 * The typedef for \c CenterType and \c MeanType has changed to itk::Array
 * from FixedArray and Vector respectively to allow the measurement vector
 * length to be set at run time. The StaticConst macro \c MeasurementVectorSize
 * has been removed. It is now obtained from the sample. The typedef for
 * ProjectionAxisArrayType has changed from FixedArray to VariableSizeMatrix.
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
  
  /** TInputSample type alias */
  typedef TInputSample InputSampleType ;

  
  /** Typedefs from the TInputSample */
  typedef typename TInputSample::MeasurementType MeasurementType ;
  typedef typename TInputSample::MeasurementVectorType MeasurementVectorType ;
  typedef typename TInputSample::MeasurementVectorSizeType MeasurementVectorSizeType;

  /** Resample() output type */
  typedef Subsample< TInputSample > ResampledSampleType ;

  /** Histogram type that will be used for observed and expected histogram*/
  typedef Histogram< float, 1 > HistogramType ;
  typedef typename HistogramType::Pointer         HistogramPointer ;
  typedef typename HistogramType::ConstPointer    HistogramConstPointer ;

  /** Type of the array of component parameters */
  typedef Array< double >  ParametersType ;

  /** Type of the center position for the hyperspherical neighborhood
   *  sampling */
  typedef Array< double >  CenterType;

  /** Type of the radius of the hyperspherical neighborhood sampling */
  typedef double RadiusType ;

  /** Type of the mean of the distribution */
  typedef Array< double >   MeanType ;


  /** Type of standard deviation of the distribution */
  typedef double StandardDeviationType ;

  /** Set/Gets the input sample */
  virtual void SetInputSample(const TInputSample* sample) ;
  const TInputSample* GetInputSample() const;

  /** Gets the total number of parameters for this component */
  virtual unsigned int GetNumberOfParameters() const = 0 ;

  /** Set/Gets the component parameters */
  virtual void SetParameters(const ParametersType &parameters) ;
  ParametersType* GetParameters()
  { return m_Parameters ; }

  /** Sets the flag that indicates this component uses the histogram
   * generated with expected distribution from the parameters.*/
  void SetUseExpectedHistogram(bool flag) ;

  /** Set/Gets the nubmer of bins of histograms (expected and observed) */
  void SetHistogramNumberOfBins(int numberOfBins) ;
  int GetHistogramNumberOfBins()
  { return m_HistogramNumberOfBins ; }

  /** Set/Gets the flag that indicates the probability of each bins in the
   * histograms should be equal. This can be achieved by varying the
   * interval of bins. */
  void SetHistogramUseEquiProbableBins(bool flag) ;
  bool GetHistogramUseEquiProbableBins()
  { return m_HistogramUseEquiProbableBins ; }

  /** Set/Get the overlapping effects extent. */
  void SetHistogramBinOverlap(double overlap) ;
  double GetHistogramBinOverlap()
  { return m_HistogramBinOverlap ; }

  /** Set/Gets the extent of histogram from the mean in terms of
   * standard deivation */
  void SetHistogramExtent(double extent) ;
  double GetHistogramExtent()
  { return m_HistogramExtent ; }

  /** Gets the center position for the neighborhood sampling */
  virtual CenterType* GetCenter() = 0 ;
  
  /** Gets the radius for the neighborhood sampling */
  virtual RadiusType* GetRadius() = 0 ;

  /** Gets the mean of the distribution */
  virtual MeanType* GetMean() = 0 ;

  /** Gets the standard deviation of the distribution */
  virtual RadiusType* GetStandardDeviation() = 0 ;

  /** Generates the histogram (expected and observed) */
  virtual void CreateHistograms() ;

  /** Samples measurement vectors using the center and radius */
  virtual void Resample() ;

  /** Gets the sampled data set */
  ResampledSampleType* GetResampledSample()
  { return m_Resampler->GetOutput() ; }

  /** Gest the size of the sampled data set */
  virtual unsigned int GetResampledSampleSize() ;

  /** Calculates the longest axis based on eigen analysis */
  virtual void CalculateProjectionAxes() = 0 ;

  /** Projects measurement vectors onto the projection axis calculated
   * by the CalculateProjectionAxes method. */
  virtual void Project(int projectionAxisIndex) ;

  /** Fills up the expected histogram based on the distribution
   *  parameters */
  virtual void UpdateExpectedHistogram() ;

  /** Gets the total scale of the observed histogram */
  double* GetTotalObservedScale()
  { return &m_TotalObservedScale ; } 

  /** Gets the probability of x. univariate function */
  virtual double GetCumulativeProbability(double x) 
    const = 0 ;

  /** Gets the probability density of measurements. multivariate
   * function */
  virtual double GetProbabilityDensity(MeasurementVectorType &measurements) 
    const = 0 ;

  /** Gets the proportion of this component among multiple components. */
  virtual double GetProportion() const
  { return m_Proportion ; }

  /** Gets the observed historm */
  HistogramType *  GetObservedHistogram();
  
  /** Gets the expected historm */
  HistogramType *  GetExpectedHistogram();

  /** Prints component parameters. For debugging */
  virtual void PrintParameters(std::ostream &os) const = 0 ;

  /** Gest the parameters of this component */
  virtual ParametersType GetFullParameters() const = 0 ;

  /** Get Macro to get the length of a measurement vector. This is equal to 
   * the length of each measurement vector contained in the samples that are
   * plugged in as input to this class. GetMeasurementVectorSize() will return 
   * zero until the SetInputSample() method has been called */
  itkGetConstMacro( MeasurementVectorSize, MeasurementVectorSizeType );
  
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
   * equal to the number of components of a measurement vector. */ 
  typedef VariableSizeMatrix< double > ProjectionAxisArrayType;


  ProjectionAxisArrayType* GetProjectionAxes()
  { return &m_ProjectionAxes ; }

  /** Creates an empty histogram with bins having same interval */
  virtual void CreateEquiRangeBins() ;

  /** Creates an empty histogram with bins having same probability
   * based on the distribution parameters */
  virtual void CreateEquiProbableBins() ;

private:
  /** Length of each measurement vector */
  MeasurementVectorSizeType m_MeasurementVectorSize;

  const TInputSample* m_InputSample ;
  ParametersType m_Parameters ;

  /** helper classes */
  typename ResamplerType::Pointer m_Resampler ;
  typename ProjectorType::Pointer m_Projector ;

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
  HistogramPointer m_ObservedHistogram ;
  HistogramPointer m_ExpectedHistogram ;
  bool m_UseExpectedHistogram ;

} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGoodnessOfFitComponentBase.txx"
#endif

#endif

