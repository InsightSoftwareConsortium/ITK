
#ifndef __itkStatGaussianDensityFunction_h
#define __itkStatGaussianDensityFunction_h

//#include "itkStatLib.h"
#include "itkStatDensityEstimate.h"
//#include "itkStatDensityEstimate.txx"
#include "itkStatDensityFunction.h"
//#include "itkStatDensityFunction.txx"
#include <vnl/vnl_vector.h>
#include <vector>

namespace itk{

/** \class GaussianDensityFunction
 * \brief
 *
 * The GaussianDensityFunction takes in a histogram and
 * creates a mean and covariance based on it. New histogram 
 * can then be tested on normal distribution.
 */

template <class THistogram>
class GaussianDensityFunction : public DensityFunction<THistogram>
{
public:
 /**
  * Standard "Self" typedef
  */
  typedef GaussianDensityFunction Self;

 /**
  * Smart Pointer Support
  */
  typedef SmartPointer<Self> Pointer;
              
 /**
  * Interface into object factory
  */
  itkNewMacro(Self);

 /**
  * Run-time type information
  */
  itkTypeMacro(GaussianDensityFunction,DensityFunction);

 /**
  * Method to get mean
  */
  vector<double> GetMeans() { return m_Means; };

 /**
  * Method to get standard deviation
  */
  vector<double> GetStandardDeviations() { return m_StandardDeviations; };
 
 /**
  * call to train the gaussian. Histogram should be loaded prior
  * Train the density function as a gaussian. The density function will
  * iterate through every instance in every sample to find the means and
  * standard deviations
  */
  void Train(void);

 /**
  * call to test a sample with a gaussian.
  * there should be another call that uses a filename
  */
  typename DensityEstimate<THistogram>::Pointer Test(typename THistogram::Pointer h);

protected:		
  GaussianDensityFunction(void){};
  ~GaussianDensityFunction(void){};

  vector<double> m_Means;   //vector containing means
  vector<double> m_StandardDeviations; //vector containing stddev's
  vnl_matrix<double> m_Covariance;

//  double *m_FeatureSums;  //this is really a temporary variable that is
                          //handy to keep around to speed up computations,
                          //as the values can be re-used
   
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatGaussianDensityFunction.txx"
#endif

#endif
