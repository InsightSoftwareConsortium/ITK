
#ifndef __itkStatGaussianDensityFunction_txx
#define __itkStatGaussianDensityFunction_txx

//#include "itkStatLib.h"
#include "vnl/vnl_transpose.h"
#include "vnl/vnl_matrix.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "vnl/algo/vnl_determinant.h"
#include "itkIndex.h"
#include "itkStatHistogramMean.h"
#include "itkStatHistogramCovariance.h"
#include "itkStatHistogramStandardDeviation.h"
#include "itkStatGaussianDensityFunction.h"

const float PI = 3.14159;
namespace itk{

template <class THistogram>
void
GaussianDensityFunction<THistogram>
::Train(void)
{
  int dimension = THistogram::GetHistogramDimension();

  // Allocate memory
  m_Means.resize(dimension);
  m_StandardDeviations.resize(dimension);
  m_Covariance.resize(dimension, dimension);


  typedef HistogramMean<double, THistogram> MeanType;
  MeanType::Pointer mean = MeanType::New();

  typedef HistogramStandardDeviation<double, THistogram> StdType;
  StdType::Pointer std = StdType::New();

  typedef HistogramCovariance<double, THistogram> CovType;
  CovType::Pointer cov = CovType::New();


  // Calculate mean
  mean->SetHistogram(m_Histogram);
  mean->Execute();
  m_Means = mean->GetMean();

  // Calculate standard deviation
  std->SetHistogram(m_Histogram);
  std->Execute();
  m_StandardDeviations = std->GetStandardDeviation(); 

  // Calculate covariance matrix
  cov->SetHistogram(m_Histogram);
  cov->Execute();
  m_Covariance = cov->GetCovariance(); 

}

/*
 *  Test uses the mean and standard deviation to find the
 *  probability that an instance is of the type that has been 
 *  trained. It returns a DensityEstimate which includes a 
 *  probability for each tested instance.
 **NOTE: This has not been tested extensively. It is
 * likely that this function is not giving correct result. I ran
 * out of time and had to work on infrastructure. sorry :(
 *
 *\param Sample<T>::Pointer sample The sample to be tested
 *\return DensityEstimate<T>::Pointer A vector of probabilities
*/

template <class THistogram>
typename DensityEstimate<THistogram>::Pointer
GaussianDensityFunction<THistogram>
::Test(typename THistogram::Pointer h)
{
  typedef DensityEstimate<THistogram> DensityEstimateType;
  DensityEstimateType::Pointer normalEstimate = DensityEstimateType::New();

  // allocate container
  typename THistogram::SizeType size;
  size = h->GetSize();
  normalEstimate->SetSize(size);
  normalEstimate->Allocate();
  int dim = THistogram::GetHistogramDimension();

  // inverse matrix of the covariance matrix
  vnl_matrix<double> inverseMatrix(dim, dim);
  inverseMatrix = vnl_matrix_inverse<double>(m_Covariance);

  // calculate the determinant of the covaraince matrix
  double determinant = vnl_determinant(m_Covariance);

  // calculate coefficient C of multivariate gaussian
  // p(x) = C exp(-0.5 * (x-u) * inv(covariance) * (x-u)')
  double scale = 1.0/pow( pow(2.0*PI, dim), 1/2.0)*sqrt(fabs(determinant));

  double prob;
  vnl_matrix<double> xMatrix(dim,1);

  vnl_matrix<double> exponentMatrix(1,1);
  typename THistogram::IndexType index;
 
  int i;
  typename THistogram::Iterator it(h);
  it.Begin();
  while ( !it.IsAtEnd() )
    {
    for ( i=0; i < dim; i++)
      {
      xMatrix.put(i,0, it.GetFeature(i) - m_Means[i]);
      }
    exponentMatrix = vnl_transpose(xMatrix) * inverseMatrix * xMatrix;
    prob = scale * exp( -0.5*exponentMatrix.get(0,0) );
    index = it.GetIndex();
    normalEstimate->SetProbability(index, prob);
    ++it;
    }

  it = it.Begin();
  cout << it.GetIndex() << endl;
  while ( !it.IsAtEnd() )
    {
    cout << " second loop " << endl;
    index = it.GetIndex();
    cout << normalEstimate->GetProbability(index) << endl;
    ++it;
    }
  cout << "Test() done" << endl;
  return normalEstimate;
}


} // end namespace itk

#endif
