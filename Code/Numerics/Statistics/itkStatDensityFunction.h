
#ifndef __itkStatDensityFunction_h
#define __itkStatDensityFunction_h

//#include "itkStatLib.h"
//#include "itkStatSample.h"
//#include "itkStatSample.txx"
//#include "itkStatDensityEstimate.h"
//#include "itkStatDensityEstimate.txx"
#include <vector>

#include "itkStatHistogram.h"
#include "itkStatDensityEstimate.h"
#include "itkStatDenseHistogram.h"
#include "itkStatSparseHistogram.h"
#include "itkObject.h"

namespace itk{

/** \class DensityFunction
 * \brief
 *
 * DensityFunction provides a base class to model
 * statistical functions. It provides a common Test
 * and Train interface
 */

template <class THistogram>
class DensityFunction : public Object
{
public:
 /**
  * Standard "Self" typedef
  */
  typedef DensityFunction Self;

 /**
  * Smart Pointer Support
  */
  typedef SmartPointer<Self> Pointer;

  typedef typename THistogram::BinType BinType;
  typedef typename THistogram::FeatureType FeatureType;

  
 /**
  *
  */
  typedef DenseHistogram<BinType, THistogram::Dimension, FeatureType> DenseType;
               
 /**
  * Interface into object factory
  */
  itkNewMacro(Self);

 /**
  * Run-time type information 
  */
  itkTypeMacro(DensityFunction,Object);

 /**
  * add a sample to the density function
  */
   void SetHistogram(typename THistogram::Pointer h)
  {  m_Histogram = h; } 

 /**
  * purely virtual Train/Test functions
  */
		virtual void Train() = 0;
  virtual typename DensityEstimate<THistogram>::Pointer Test(typename THistogram::Pointer h) = 0;
 
 virtual vector<FeatureType> GenerateInstances(int num){};
protected:		
  DensityFunction() {};
  ~DensityFunction(){};

  typename THistogram::Pointer m_Histogram;  //histogram
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatDensityFunction.txx"
#endif

#endif
