#ifndef __itkStatDensityEstimate_h
#define __itkStatDensityEstimate_h


//#include "itkStatLib.h"
#include "itkObject.h"
#include "itkSize.h"

namespace itk{

/** \class DensityEstimate
 * \brief 
 *
 * DensityEstimate contains the result of a test 
 * on a hiatogram. 
 */

template <class THistogram>
class DensityEstimate : public Object
{
public:
 /**
  * Standard "Self" typedef
  */
  typedef DensityEstimate<THistogram> Self;

 /**
  * Smart Pointer Support
  */
  typedef SmartPointer<Self> Pointer;

 /**
  * Size typedef support
  */ 
  typedef typename THistogram::SizeType SizeType;

  typedef typename THistogram::IndexType IndexType;
              
 /**
  * Interface into object factory
  */
  itkNewMacro(Self);

 /**
  * Run-time type information
  */
  itkTypeMacro(DensityEstimate,Object);
    
 /**
  * set the number of instances in the estimate
  */
//  void SetNumInstances(const int numInstances);

 /**
  * get the number of instances in the estimate
  */
//  int GetNumInstances(void);

 /**
  * set the label that the estimate is based on
  */
//  void SetLabel(const char *newLabel);

 /** 
  * get the name of the label that the estimate is based on
  */
//  const char *GetLabel(void);

 /**
  * Set the estimate for an individual instance
  */
  void SetProbability(const IndexType index, const float probability);

 /**
  * return the probability of an individual instance
  */
		double GetProbability(const IndexType index);

 /**
  * Set m_Size
  */
  void SetSize(SizeType size) { m_Size = size;};
  
 /**
  * Allocate m_Probability which is the container of density estimate
  * m_Size should be set prior
  */
  void Allocate()
  { m_Probabilities = THistogram::New();
    m_Probabilities->SetSize(m_Size);
    m_Probabilities->AllocateHistogram();
  }

 /**
  * Print information about the estimate
  */
//  void PrintDensity(std::ostream &);

protected:
		DensityEstimate() {};
		~DensityEstimate(){};

//		int m_NumInstances; //number of instances
//  char *m_Label;      //the label for this estimate
    //perhaps this should be a different data structure?

  // The number of bins for each image
  SizeType m_Size;

  typename THistogram::Pointer m_Probabilities; //the histogram of probability values
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatDensityEstimate.txx"
#endif

#endif
