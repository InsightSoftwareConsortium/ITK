
#ifndef __itkStatDensityEstimate_txx
#define __itkStatDensityEstimate_txx

#include "itkStatDensityEstimate.h"
#include <iostream.h>

namespace itk{

/*
template <class T>
DensityEstimate<T>::
DensityEstimate()
{
  m_Label = NULL;  
  m_Probabilities = NULL;
  m_NumInstances = 0;
}

template <class T>
DensityEstimate<T>::~DensityEstimate(){
}
*/

/** SetNumInstances
  * Set the number of instances in the estimate
  *
  *\param const int numInstances
  *
template <class T>
void 
DensityEstimate<T>::
SetNumInstances(const int numInstances)
{
  if(m_Probabilities !=NULL)
    {
    delete m_Probabilities;
    }
  m_NumInstances = numInstances;
  m_Probabilities = new double[m_NumInstances];
}

/** GetNumInstances
  * return the number of instances in the estimate
  *
  *\return int Number of instances
  *
template <class T>
int 
DensityEstimate<T>::
GetNumInstances(void)
{
  return m_NumInstances;
}

/** SetLabel
  * Set the label of the estimate
  *
  * \param const char* newLabel the new label for the estimate
  *
template <class T>
void 
DensityEstimate<T>::
SetLabel(const char *newLabel)
{
  if(newLabel == NULL)
    {
    return;
    }
  m_Label = new char[strlen(newLabel)];
  strcpy(m_Label,newLabel);
}

/** GetLabel
  * return the label of the estimate
  *
  *\return char* name of estimate
  *
template <class T>
const char * 
DensityEstimate<T>::
GetLabel(void)
{
  return m_Label;
}

 /** SetProbability
  * set the probability of one of the instances
  *
  * \param const IndexType index Which instance to set
  * \param const double Probability The value to set it to
  */
template <class THistogram>
void 
DensityEstimate<THistogram>
::SetProbability(const IndexType index, const float probability)
{
  m_Probabilities->SetFrequency(index, probability);
}

 /** GetProbability
  * return the probability of one instance
  *
  *\param const int instanceNum which instance is of interest
  *\return double the value of that instance
  */
template <class THistogram>
double 
DensityEstimate<THistogram>
::GetProbability(const IndexType index)
{
  return m_Probabilities->GetFrequency(index);
}

/** PrintDensity
  * Print debug info about the estimate
  *
template <class T>
void 
DensityEstimate<T>::
PrintDensity(std::ostream &out)
{
  if(m_Probabilities == NULL || m_NumInstances==0)
    {
    out << "No instances" << endl;
    return;
    }
  out << "Probability of instance: ";
  if(m_Label != NULL)
    {
    out << m_Label;
    }
  out << endl;
  for(int instancect =0;instancect<m_NumInstances;instancect++)
    {
    out << instancect << " = " << m_Probabilities[instancect] << endl;
    }
}
*/

} //end namespace itk
	
#endif
