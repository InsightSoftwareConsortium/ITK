#ifndef __itkBloxBoundaryProfile_txx
#define __itkBloxBoundaryProfile_txx

#include "itkBloxBoundaryProfileItem.h"

namespace itk
{
template <unsigned int VImageDimension>
BloxBoundaryProfileItem<VImageDimension>
::BloxBoundaryProfileItem()
{
  m_LowerIntensity = 0;
  m_UpperIntensity = 0;
  m_Mean = 0;
  m_ProfileLength = 0;
  m_MeanNormalized = 0;
  m_StandardDeviation = 0;
  m_StandardDeviationNormalized = 0;
  m_BoundaryPoint = 0;
  m_Gradient = 0;
}

template <unsigned int VImageDimension>
BloxBoundaryProfileItem<VImageDimension>
::~BloxBoundaryProfileItem()
{

}

template <unsigned int VImageDimension>
void
BloxBoundaryProfileItem<VImageDimension>
::SetBoundaryPoint(BPItemType * point) 
{
  m_BoundaryPoint = point;
}

template <unsigned int VImageDimension>
void
BloxBoundaryProfileItem<VImageDimension>
::SetLowerIntensity(double lowerIntensity)
{
  m_LowerIntensity = lowerIntensity;
}

template <unsigned int VImageDimension>
double
BloxBoundaryProfileItem<VImageDimension>
::GetLowerIntensity(void)
{
  return m_LowerIntensity;
}

template <unsigned int VImageDimension>
void
BloxBoundaryProfileItem<VImageDimension>
::SetUpperIntensity(double upperIntensity)
{
  m_UpperIntensity = upperIntensity;
}

template <unsigned int VImageDimension>
double
BloxBoundaryProfileItem<VImageDimension>
::GetUpperIntensity(void)
{
  return(m_UpperIntensity);
}

template <unsigned int VImageDimension>
void
BloxBoundaryProfileItem<VImageDimension>
::SetMean(double mean)
{
  m_Mean = mean;
}

template <unsigned int VImageDimension>
double
BloxBoundaryProfileItem<VImageDimension>
::GetMean(void)
{
  return(m_Mean);
}

template <unsigned int VImageDimension>
void 
BloxBoundaryProfileItem<VImageDimension>
::SetProfileLength(unsigned int profileLength)
{
  m_ProfileLength = profileLength;
}

template <unsigned int VImageDimension>
unsigned int
BloxBoundaryProfileItem<VImageDimension>
::GetProfileLength(void)
{
  return(m_ProfileLength);
}

template <unsigned int VImageDimension>
void
BloxBoundaryProfileItem<VImageDimension>
::SetMeanNormalized(void)
{
  m_MeanNormalized = m_Mean - m_ProfileLength/2;
}

template <unsigned int VImageDimension>
double
BloxBoundaryProfileItem<VImageDimension>
::GetMeanNormalized(void)
{
  return(m_MeanNormalized);
}

template <unsigned int VImageDimension>
void
BloxBoundaryProfileItem<VImageDimension>
::SetStandardDeviation(double standardDeviation)
{
  m_StandardDeviation = standardDeviation;
}

template <unsigned int VImageDimension>
double
BloxBoundaryProfileItem<VImageDimension>
::GetStandardDeviation(void)
{
  return(m_StandardDeviation);
}

template <unsigned int VImageDimension>
void
BloxBoundaryProfileItem<VImageDimension>
::SetStandardDeviationNormalized(void)
{
  m_StandardDeviationNormalized = m_StandardDeviation / m_ProfileLength;
}

template <unsigned int VImageDimension>
double
BloxBoundaryProfileItem<VImageDimension>
::GetStandardDeviationNormalized(void)
{
  return(m_StandardDeviationNormalized);
}

template <unsigned int VImageDimension>
void
BloxBoundaryProfileItem<VImageDimension>
::SetOptimalBoundaryLocation(VectorType spatialFunctionOriginVector, VectorType orientation)
{
  VectorType optimalBoundaryLocation;
  optimalBoundaryLocation = m_MeanNormalized * orientation;
  optimalBoundaryLocation = spatialFunctionOriginVector + optimalBoundaryLocation;
  for(unsigned int i = 0; i < VImageDimension; i++)
    m_OptimalBoundaryLocation[i] = optimalBoundaryLocation[i];
}

template <unsigned int VImageDimension>
Point<double, VImageDimension>
BloxBoundaryProfileItem<VImageDimension>
::GetOptimalBoundaryLocation(void)
{
  return(m_OptimalBoundaryLocation);
}

template <unsigned int VImageDimension>
void
BloxBoundaryProfileItem<VImageDimension>
::SetGradient(GradientType * gradient)
{
  m_Gradient = gradient;
}

template <unsigned int VImageDimension>
CovariantVector<double, VImageDimension>
BloxBoundaryProfileItem<VImageDimension>
::GetGradient()
{
  return m_Gradient;
}

} // end namespace itk

#endif

