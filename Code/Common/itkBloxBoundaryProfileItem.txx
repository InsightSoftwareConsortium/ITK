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
  m_BoundaryWidth = 0;
  m_ProfileLength = 0;
  m_MeanNormalized = 0;
  m_StandardDeviation = 0;
  m_StandardDeviationNormalized = 0;
}

template <unsigned int VImageDimension>
BloxBoundaryProfileItem<VImageDimension>
::~BloxBoundaryProfileItem()
{

}

} // end namespace itk

#endif
