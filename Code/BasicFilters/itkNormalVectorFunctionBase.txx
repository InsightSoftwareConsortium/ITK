#ifndef __itkNormalVectorFunctionBase_txx_
#define __itkNormalVectorFunctionBase_txx_

#include "itkNormalVectorFunctionBase.h"
#include "itkNumericTraits.h"

namespace itk {

template <class TSparseImageType>
NormalVectorFunctionBase <TSparseImageType>
::NormalVectorFunctionBase ()
{
  this->SetTimeStep (NumericTraits<TimeStepType>::One);
}

template <class TSparseImageType>
void
NormalVectorFunctionBase <TSparseImageType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "TimeStep: " << m_TimeStep << std::endl;
}

} // end namespace itk

#endif
