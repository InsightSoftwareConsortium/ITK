#ifndef __itkFiniteDifferenceSparseImageFunction_txx_
#define __itkFiniteDifferenceSparseImageFunction_txx_

#include "itkFiniteDifferenceSparseImageFunction.h"

namespace itk {

template <class TSparseImageType>
void
FiniteDifferenceSparseImageFunction <TSparseImageType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
