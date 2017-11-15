
#ifndef _itkCartesianToPolarTransform_txx
#define _itkCartesianToPolarTransform_txx

#include "itkCartesianToPolarTransform.h"
#include "vnl/vnl_math.h"

namespace itk
{

// Constructor with default arguments
template <class TScalarType, unsigned int NDimensions>
CartesianToPolarTransform<TScalarType, NDimensions>::CartesianToPolarTransform()
  : Superclass(NDimensions, 0)
{
  return;
}


// Destructor
template <class TScalarType, unsigned int NDimensions>
CartesianToPolarTransform<TScalarType, NDimensions>::~CartesianToPolarTransform()
{
  return;
}


// Print self
template <class TScalarType, unsigned int NDimensions>
void
CartesianToPolarTransform<TScalarType, NDimensions>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


// Transform a point
template <class TScalarType, unsigned int NDimensions>
typename CartesianToPolarTransform<TScalarType, NDimensions>::OutputPointType
CartesianToPolarTransform<TScalarType, NDimensions>::TransformPoint(const InputPointType & point) const
{

  if (NDimensions < 2)
  {
    itkExceptionMacro(<< "Method not applicable for dimension lower than 2.");
    return OutputPointType();
  }

  OutputPointType opoint(point);

  opoint[1] = sqrt(point[0] * point[0] + point[1] * point[1]);
  opoint[0] = acos(point[0] / opoint[1]);
  if (point[1] < 0)
    opoint[0] = 2 * vnl_math::pi - opoint[0];

  return opoint;
}

} // namespace itk

#endif
