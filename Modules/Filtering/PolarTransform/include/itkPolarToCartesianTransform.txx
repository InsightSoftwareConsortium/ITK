
#ifndef _itkPolarToCartesianTransform_txx
#define _itkPolarToCartesianTransform_txx

#include "itkPolarToCartesianTransform.h"


namespace itk
{

// Constructor with default arguments
template <class TScalarType, unsigned int NDimensions>
PolarToCartesianTransform<TScalarType, NDimensions>::PolarToCartesianTransform()
  : Superclass(NDimensions, 0)
{
  return;
}


// Destructor
template <class TScalarType, unsigned int NDimensions>
PolarToCartesianTransform<TScalarType, NDimensions>::~PolarToCartesianTransform()
{
  return;
}


// Print self
template <class TScalarType, unsigned int NDimensions>
void
PolarToCartesianTransform<TScalarType, NDimensions>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


// Transform a point
template <class TScalarType, unsigned int NDimensions>
typename PolarToCartesianTransform<TScalarType, NDimensions>::OutputPointType
PolarToCartesianTransform<TScalarType, NDimensions>::TransformPoint(const InputPointType & point) const
{

  if (NDimensions < 2)
  {
    itkExceptionMacro(<< "Method not applicable for dimension lower than 2.");
    return OutputPointType();
  }

  OutputPointType opoint(point);
  opoint[0] = point[1] * cos(point[0]); // r*cos(alpha)
  opoint[1] = point[1] * sin(point[0]); // r*sin(alpha)
  return opoint;
}

} // namespace itk

#endif
