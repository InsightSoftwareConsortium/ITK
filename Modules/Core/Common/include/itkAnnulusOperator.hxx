/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkAnnulusOperator_hxx
#define __itkAnnulusOperator_hxx

#include "itkAnnulusOperator.h"
#include "itkMath.h"
#include "itkSphereSpatialFunction.h"

namespace itk
{
/** Create the operator */
template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::CreateOperator()
{
  CoefficientVector coefficients;

  coefficients = this->GenerateCoefficients();

  this->Fill(coefficients);
}

/** Set/Get the inner radius of the annulus. Radius is specified in
* physical units (mm). */
template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::SetInnerRadius(double r)
{
  m_InnerRadius = r;
}

template< class TPixel, unsigned int TDimension, class TAllocator >
double
AnnulusOperator< TPixel, TDimension, TAllocator >
::GetInnerRadius() const
{
  return m_InnerRadius;
}

/** Set/Get the thickness of the annulus.  The outer radius of the
 * annulus is defined as r = InnerRadius + Thickness. Thickness is
 * specified in physical units (mm). */
template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::SetThickness(double t)
{
  m_Thickness = t;
}

template< class TPixel, unsigned int TDimension, class TAllocator >
double
AnnulusOperator< TPixel, TDimension, TAllocator >
::GetThickness() const
{
  return m_Thickness;
}

/** Set/Get the pixel spacings.  Setting these ensures the annulus
 * is round in physical space. Defaults to 1. */
template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::SetSpacing(SpacingType & s)
{
  m_Spacing = s;
}

/** Set/Get the pixel spacings.  Setting these ensures the annulus
 * is round in physical space. Defaults to 1. */
template< class TPixel, unsigned int TDimension, class TAllocator >
const typename AnnulusOperator< TPixel, TDimension, TAllocator >::SpacingType &
AnnulusOperator< TPixel, TDimension, TAllocator >
::GetSpacing() const
{
  return m_Spacing;
}

/** Set/Get whether kernel values are computed automatically or
 * specified manually */
template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::SetNormalize(bool b)
{
  m_Normalize = b;
}

template< class TPixel, unsigned int TDimension, class TAllocator >
bool
AnnulusOperator< TPixel, TDimension, TAllocator >
::GetNormalize() const
{
  return m_Normalize;
}

template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::NormalizeOn()
{
  this->SetNormalize(true);
}

template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::NormalizeOff()
{
  this->SetNormalize(false);
}

/** If Normalize is on, you define the annulus to have a bright
 * center or a dark center. */
template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::SetBrightCenter(bool b)
{
  m_BrightCenter = b;
}

template< class TPixel, unsigned int TDimension, class TAllocator >
bool
AnnulusOperator< TPixel, TDimension, TAllocator >
::GetBrightCenter() const
{
  return m_BrightCenter;
}

template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::BrightCenterOn()
{
  this->SetBrightCenter(true);
}

template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::BrightCenterOff()
{
  this->SetBrightCenter(false);
}

/** If Normalize is off, the interior to annulus, the
 * annulus (region between the two circles), and the region exterior to the
 * annulus to be defined manually.  Defauls are 0, 1, 0
 * respectively. */
template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::SetInteriorValue(TPixel v)
{
  m_InteriorValue = v;
}

template< class TPixel, unsigned int TDimension, class TAllocator >
typename AnnulusOperator< TPixel, TDimension, TAllocator >::PixelType
AnnulusOperator< TPixel, TDimension, TAllocator >
::GetInteriorValue() const
{
  return m_InteriorValue;
}

template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::SetAnnulusValue(TPixel v)
{
  m_AnnulusValue = v;
}

template< class TPixel, unsigned int TDimension, class TAllocator >
typename AnnulusOperator< TPixel, TDimension, TAllocator >
::PixelType
AnnulusOperator< TPixel, TDimension, TAllocator >
::GetAnnulusValue() const
{
  return m_AnnulusValue;
}

template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::SetExteriorValue(TPixel v)
{
  m_ExteriorValue = v;
}

template< class TPixel, unsigned int TDimension, class TAllocator >
typename AnnulusOperator< TPixel, TDimension, TAllocator >
::PixelType
AnnulusOperator< TPixel, TDimension, TAllocator >
::GetExteriorValue() const
{
  return m_ExteriorValue;
}

/** Assignment operator */
template< class TPixel, unsigned int TDimension, class TAllocator >
AnnulusOperator< TPixel, TDimension, TAllocator > &
AnnulusOperator< TPixel, TDimension, TAllocator >
::operator=(const Self & other)
{
  Superclass::operator=(other);
  m_InnerRadius = other.m_InnerRadius;
  m_Thickness = other.m_Thickness;
  m_Spacing = other.m_Spacing;
  m_InteriorValue = other.m_InteriorValue;
  m_AnnulusValue = other.m_AnnulusValue;
  m_ExteriorValue = other.m_ExteriorValue;
  m_Normalize = other.m_Normalize;
  m_BrightCenter = other.m_BrightCenter;
  return *this;
}

/** Prints some debugging information */
template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::PrintSelf(std::ostream & os, Indent i) const
{
  os << i << "AnnulusOperator { this=" << this
     << ", m_InnerRadius = " << m_InnerRadius
     << ", m_Thickness = " << m_Thickness
     << ", m_Spacing = " << m_Spacing
     << ", m_Normalize = " << m_Normalize
     << ", m_BrightCenter = " << m_BrightCenter
     << ", m_InteriorValue = " << m_InteriorValue
     << ", m_ExteriorValue = " << m_ExteriorValue
     << "}" << std::endl;
  Superclass::PrintSelf( os, i.GetNextIndent() );
}

/** This function fills the coefficients into the corresponding
 *  neighborhood. */
template< class TPixel, unsigned int TDimension, class TAllocator >
void
AnnulusOperator< TPixel, TDimension, TAllocator >
::Fill(const CoefficientVector & coeff)
{
  std::slice *temp_slice = new std::slice(0, coeff.size(), 1);

  typename Self::SliceIteratorType data(this, *temp_slice);
  delete temp_slice;

  typename Superclass::CoefficientVector::const_iterator it = coeff.begin();

  // Copy the coefficients into the neighborhood
  for ( data = data.Begin(); data < data.End(); ++data, ++it )
    {
    *data = *it;
    }
}

template< class TPixel, unsigned int TDimension, class TAllocator >
typename AnnulusOperator< TPixel, TDimension, TAllocator >
::CoefficientVector
AnnulusOperator< TPixel, TDimension, TAllocator >
::GenerateCoefficients()
{
  // Determine the initial kernel values...
  double interiorV, annulusV, exteriorV;

  if ( m_Normalize )
    {
    double bright = ( m_BrightCenter ? 1.0 : -1.0 );

    // Initial values for a normalized kernel
    interiorV = bright;
    annulusV = -1.0 * bright;
    exteriorV = 0.0;
    }
  else
    {
    // values for a specified kernel
    interiorV = m_InteriorValue;
    annulusV = m_AnnulusValue;
    exteriorV = m_ExteriorValue;
    }

  // Compute the size of the kernel in pixels
  SizeType     r;
  unsigned int i, j;
  double       outerRadius = m_InnerRadius + m_Thickness;
  for ( i = 0; i < TDimension; ++i )
    {
    r[i] = Math::Ceil< SizeValueType >(outerRadius / m_Spacing[i]);
    }
  this->SetRadius(r);

  // Use a couple of sphere spatial functions...
  typedef SphereSpatialFunction< TDimension > SphereType;
  typename SphereType::Pointer innerS = SphereType::New();
  typename SphereType::Pointer outerS = SphereType::New();

  innerS->SetRadius(m_InnerRadius);
  outerS->SetRadius(m_InnerRadius + m_Thickness);

  // Walk the neighborhood (this) and evaluate the sphere spatial
  // functions
  bool         inInner, inOuter;
  double       sumNotExterior = 0.0;
  double       sumNotExteriorSq = 0.0;
  unsigned int countNotExterior = 0;

  const typename SizeType::SizeValueType w = this->Size();

  std::vector< bool > outside(w);
  CoefficientVector   coeffP(w);
  OffsetType          offset;
  typename SphereType::InputType point;

  for ( i = 0; i < w; ++i )
    {
    // get the offset from the center pixel
    offset = this->GetOffset(i);

    // convert to a position
    for ( j = 0; j < TDimension; ++j )
      {
      point[j] = m_Spacing[j] * offset[j];
      }

    // evaluate the spheres
    inInner = innerS->Evaluate(point);
    inOuter = outerS->Evaluate(point);

    // set the coefficients
    if ( !inOuter )
      {
      // outside annulus
      coeffP[i] = exteriorV;
      outside[i] = true;
      }
    else if ( !inInner )
      {
      // inside the outer circle but outside the inner circle
      coeffP[i] = annulusV;
      sumNotExterior += annulusV;
      sumNotExteriorSq += ( annulusV * annulusV );
      countNotExterior++;
      outside[i] = false;
      }
    else
      {
      // inside inner circle
      coeffP[i] = interiorV;
      sumNotExterior += interiorV;
      sumNotExteriorSq += ( interiorV * interiorV );
      countNotExterior++;
      outside[i] = false;
      }
    }

  // Normalize the kernel if necessary
  if ( m_Normalize )
    {
    // Calculate the mean and standard deviation of kernel values NOT
    // the exterior
    double num = static_cast< double >( countNotExterior );
    double mean = sumNotExterior / num;
    double var = ( sumNotExteriorSq - ( sumNotExterior * sumNotExterior / num ) )
      / ( num - 1.0 );
    double std = vcl_sqrt(var);

    // convert std to a scaling factor k such that
    //
    //        || (coeffP - mean) / k || = 1.0
    //
    double k = std * vcl_sqrt(num - 1.0);

    // Run through the kernel again, shifting and normalizing the
    // elements that are not exterior to the annulus.  This forces the
    // kernel to have mean zero and norm 1 AND forces the region
    // outside the annulus to have no influence.
    for ( i = 0; i < w; ++i )
      {
      // normalize the coefficient if it is inside the outer circle
      // (exterior to outer circle is already zero)
      if ( !outside[i] )
        {
        coeffP[i] = ( coeffP[i] - mean ) / k;
        }
      }
    }

  return coeffP;
}

} // namespace itk

#endif
