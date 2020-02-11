/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkAnnulusOperator_h
#define itkAnnulusOperator_h

#include "itkNeighborhoodOperator.h"
#include "itkVector.h"

namespace itk
{
/**
 * \class AnnulusOperator
 *
 * \brief A NeighborhoodOperator for performing a matched filtering with an
 * annulus (two concentric circles, spheres, hyperspheres, etc.)
 *
 * AnnulusOperator defines a non-directional NeighborhoodOperator
 * representing two concentric circles, spheres, hyperspheres, etc.
 * The inner radius and the thickness of the annulus can be specified.
 *
 * The values for the annulus can be specified in a variety of
 * manners:
 *
 * 1) The values for the interior of the annulus (interior of inner
 * circle), the values for annulus (the region between the inner and
 * outer circle), and the values for the exterior of the annulus can
 * be specified.  This mode is useful in correlation based matched
 * filter applications. For instance, defining a hollow (or even
 * filled) circle.
 *
 * 2) The values can defined automatically for normalized
 * correlation. The values in the kernel will be defined to have mean
 * zero and norm 1.  The area outside the annulus will have values
 * of zero. In this mode, you can also specify whether you want the
 * center of the annulus to be bright (intensity > 0) or dark
 * (intensity < 0).
 *
 * 1) Set the annulus parameters: InnerRadius and Thickness
 * 2) Set the intensities to use for interior, wall, and exterior
 * kernel positions for correlation based operations or call
 * NormalizeOn() to define kernel values automatically for use in
 * normalized correlation.
 * 3) If NormalizedOn(), indicate whether you want the center of the
 * annulus to be bright or dark.
 * 4) call \c CreateOperator()
 *
 * \note AnnulusOperator does not have any user-declared "special member function",
 * following the C++ Rule of Zero: the compiler will generate them if necessary.
 *
 * \sa NeighborhoodOperator
 * \sa Neighborhood
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 */
template <typename TPixel, unsigned int TDimension = 2, typename TAllocator = NeighborhoodAllocator<TPixel>>
class ITK_TEMPLATE_EXPORT AnnulusOperator : public NeighborhoodOperator<TPixel, TDimension, TAllocator>
{
public:
  /** Standard type alias */
  using Self = AnnulusOperator;
  using Superclass = NeighborhoodOperator<TPixel, TDimension, TAllocator>;

  /** Additional type alias */
  using SizeType = typename Superclass::SizeType;
  using OffsetType = typename Superclass::OffsetType;
  using SpacingType = Vector<double, TDimension>;

  itkTypeMacro(AnnulusOperator, NeighborhoodOperator);

  /** This function is called to create the operator.  The radius of
   * the operator is determine automatically.  */
  void
  CreateOperator();

  /** Set/Get the inner radius of the annulus. Radius is specified in
   * physical units (mm). */
  void
  SetInnerRadius(double r)
  {
    m_InnerRadius = r;
  }
  double
  GetInnerRadius() const
  {
    return m_InnerRadius;
  }

  /** Set/Get the thickness of the annulus.  The outer radius of the
   * annulus is defined as r = InnerRadius + Thickness. Thickness is
   * specified in physical units (mm). */
  void
  SetThickness(double t)
  {
    m_Thickness = t;
  }
  double
  GetThickness() const
  {
    return m_Thickness;
  }

  /** Set/Get the pixel spacings.  Setting these ensures the annulus
   * is round in physical space. Defaults to 1. */
  void
  SetSpacing(SpacingType & s)
  {
    m_Spacing = s;
  }
  const SpacingType &
  GetSpacing() const
  {
    return m_Spacing;
  }

  /** Set/Get whether kernel values are computed automatically or
   * specified manually */
  void
  SetNormalize(bool b)
  {
    m_Normalize = b;
  }
  bool
  GetNormalize() const
  {
    return m_Normalize;
  }
  void
  NormalizeOn()
  {
    this->SetNormalize(true);
  }
  void
  NormalizeOff()
  {
    this->SetNormalize(false);
  }

  /** If Normalize is on, you define the annulus to have a bright
   * center or a dark center. */
  void
  SetBrightCenter(bool b)
  {
    m_BrightCenter = b;
  }
  bool
  GetBrightCenter() const
  {
    return m_BrightCenter;
  }
  void
  BrightCenterOn()
  {
    this->SetBrightCenter(true);
  }
  void
  BrightCenterOff()
  {
    this->SetBrightCenter(false);
  }

  /** If Normalize is off, the interior to annulus, the
   * annulus (region between the two circles), and the region exterior to the
   * annulus to be defined manually.  Defauls are 0, 1, 0
   * respectively. */
  void
  SetInteriorValue(TPixel v)
  {
    m_InteriorValue = v;
  }
  TPixel
  GetInteriorValue() const
  {
    return m_InteriorValue;
  }
  void
  SetAnnulusValue(TPixel v)
  {
    m_AnnulusValue = v;
  }
  TPixel
  GetAnnulusValue() const
  {
    return m_AnnulusValue;
  }
  void
  SetExteriorValue(TPixel v)
  {
    m_ExteriorValue = v;
  }
  TPixel
  GetExteriorValue() const
  {
    return m_ExteriorValue;
  }

  /** Prints some debugging information */
  void
  PrintSelf(std::ostream & os, Indent i) const override
  {
    os << i << "AnnulusOperator { this=" << this << ", m_InnerRadius = " << m_InnerRadius
       << ", m_Thickness = " << m_Thickness << ", m_Spacing = " << m_Spacing << ", m_Normalize = " << m_Normalize
       << ", m_BrightCenter = " << m_BrightCenter << ", m_InteriorValue = " << m_InteriorValue
       << ", m_ExteriorValue = " << m_ExteriorValue << "}" << std::endl;
    Superclass::PrintSelf(os, i.GetNextIndent());
  }

protected:
  /** Typedef support for coefficient vector type.  Necessary to
   *  work around compiler bug on VC++. */
  using CoefficientVector = typename Superclass::CoefficientVector;
  using PixelType = typename Superclass::PixelType;

  /** Calculates operator coefficients. */
  CoefficientVector
  GenerateCoefficients() override;

  /** Arranges coefficients spatially in the memory buffer. */
  void
  Fill(const CoefficientVector & c) override;

private:
  double      m_InnerRadius{ 1.0 };
  double      m_Thickness{ 1.0 };
  bool        m_Normalize{ false };
  bool        m_BrightCenter{ false };
  PixelType   m_InteriorValue{ NumericTraits<PixelType>::ZeroValue() };
  PixelType   m_AnnulusValue{ NumericTraits<PixelType>::OneValue() };
  PixelType   m_ExteriorValue{ NumericTraits<PixelType>::ZeroValue() };
  SpacingType m_Spacing{ 1.0 };
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAnnulusOperator.hxx"
#endif
#endif
