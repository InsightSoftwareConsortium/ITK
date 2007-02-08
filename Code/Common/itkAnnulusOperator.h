/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnnulusOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAnnulusOperator_h
#define __itkAnnulusOperator_h

#include "itkExceptionObject.h"
#include "itkNeighborhoodOperator.h"
#include "itkVector.h"

namespace itk {

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
 * \sa NeighborhoodOperator
 * \sa Neighborhood
 * 
 * \ingroup Operators
 */
template<class TPixel, unsigned int TDimension=2,
  class TAllocator = NeighborhoodAllocator<TPixel> >
class ITK_EXPORT AnnulusOperator
  : public NeighborhoodOperator<TPixel, TDimension, TAllocator>
{
public:
  /** Standard typedefs */
  typedef AnnulusOperator                                       Self;
  typedef NeighborhoodOperator<TPixel, TDimension, TAllocator>  Superclass;

  /** Additional typedefs */
  typedef typename Superclass::SizeType   SizeType;
  typedef typename Superclass::OffsetType OffsetType;
  typedef Vector<double, TDimension>      SpacingType;
  
  itkTypeMacro(AnnulusOperator, NeighborhoodOperator);
  
  AnnulusOperator()
    : NeighborhoodOperator<TPixel, TDimension, TAllocator>(),
      m_Normalize(false), m_BrightCenter(false),
      m_InteriorValue(NumericTraits<PixelType>::Zero),
      m_AnnulusValue(NumericTraits<PixelType>::One),
      m_ExteriorValue(NumericTraits<PixelType>::Zero)
    { m_Spacing.Fill(1.0); }

  AnnulusOperator(const Self& other)
    : NeighborhoodOperator<TPixel, TDimension, TAllocator>(other)
    {
    m_InnerRadius = other.m_InnerRadius;
    m_Thickness = other.m_Thickness;
    m_Spacing = other.m_Spacing;
    m_InteriorValue = other.m_InteriorValue;
    m_AnnulusValue = other.m_AnnulusValue;
    m_ExteriorValue = other.m_ExteriorValue;
    m_Normalize = other.m_Normalize;
    m_BrightCenter = other.m_BrightCenter;
    }

  /** This function is called to create the operator.  The radius of
   * the operator is determine automatically.  */ 
  void CreateOperator();  

  /** Set/Get the inner radius of the annulus. Radius is specified in
   * physical units (mm). */
  void SetInnerRadius(double r)
    { m_InnerRadius = r; }
  double GetInnerRadius() const
    { return m_InnerRadius; }

  /** Set/Get the thickness of the annulus.  The outer radius of the
   * annulus is defined as r = InnerRadius + Thickness. Thickness is
   * specified in physical units (mm). */
  void SetThickness(double t)
    { m_Thickness = t; }
  double GetThickness() const
    { return m_Thickness; }

  /** Set/Get the pixel spacings.  Setting these ensures the annulus
   * is round in physical space. Defaults to 1. */
  void SetSpacing(SpacingType &s)
    { m_Spacing = s; }
  const SpacingType& GetSpacing() const
    { return m_Spacing; }
  
  /** Set/Get whether kernel values are computed automatically or
   * specified manually */
  void SetNormalize(bool b)
    { m_Normalize = b; }
  bool GetNormalize() const
    { return m_Normalize; }
  void NormalizeOn()
    { this->SetNormalize(true); }
  void NormalizeOff()
    { this->SetNormalize(false); }

  /** If Normalize is on, you define the annulus to have a bright
   * center or a dark center. */
  void SetBrightCenter(bool b)
    { m_BrightCenter = b; }
  bool GetBrightCenter() const
    { return m_BrightCenter; }
  void BrightCenterOn()
    { this->SetBrightCenter(true); }
  void BrightCenterOff()
    { this->SetBrightCenter(false); }

  /** If Normalize is off, the interior to annulus, the
   * annulus (region between the two circles), and the region exterior to the
   * annulus to be defined manually.  Defauls are 0, 1, 0
   * respectively. */
  void SetInteriorValue(TPixel v)
    { m_InteriorValue = v; }
  TPixel GetInteriorValue() const
    { return m_InteriorValue; }
  void SetAnnulusValue(TPixel v)
    { m_AnnulusValue = v; }
  TPixel GetAnnulusValue() const
    { return m_AnnulusValue; }
  void SetExteriorValue(TPixel v)
    { m_ExteriorValue = v; }
  TPixel GetExteriorValue() const
    { return m_ExteriorValue; }
  
  /** Assignment operator */
  Self &operator=(const Self& other)
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
  virtual void PrintSelf(std::ostream &os, Indent i) const  
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
    Superclass::PrintSelf(os, i.GetNextIndent());
    }
  
protected:
  
  /** Typedef support for coefficient vector type.  Necessary to
   *  work around compiler bug on VC++. */
  typedef typename Superclass::CoefficientVector CoefficientVector;
  typedef typename Superclass::PixelType         PixelType;

  /** Calculates operator coefficients. */
  CoefficientVector GenerateCoefficients();

  /** Arranges coefficients spatially in the memory buffer. */
  void Fill(const CoefficientVector &c);

private:

  double      m_InnerRadius;
  double      m_Thickness;
  bool        m_Normalize;
  bool        m_BrightCenter;
  PixelType   m_InteriorValue;
  PixelType   m_AnnulusValue;
  PixelType   m_ExteriorValue;
  SpacingType m_Spacing;
};

} // namespace itk


// Define instantiation macro for this template.
#define ITK_TEMPLATE_AnnulusOperator(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT AnnulusOperator< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef AnnulusOperator< ITK_TEMPLATE_2 x > \
                                                  AnnulusOperator##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkAnnulusOperator+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkAnnulusOperator.txx"
#endif

#endif
