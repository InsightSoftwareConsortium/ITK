/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFrustumSpatialFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkFrustumSpatialFunction_h
#define __itkFrustumSpatialFunction_h

#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{

/**
 * \class FrustumSpatialFunction
 * \brief Spatial function implementation of a truncated pyramid.
 *
 * Implements a function that returns 0 for points inside or on the surface
 * of a truncated pyrami, 1 for points outside the truncated pyramid
 * 
 * \ingroup SpatialFunctions
 *
 * */

template <unsigned int VImageDimension=3,typename TInput=Point<double,3> >
class ITK_EXPORT FrustumSpatialFunction : 
            public InteriorExteriorSpatialFunction<VImageDimension,TInput>
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef FrustumSpatialFunction<VImageDimension,TInput> Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef InteriorExteriorSpatialFunction<VImageDimension,TInput> Superclass;
  
  /**
   * Input type for the function
   */
  typedef typename Superclass::InputType InputType;

  /**
   * Output type for the function
   */
  typedef typename Superclass::OutputType OutputType;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FrustumSpatialFunction,InteriorExteriorSpatialFunction);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Evaluates the function at a given position
   */
  OutputType Evaluate(const InputType& position) const;

  /**
   * Get and set the center of the sphere
   */
  itkGetMacro( Apex, InputType);
  itkSetMacro( Apex, InputType);

  /**
   * Get and set the angle of the pyramid axis 
   * with respect to the Z axis
   */
  itkGetMacro( AngleZ, double);
  itkSetMacro( AngleZ, double);
     
  /**
   * Get and set the aperture angle in X
   */
  itkGetMacro( ApertureAngleX, double);
  itkSetMacro( ApertureAngleX, double);
     
  /**
   * Get and set the aperture angle in Y
   */
  itkGetMacro( ApertureAngleY, double);
  itkSetMacro( ApertureAngleY, double);
     
  /**
   * Get and set the top plane distance to the Apex
   */
  itkGetMacro( TopPlane, double);
  itkSetMacro( TopPlane, double);
     
  /**
   * Get and set the bottom plane distance to the Apex
   */
  itkGetMacro( BottomPlane, double);
  itkSetMacro( BottomPlane, double);
     
protected:

  FrustumSpatialFunction();
  virtual ~FrustumSpatialFunction();

  FrustumSpatialFunction(const FrustumSpatialFunction&) {};
  void operator=(const FrustumSpatialFunction&) {};

private:

  /**
   * The apex of the pyramid (of the same type as Input)
   */
  InputType m_Apex;

  /**
   * Angle between the pyramid axis and the Z axis
   */
  double m_AngleZ;

  /**
   * Aperture Angle in X direction
   */
  double m_ApertureAngleX;

  /**
   * Aperture Angle in Y direction
   */
  double m_ApertureAngleY;

  /**
   * Distance from Apex to top plane
   */
  double m_TopPlane;

  /**
   * Distance from Apex to bottom plane
   */
  double m_BottomPlane;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFrustumSpatialFunction.txx"
#endif

#endif




