
/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConicShellInteriorExteriorSpatialFunction.h
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
#ifndef __itkConicShellInteriorExteriorSpatialFunction_h
#define __itkConicShellInteriorExteriorSpatialFunction_h

#include "vnl/vnl_vector_fixed.h"
#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{

/**
 * \class ConicShellInteriorExteriorSpatialFunction
 * \brief Spatial function implementation of a conic shell
 *
 * We are creating search areas from BoundaryPoint1 in which to look for 
 * candidate BoundaryPoint2's with which to form core atoms.  Assume the 
 * "worst case" that BoundaryPoint2 is somewhere in that search area pointing
 * directly at BoundaryPoint1. 
 *
 * The search area (ConicShell?) from each BoundaryPoint1 has the following parameters: 
 *
 * DistanceMax and DistanceMin from the location of the BoundaryPoint 
 *
 * AngleMax from the line along the gradient at the boundary point.
 * This is determined in n dimensions by taking the dot product of two vectors,
 * (1) the normalized gradient at BoundaryPoint1 and
 * (2) the normalized vector from BoundaryPoint1 to BoundaryPoint2.
 *
 * If the absolute value of that dot product is greater than (1 - epsilon)
 * then you are in the ConicShell.  This epsilon is the same one determining
 * face-to-faceness in the IEEE TMI paper. 
 *
 * Polarity, i.e. which direction along the gradient of BoundaryPoint1 you want to look.
 *
 * */

template <unsigned int VImageDimension=3>
class ITK_EXPORT ConicShellInteriorExteriorSpatialFunction : public InteriorExteriorSpatialFunction<VImageDimension>
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef ConicShellInteriorExteriorSpatialFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef InteriorExteriorSpatialFunction<VImageDimension> Superclass;

  typedef typename Superclass::TFunctionValueType TFunctionValueType;
  typedef typename Superclass::TVectorType TVectorType;
  
  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  itkTypeMacro(ConicShellInteriorExteriorSpatialFunction,InteriorExteriorSpatialFunction);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Evaluates the function at a given position
   */
  TFunctionValueType Evaluate(TVectorType* position);

  itkGetMacro( Origin, TVectorType);
  itkSetMacro( Origin, TVectorType);

  itkGetMacro( OriginGradient, TVectorType);
  itkSetMacro( OriginGradient, TVectorType);

  itkGetMacro( DistanceMin, double);
  itkSetMacro( DistanceMin, double);

  itkGetMacro( DistanceMax, double);
  itkSetMacro( DistanceMax, double);

  itkGetMacro( Epsilon, double);
  itkSetMacro( Epsilon, double);

  itkGetMacro( Polarity, bool);
  itkSetMacro( Polarity, bool);
     
protected:

  ConicShellInteriorExteriorSpatialFunction();
  virtual ~ConicShellInteriorExteriorSpatialFunction();

  ConicShellInteriorExteriorSpatialFunction(const ConicShellInteriorExteriorSpatialFunction&) {};
  void operator=(const ConicShellInteriorExteriorSpatialFunction&) {};

private:

  TVectorType m_Origin;
  TVectorType m_OriginGradient;

  double m_DistanceMin;
  double m_DistanceMax;
  double m_Epsilon;
  bool m_Polarity;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConicShellInteriorExteriorSpatialFunction.txx"
#endif

#endif
