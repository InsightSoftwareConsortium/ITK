/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkAcosImageAdaptor.h
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
#ifndef __itkFiniteDifferenceFunction_h_
#define __itkFiniteDifferenceFunction_h_

#include "itkLightObject.h"
#include "itkNeighborhoodIterator.h"
#include "itkRegionNeighborhoodIterator.h"
#include "itkSmartRegionNeighborhoodIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"

namespace itk {

/**
 * \class FiniteDifferenceFunction
 *
 * \warning  The Evaluate() methods are declared as const to enforce
 *  thread-safety during execution of FiniteDifferenceImageFilter
 *  algorithms.
 */
template<class TImageType>
class FiniteDifferenceFunction : public LightObject
{
public:

 /**
   * Standard itk Self & Superclass typedefs
   */
  typedef FiniteDifferenceFunction Self;
  typedef LightObject Superclass;

  /**
   * Extract some parameters from the image type
   */
  typedef TImageType ImageType;
  enum { ImageDimension = ImageType::ImageDimension };
  typedef typename ImageType::PixelType PixelType;

  /**
   * The default boundary condition for finite difference
   * functions that is used unless overridden in the Evaluate() method.
   */
  typedef ZeroFluxNeumannBoundaryCondition<ImageType>
    DefaultBoundaryConditionType;
  
  /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( FiniteDifferenceFunction, LightObject );
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Neighborhood radius type
   */
  typedef typename NeighborhoodIterator<TImageType>::RadiusType RadiusType;

  /**
   * 
   */
  typedef RegionNeighborhoodIterator<TImageType> NeighborhoodType;

  /**
   *
   */
  typedef SmartRegionNeighborhoodIterator<TImageType,
    DefaultBoundaryConditionType> BoundaryNeighborhoodType;


  /**
   *
   */
  virtual void InitializeIteration() {};
  
  /**
   *
   */
  virtual PixelType  Evaluate(const NeighborhoodType &neighborhood,
                               PixelType &dt) const = 0;

  /**
   *
   */
  virtual PixelType  Evaluate(const BoundaryNeighborhoodType
                               &neighborhood, PixelType &dt) const = 0;
  
  /**
   *  
   */
  void SetRadius(const RadiusType &r)
    {      m_Radius = r;    }
  /**
   *
   */
  const RadiusType &GetRadius() const
    {      return m_Radius;    }

  /**
   * 
   */
  virtual PixelType GetInitialTimeStep() const = 0;

protected:
  FiniteDifferenceFunction() {}
  ~FiniteDifferenceFunction() {}
  FiniteDifferenceFunction(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent)
  {
    os << indent << "FiniteDifferenceFunction";
    Superclass::PrintSelf(os, indent.GetNextIndent() );
  }
  
  RadiusType m_Radius;
};

  
} // end namespace itk


#endif
