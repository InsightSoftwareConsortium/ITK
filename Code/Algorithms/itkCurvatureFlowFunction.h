/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkCurvatureFlowFunction.h
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
#ifndef __itkCurvatureFlowFunction_h_
#define __itkCurvatureFlowFunction_h_

#include "itkFiniteDifferenceEquation.h"
#include "itkMacro.h"

namespace itk {

/**
 * \class CurvatureFlowFunction
 *  
 * This class encapsulate the finite difference equation which drives a curvature
 * flow denoising algorithm.
 *
 * This class operates as part of the finite difference solver hierarchy.
 *
 * \sa CurvatureFlowImageFilter.
 */ 
template <class TImage>
class CurvatureFlowFunction :
    public FiniteDifferenceEquation<TImage>
{
public:

  /**
   *  Standard "Self" typedef.
   */
  typedef CurvatureFlowFunction Self;

  /**
   * Standard "Superclass" typedef.
   */   
  typedef FiniteDifferenceEquation<TImage> Superclass;

  /** 
   * Smart pointer support for this class.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro( CurvatureFlowFunction,
                FiniteDifferenceFunction );
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Inherit some parameters from the superclass type
   */
  typedef typename Superclass::PixelType PixelType;
  typedef typename Superclass::RadiusType RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType BoundaryNeighborhoodType;
  typedef typename Superclass::FloatOffsetType FloatOffsetType;
  typedef typename Superclass::TimeStepType TimeStepType;
  enum { ImageDimension = Superclass::ImageDimension };  

  /**
   * Returns the time step supplied by the user.  We don't need to use the
   * global data supplied since we are returning a fixed value.
   */
  virtual TimeStepType ComputeGlobalTimeStep(void *GlobalData) const
    { return this->GetTimeStep(); }

  /**
   * This class does not use this particular parameter
   * so it's safe to return a null value.
   */
  virtual void *GetGlobalDataPointer() const
    {  return 0; }

  /**
   * Does nothing.  No global data is used in this class of equations.
   */
  virtual void ReleaseGlobalDataPointer(void *GlobalData) const
    { /* do nothing */ }

  /**
   * Set the time step parameter
   */
  void SetTimeStep( const TimeStepType & t )
    { m_TimeStep = t; }

  /**
   * Get the time step parameter
   */
  const TimeStepType &GetTimeStep() const
    { return m_TimeStep; }


  /**
   * This method computes the solution update for each pixel that does not
   * lie on a the data set boundary.
   */
  virtual PixelType ComputeUpdate(const NeighborhoodType &neighborhood,
                                  void * globalData,
                                  const FloatOffsetType& offset = m_ZeroOffset
                                  ) const;

  /**
   * This method computes the solution update for each pixel that lies
   * on the data set boundary.
   */
  virtual PixelType ComputeUpdate(const BoundaryNeighborhoodType
                                  &neighborhood, void * globalData,
                                  const FloatOffsetType& offset = m_ZeroOffset
                                  ) const;

  
protected:

  CurvatureFlowFunction();
  ~CurvatureFlowFunction() {}
  CurvatureFlowFunction(const Self&) {}
  void operator=(const Self&) {}

private:

  TimeStepType       m_TimeStep;
  
  
};

}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvatureFlowFunction.txx"
#endif

#endif
