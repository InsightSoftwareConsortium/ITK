/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEllipsoidInteriorExteriorSpatialFunction.h
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
#ifndef __itkEllipsoidInteriorExteriorSpatialFunction_h
#define __itkEllipsoidInteriorExteriorSpatialFunction_h

#include "itkInteriorExteriorSpatialFunction.h"

namespace itk
{

/**
 * \class EllipsoidSpatialFunction
 * \brief Function implementation of an ellipsoid
 *
 * Implements a function that returns 1 for points inside or on the surface
 * of a ellipsoid and 0 for points outside the ellipsoid. The orientation of the 
 * n-dimensional ellipsoid axes are defined by n orthogonal vectors.
 * See Examples/EllipsoidInteriorExteriorSpatialFunction/README for an example
 * of creating an Ellipsoid in an image.
 *
 **/

template <class T, unsigned int VImageDimension=3>
class ITK_EXPORT EllipsoidInteriorExteriorSpatialFunction : public InteriorExteriorSpatialFunction<VImageDimension>
{
public:

  /**
   * Standard "Self" typedef.
   */
  typedef EllipsoidInteriorExteriorSpatialFunction Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef InteriorExteriorSpatialFunction<VImageDimension> Superclass;
   
    
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
  itkTypeMacro(EllipsoidInteriorExteriorSpatialFunction,InteriorExteriorSpatialFunction);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Vector typedef.
   */
  typedef T VectorType;

  /**
   * Get and set the center of the ellipsoid.
   */
  itkGetMacro(Center, InputType);
  itkSetMacro(Center, InputType);

  /**
   * Get and set the axes lengths of the ellipsoid.
   */
  itkGetMacro(Axes, InputType);
  itkSetMacro(Axes, InputType);

  /**
   * Set the orientation vectors (must be orthogonal) of the ellipsoid axes.
   * Must be normalized!!!!!
   */
  void SetOrientations(vnl_matrix<VectorType>);

  /**
   * Evaluates the function at a given position.
   */
  OutputType Evaluate(const InputType& position) const;
     
protected:

  EllipsoidInteriorExteriorSpatialFunction();
  virtual ~EllipsoidInteriorExteriorSpatialFunction();

  EllipsoidInteriorExteriorSpatialFunction(const EllipsoidInteriorExteriorSpatialFunction&) {};
  void operator=(const EllipsoidInteriorExteriorSpatialFunction&) {};

private:

  /**
   * The center of the ellipsoid.
   */
  InputType m_Center;

  /**
   * The axes lenths of the ellipsoid.
   */
  InputType m_Axes;

  /**
   * The orientation vectors (must be orthogonal) of the ellipsoid axes.
   */  
  VectorType ** m_orientations;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEllipsoidInteriorExteriorSpatialFunction.txx"
#endif

#endif
