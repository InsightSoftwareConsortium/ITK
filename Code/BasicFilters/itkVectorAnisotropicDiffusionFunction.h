/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorAnisotropicDiffusionFunction.h
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
#ifndef __itkVectorAnisotropicDiffusionFunction_h_
#define __itkVectorAnisotropicDiffusionFunction_h_

#include "itkAnisotropicDiffusionFunction.h"
#include "itkVector.h"

namespace itk {

/** \class VectorAnisotropicDiffusionFunction
 *
 * This class implements a vector-valued version of
 * AnisotropicDiffusionFunction.  Typically in vector-valued diffusion, vector
 * components are diffused independently of one another using a conductance
 * term that is linked across the components. Refer to the the documentation of
 * AnisotropicDiffusionFunction for an overview of anisotropic diffusion.  The
 * way that the conductance term is calculated is specific to the specific type
 * of diffusion function.
 *
 * \par Data type requirements
 * This filter was designed to process itk::Images of itk::Vector type.  The code 
 * relies on various typedefs and overloaded operators defined in itk::Vector.
 * It is perfectly reasonable, however, to apply this filter to images of other,
 * user-defined types as long as the appropriate typedefs and operator overloads
 * are in place.  As a general rule, follow the example of itk::Vector in
 * defining your data types.
 *
 *  \ingroup Functions
 *  \ingroup ImageEnhancement
 *
 * \sa AnisotropicDiffusionFunction
 * \sa ScalarAnisotropicDiffusionFunction
 * */
template <class TImage>
class VectorAnisotropicDiffusionFunction :
    public AnisotropicDiffusionFunction<TImage>
{
public:
  /** Standard class typedefs. */
  typedef VectorAnisotropicDiffusionFunction   Self;
  typedef AnisotropicDiffusionFunction<TImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(VectorAnisotropicDiffusionFunction,
               AnisotropicDiffusionFunction);
  
  /** Inherit some parameters from the superclass type */
  typedef typename Superclass::ImageType        ImageType;
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType BoundaryNeighborhoodType;

  /** Inherit some parameters from the superclass type */
  enum { ImageDimension = Superclass::ImageDimension };
  enum { VectorDimension = PixelType::VectorDimension };

  /** Compute the average gradient magnitude squared. */
  virtual void CalculateAverageGradientMagnitudeSquared(TImage *);

protected:
  VectorAnisotropicDiffusionFunction() {}
  ~VectorAnisotropicDiffusionFunction() {}
  void PrintSelf(std::ostream& os, Indent indent) const
    { Superclass::PrintSelf(os,indent); }
private:
  VectorAnisotropicDiffusionFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorAnisotropicDiffusionFunction.txx"
#endif

#endif
