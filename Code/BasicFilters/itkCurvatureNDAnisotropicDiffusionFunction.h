/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCurvatureNDAnisotropicDiffusionFunction.h
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
#ifndef __itkCurvatureNDAnisotropicDiffusionFunction_h_
#define __itkCurvatureNDAnisotropicDiffusionFunction_h_

#include "itkScalarAnisotropicDiffusionFunction.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"

namespace itk {

/**
 * \class CurvatureNDAnisotropicDiffusionFunction
 *
 * This class implements a variation on the classic, Perona-Malik anisotropic
 * image diffusion equation as described in
 * itkGradientNDAnisotropicDiffusionFunction.  This object is a level-set
 * analog of that equation and will be referred to below as the \em modified
 * \em curvature \em diffusion \em equation ( \MCDE ).  MCDE does not exhibit
 * the edge enhancing properties of classic anisotropic diffusion, which can under 
 * certain conditions undergo a ``negative'' diffusion,which enhances the
 * contrast of edges.  Equations of the form of MCDE always undergo positive
 * diffusion, with the conductance term only varying the strength of that
 * diffusion.
 *
 * \par
 * Qualitatively, MCDE compares well with other non-linear diffusion
 * techniques.  It is less sensitive to contrast than classic Perona-Malik
 * style diffusion, and preserves finer detailed structures in images.
 * There is a potential speed trade-off for using this function in place of
 * itkGradientNDAnisotropicDiffusionFunction.  Each iteration of the solution
 * takes roughly twice as long.  Fewer iterations, however, may be required to
 * reach an acceptable solution.
 * 
 * \par
 * The MCDE equation is given as:
 *
 * \f[ f_t = \mid \nabla f \mid \nabla \cdot c( \mid \nabla f \mid ) \frac{
 * \nabla f }{ \mid \nabla f \mid } \f] ,
 *
 * \par
 * where the conductance modified curvature term is
 *
 * \f[ \nabla \cdot \frac{\nabla f}{\mid \nabla f \mid} \f] .
 *
 * \par References
 *
 *
 *  
 * \sa AnisotropicDiffusionFunction
 * \ingroup Functions
 * \ingroup ImageEnhancement
 * \todo References */
template <class TImage>
class CurvatureNDAnisotropicDiffusionFunction :
    public ScalarAnisotropicDiffusionFunction<TImage>
{
public:
  /** Standard class typedefs. */
  typedef CurvatureNDAnisotropicDiffusionFunction Self;
  typedef ScalarAnisotropicDiffusionFunction<TImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( CurvatureNDAnisotropicDiffusionFunction,
                ScalarAnisotropicDiffusionFunction );
  
  /** Inherit some parameters from the superclass type. */
  typedef typename Superclass::ImageType        ImageType;
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::BoundaryNeighborhoodType
    BoundaryNeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;

  /** Inherit some parameters from the superclass type. */
  enum { ImageDimension = Superclass::ImageDimension };

  /** Compute incremental update. */
  virtual PixelType ComputeUpdate(const NeighborhoodType &neighborhood,
                                  void *globalData,
                                  const FloatOffsetType& offset = m_ZeroOffset
                                  ) const;
  virtual PixelType ComputeUpdate(const BoundaryNeighborhoodType
                                  &neighborhood, void *globalData,
                                  const FloatOffsetType& offset = m_ZeroOffset
                                  ) const;

  /** This method is called prior to each iteration of the solver. */
  virtual void InitializeIteration()
    {
      m_k = this->GetAverageGradientMagnitudeSquared() *
        this->GetConductanceParameter() * -1.0f;
    }
  
protected:
  CurvatureNDAnisotropicDiffusionFunction();
  ~CurvatureNDAnisotropicDiffusionFunction() {}
  void PrintSelf(std::ostream& os, Indent indent) const
    {
      Superclass::PrintSelf(os,indent);
    }
  
private:
  CurvatureNDAnisotropicDiffusionFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Inner product function. */
  NeighborhoodInnerProduct<ImageType> m_InnerProduct;

  /** Boundary Inner product function. */
  SmartNeighborhoodInnerProduct<ImageType> m_SmartInnerProduct;

  /** Slices for the ND neighborhood. */
  std::slice  x_slice[ImageDimension];
  std::slice xa_slice[ImageDimension][ImageDimension];
  std::slice xd_slice[ImageDimension][ImageDimension];

  /** Derivative operator */
  DerivativeOperator<PixelType, ImageDimension> dx_op;

  /** Modified global average gradient magnitude term. */
  PixelType m_k;

  /** */
  static double m_MIN_NORM;
  
  unsigned long m_Center;
  unsigned long m_Stride[ImageDimension];

};


  
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvatureNDAnisotropicDiffusionFunction.txx"
#endif

#endif
