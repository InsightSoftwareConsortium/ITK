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
#ifndef itkCurvatureNDAnisotropicDiffusionFunction_h
#define itkCurvatureNDAnisotropicDiffusionFunction_h

#include "itkScalarAnisotropicDiffusionFunction.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkDerivativeOperator.h"

namespace itk
{
/**
 * \class CurvatureNDAnisotropicDiffusionFunction
 *
 * This class implements a variation on the classic, Perona-Malik anisotropic
 * image diffusion equation as described in
 * itkGradientNDAnisotropicDiffusionFunction.  This object is a level-set
 * analog of that equation and will be referred to below as the \em modified
 * \em curvature \em diffusion \em equation (MCDE).  MCDE does not exhibit
 * the edge enhancing properties of classic anisotropic diffusion, which can under
 * certain conditions undergo a "negative" diffusion,which enhances the
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
 *  R. Whitaker and X. Xue. Variable-Conductance, Level-Set Curvature for
 *  Image Denoising, International Conference on Image Processing, 2001
 *  pp. 142-145, Vol.3.
 *
 *
 * \sa AnisotropicDiffusionFunction
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ImageEnhancement
 * \todo References
 * \ingroup ITKAnisotropicSmoothing
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT CurvatureNDAnisotropicDiffusionFunction:
  public ScalarAnisotropicDiffusionFunction< TImage >
{
public:
  /** Standard class typedefs. */
  typedef CurvatureNDAnisotropicDiffusionFunction      Self;
  typedef ScalarAnisotropicDiffusionFunction< TImage > Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(CurvatureNDAnisotropicDiffusionFunction,
               ScalarAnisotropicDiffusionFunction);

  /** Inherit some parameters from the superclass type. */
  typedef typename Superclass::ImageType        ImageType;
  typedef typename Superclass::PixelType        PixelType;
  typedef typename Superclass::TimeStepType     TimeStepType;
  typedef typename Superclass::RadiusType       RadiusType;
  typedef typename Superclass::NeighborhoodType NeighborhoodType;
  typedef typename Superclass::FloatOffsetType  FloatOffsetType;

  typedef typename NeighborhoodType::SizeValueType NeighborhoodSizeValueType;

  /** Inherit some parameters from the superclass type. */
  itkStaticConstMacro(ImageDimension, unsigned int, Superclass::ImageDimension);

  /** Compute incremental update. */
  virtual PixelType ComputeUpdate(const NeighborhoodType & neighborhood,
                                  void *globalData,
                                  const FloatOffsetType & offset = FloatOffsetType(0.0)
                                  ) ITK_OVERRIDE;

  /** This method is called prior to each iteration of the solver. */
  virtual void InitializeIteration() ITK_OVERRIDE
  {
    m_K = static_cast< PixelType >( this->GetAverageGradientMagnitudeSquared()
                                    * this->GetConductanceParameter()
                                    * this->GetConductanceParameter() * -2.0f );
  }

protected:
  CurvatureNDAnisotropicDiffusionFunction();
  ~CurvatureNDAnisotropicDiffusionFunction() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(CurvatureNDAnisotropicDiffusionFunction);

  /** Inner product function. */
  NeighborhoodInnerProduct< ImageType > m_InnerProduct;

  /** Slices for the ND neighborhood. */
  std::slice x_slice[ImageDimension];
  std::slice xa_slice[ImageDimension][ImageDimension];
  std::slice xd_slice[ImageDimension][ImageDimension];

  /** Derivative operator */
  DerivativeOperator< PixelType, itkGetStaticConstMacro(ImageDimension) > dx_op;

  /** Modified global average gradient magnitude term. */
  PixelType m_K;

  /** */
  static double m_MIN_NORM;

  NeighborhoodSizeValueType m_Center;
  NeighborhoodSizeValueType m_Stride[ImageDimension];
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCurvatureNDAnisotropicDiffusionFunction.hxx"
#endif

#endif
