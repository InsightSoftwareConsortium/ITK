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
#ifndef itkIsotropicFourthOrderLevelSetImageFilter_h
#define itkIsotropicFourthOrderLevelSetImageFilter_h

#include "itkSparseFieldFourthOrderLevelSetImageFilter.h"

namespace itk
{
/**
 * \class IsotropicFourthOrderLevelSetImageFilter
 *
 * \brief This class implements the 4th-order level set isotropic diffusion
 * (smoothing) PDE.
 *
 * \par INPUT and OUTPUT
 * This is a volume to volume filter; however, it is meant to process (smooth)
 * surfaces. The input surface is an isosurface of the input volume. The
 * isosurface value to be processed can be set by calling SetIsoSurfaceValue
 * (default is 0). The output surface is the 0-isosurface of the output volume,
 * regardless of the input isosurface value. To visualize the input/output
 * surfaces to this filter a mesh extraction method such as marching cubes can
 * be used.
 *
 * \par
 * The 4th-order level set PDE framework is proposed as an alternative to 2nd
 * order PDEs. By order we mean the order of differentiation of the level set
 * image function required to compute derivatives for updating the image. For
 * instance, the popular curvature flow uses 2nd-order derivatives of the level
 * set image; hence, it is a 2nd order PDE.
 *
 * \par
 * 2nd-order curvature flow can be used by itself to smooth surfaces as a
 * post-processing filter or it can be used with other PDE terms such as a
 * Canny edge term that attracts the surface to strong edges in a data
 * image. Curvature flow smoothes surfaces by making the surface move in the
 * direction that will decrease surface area.
 *
 * \par
 * The 4th-order PDE framework provides an improvement over curvature
 * flow. Instead of making the surface move to decrease surface area it makes
 * the surface move to decrease total curvature. Similar to curvature flow,
 * these PDEs can be used alone or in conjunction with data terms. The
 * 4th-order PDE framework is implemented in
 * SparseFieldFourthOrderLevelSetImageFilter. This filter class, which is
 * derived from that, uses the 4th-order PDE by itself to implement an
 * isotropic surface smoothing algorithm. A feature preserving anisotropic
 * variant of this algorithm is implemented in
 * AnisotropicFourthOrderLevelSetImageFilter.
 *
 * \par PARAMETERS
 * As mentioned before, the IsoSurfaceValue parameter chooses which isosurface
 * of the input to process. The MaxFilterIterations parameter determine the
 * number of iterations for which this filter will run. The more iterations,
 * the more smoothing.
 * \ingroup ITKLevelSets
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT IsotropicFourthOrderLevelSetImageFilter:
  public SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs */
  typedef IsotropicFourthOrderLevelSetImageFilter Self;
  typedef SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(IsotropicFourthOrderLevelSetImageFilter,
               SparseFieldFourthOrderLevelSetImageFilter);

  /** Standard new macro */
  itkNewMacro (Self);

  /** The sparse image type used in LevelSetFunctionWithRefitTerm */
  typedef typename Superclass::SparseImageType SparseImageType;

  /** The level set function class with a refit term that forces the curvature
      of the moving front to match a prescribed curvature image. */
  typedef LevelSetFunctionWithRefitTerm< TOutputImage, SparseImageType > FunctionType;

  /** The radius type for the neighborhoods. */
  typedef typename FunctionType::RadiusType RadiusType;

  itkGetConstMacro(MaxFilterIteration, unsigned int);
  itkSetMacro(MaxFilterIteration, unsigned int);

protected:
  IsotropicFourthOrderLevelSetImageFilter();
  ~IsotropicFourthOrderLevelSetImageFilter() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** The LevelSetFunctionWithRefitTerm object. */
  typename FunctionType::Pointer m_Function;

  /** The number of iterations for which this filter will run. */
  unsigned int m_MaxFilterIteration;

  /** This filter halts when the iteration count reaches the specified count. */
  virtual bool Halt() ITK_OVERRIDE
  {
    if ( this->GetElapsedIterations() == m_MaxFilterIteration ) { return true; }
    else { return false; }
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(IsotropicFourthOrderLevelSetImageFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkIsotropicFourthOrderLevelSetImageFilter.hxx"
#endif

#endif
