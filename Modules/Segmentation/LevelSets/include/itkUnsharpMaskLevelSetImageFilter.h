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
#ifndef itkUnsharpMaskLevelSetImageFilter_h
#define itkUnsharpMaskLevelSetImageFilter_h

#include "itkSparseFieldFourthOrderLevelSetImageFilter.h"

namespace itk
{
/**
 * \class UnsharpMaskLevelSetImageFilter
 *
 * \brief This class implements a detail enhancing filter by making use of the
 * 4th-order level set isotropic diffusion (smoothing) PDE.
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
 * This filter is an example of how the 4th order level set PDE framework can
 * be used for general purpose surface processing. It is motivated by unsharp
 * masking from image processing which is a way of enhancing detail. This
 * filter acts much like the IsotropicFourthOrderLevelSetImageFilter because it
 * first smoothes the normal vectors via isotropic diffusion. However, as a
 * post-processing step we extrapolate from the original normals in the
 * direction opposite to the new processes normals. By refiting the surface to
 * these extrapolated vectors we achieve detail enhancement. This process is
 * not the same as running the isotropic diffusion process in reverse.
 *
 * \par IMPORTANT
 * Because this filters enhances details on the surface, it will also amplify
 * noise! This filter is provided only as an example of graphics oriented
 * post-processing. Do not use it on noisy data.
 *
 * \par PARAMETERS
 * As mentioned before, the IsoSurfaceValue parameter chooses which isosurface
 * of the input to process. The MaxFilterIterations parameter determine the
 * number of iterations for which this filter will run. Since, this filter
 * enhances detail AND noise MaxFilterIterations above a couple of hundred are
 * unreasonable. Finally NormalProcessUnsharpWeight controls the amount of
 * extrapolation (or equivalently the amount of detail enhancement). This value
 * should be in the range [0.1,1] for reasonable results.
 * \ingroup ITKLevelSets
 */
template< typename TInputImage, typename TOutputImage >
class ITK_TEMPLATE_EXPORT UnsharpMaskLevelSetImageFilter:
  public SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs */
  typedef UnsharpMaskLevelSetImageFilter                                         Self;
  typedef SparseFieldFourthOrderLevelSetImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                                                   Pointer;
  typedef SmartPointer< const Self >                                             ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(UnsharpMaskLevelSetImageFilter,
               SparseFieldFourthOrderLevelSetImageFilter);

  /** Standard new macro */
  itkNewMacro(Self);

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
  UnsharpMaskLevelSetImageFilter();
  ~UnsharpMaskLevelSetImageFilter() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** The LevelSetFunctionWithRefitTerm object. */
  typename FunctionType::Pointer m_Function;

  /** The number of iterations for which this filter will run. */
  unsigned int m_MaxFilterIteration;

  /** This filter halts when the iteration count reaches the specified count. */
  virtual bool Halt() ITK_OVERRIDE
  {
    if ( this->GetElapsedIterations() == m_MaxFilterIteration )
      {
      return true;
      }
    else
      {
      return false;
      }
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(UnsharpMaskLevelSetImageFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUnsharpMaskLevelSetImageFilter.hxx"
#endif

#endif
