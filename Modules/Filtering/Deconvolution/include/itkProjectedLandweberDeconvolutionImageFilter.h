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
#ifndef itkProjectedLandweberDeconvolutionImageFilter_h
#define itkProjectedLandweberDeconvolutionImageFilter_h

#include "itkProjectedIterativeDeconvolutionImageFilter.h"
#include "itkLandweberDeconvolutionImageFilter.h"

namespace itk
{
/** \class ProjectedLandweberDeconvolutionImageFilter
 * \brief Deconvolve an image using the projected Landweber
 * deconvolution algorithm.
 *
 * This filter performs the same calculation per iteration as the
 * LandweberDeconvolutionImageFilter. However, at each iteration,
 * negative pixels in the intermediate result are projected (set) to
 * zero. This is useful if the solution is assumed to always be
 * non-negative, which is the case when dealing with images formed by
 * counting photons, for example.
 *
 * This code was adapted from the Insight Journal contribution:
 *
 * "Deconvolution: infrastructure and reference algorithms"
 * by Gaetan Lehmann
 * https://hdl.handle.net/10380/3207
 *
 * \author Gaetan Lehmann, Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France
 * \author Cory Quammen, The University of North Carolina at Chapel Hill
 *
 * \ingroup ITKDeconvolution
 * \sa IterativeDeconvolutionImageFilter
 * \sa RichardsonLucyDeconvolutionImageFilter
 * \sa LandweberDeconvolutionImageFilter
 */
template< typename TInputImage, typename TKernelImage=TInputImage, typename TOutputImage=TInputImage, typename TInternalPrecision=double >
class ITK_TEMPLATE_EXPORT ProjectedLandweberDeconvolutionImageFilter :
    public ProjectedIterativeDeconvolutionImageFilter< LandweberDeconvolutionImageFilter< TInputImage, TKernelImage, TOutputImage, TInternalPrecision > >
{
public:
    /** Standard typedefs. */
  typedef ProjectedLandweberDeconvolutionImageFilter                  Self;
  typedef ProjectedIterativeDeconvolutionImageFilter<
            LandweberDeconvolutionImageFilter< TInputImage,
                                               TKernelImage,
                                               TOutputImage,
                                               TInternalPrecision > > Superclass;
  typedef SmartPointer< Self >                                        Pointer;
  typedef SmartPointer< const Self >                                  ConstPointer;

  /** Other useful typedefs. */
  typedef TInputImage  InputImageType;
  typedef TKernelImage KernelImageType;
  typedef TOutputImage OutputImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ProjectedLandweberDeconvolutionImageFilter,
               ProjectedIterativeDeconvolutionImageFilter);

protected:
  ProjectedLandweberDeconvolutionImageFilter();
  virtual ~ProjectedLandweberDeconvolutionImageFilter() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ProjectedLandweberDeconvolutionImageFilter);

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkProjectedLandweberDeconvolutionImageFilter.hxx"
#endif


#endif
