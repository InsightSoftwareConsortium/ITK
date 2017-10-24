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
#ifndef itkGPUBoxImageFilter_h
#define itkGPUBoxImageFilter_h

#include "itkGPUImageToImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkBoxImageFilter.h"

namespace itk
{
/**
 * \class GPUBoxImageFilter
 * \brief A base class for all the GPU filters working on a box neighborhood
 *
 * This filter provides the code to store the radius information about the
 * neighborhood used in the subclasses.
 * It reuses the GenerateInputRequestedRegion() defined in BoxImageFilter class.
 *
 * \author Won-Ki Jeong
 * \ingroup ITKGPUImageFilterBase
 */

template< typename TInputImage, typename TOutputImage, typename TParentImageFilter = BoxImageFilter< TInputImage, TOutputImage > >
class GPUBoxImageFilter :
  public GPUImageToImageFilter< TInputImage, TOutputImage, TParentImageFilter >
{
public:
  /** Standard class typedefs. */
  typedef GPUBoxImageFilter                                                      Self;
  typedef GPUImageToImageFilter< TInputImage, TOutputImage, TParentImageFilter > GPUSuperclass;
  typedef TParentImageFilter                                                     CPUSuperclass;
  typedef SmartPointer< Self >                                                   Pointer;
  typedef SmartPointer< const Self >                                             ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GPUBoxImageFilter, GPUImageToImageFilter);

  /** Image related typedefs. */
  typedef TInputImage                        InputImageType;
  typedef typename CPUSuperclass::RegionType RegionType;
  typedef typename CPUSuperclass::SizeType   SizeType;
  typedef typename CPUSuperclass::IndexType  IndexType;
  typedef typename CPUSuperclass::OffsetType OffsetType;
  typedef typename TInputImage::PixelType    InputPixelType;

  typedef TOutputImage                        OutputImageType;
  typedef typename OutputImageType::PixelType OutputPixelType;

  /** n-dimensional Kernel radius. */
  typedef typename CPUSuperclass::SizeType       RadiusType;
  typedef typename InputImageType::SizeValueType RadiusValueType;

protected:
  GPUBoxImageFilter() {
  }
  ~GPUBoxImageFilter() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    GPUSuperclass::PrintSelf(os, indent);
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GPUBoxImageFilter);

};
}

#endif
