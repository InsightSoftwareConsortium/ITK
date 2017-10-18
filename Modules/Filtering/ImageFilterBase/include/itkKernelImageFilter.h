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
#ifndef itkKernelImageFilter_h
#define itkKernelImageFilter_h

#include "itkBoxImageFilter.h"

namespace itk
{

template< unsigned int VDimension > class FlatStructuringElement;
/**
 * \class KernelImageFilter
 * \brief A base class for all the filters working on an arbitrary shaped neighborhood
 *
 * This filter provides the code to store the radius information about the
 * neighborhood used in the subclasses.
 *
 * \author Gaetan Lehmann
 * \ingroup ITKImageFilterBase
 */

template< typename TInputImage, typename TOutputImage, typename TKernel /*=Neighborhood<bool,
                                                                 TInputImage::ImageDimension>*/                     >
class ITK_TEMPLATE_EXPORT KernelImageFilter:
  public BoxImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef KernelImageFilter                           Self;
  typedef BoxImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(KernelImageFilter,
               BoxImageFilter);

  /** Image related typedefs. */
  typedef TInputImage                      InputImageType;
  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::SizeType   SizeType;
  typedef typename TInputImage::IndexType  IndexType;
  typedef typename TInputImage::OffsetType OffsetType;

  typedef typename TInputImage::PixelType InputPixelType;

  typedef TOutputImage                     OutputImageType;
  typedef typename TOutputImage::PixelType OutputPixelType;

  typedef TKernel KernelType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  /** Kernel type used to create box kernel, in SetRadius() method */
  typedef FlatStructuringElement< itkGetStaticConstMacro(ImageDimension) >
  FlatKernelType;
  /** n-dimensional Kernel radius. */
  typedef typename TInputImage::SizeType RadiusType;

  /** Set kernel (structuring element). */
  virtual void SetKernel(const KernelType & kernel);

  itkGetConstReferenceMacro(Kernel, KernelType);

  /** Set the kernel to a box kernel of given radius. */
  virtual void SetRadius(const RadiusType & radius) ITK_OVERRIDE;

  virtual void SetRadius(const SizeValueType & radius) ITK_OVERRIDE
  {
    // needed because of the overloading of the method
    Superclass::SetRadius(radius);
  }

protected:
  KernelImageFilter();
  ~KernelImageFilter() ITK_OVERRIDE {}

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** kernel or structuring element to use. */
  KernelType m_Kernel;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(KernelImageFilter);

  template<typename T> void MakeKernel( const RadiusType & radius, T & kernel )
  {
    kernel.SetRadius( radius );
    for( typename T::Iterator kit=kernel.Begin(); kit != kernel.End(); kit++ )
      {
      *kit = 1;
      }
  }

  void MakeKernel( const RadiusType & radius, FlatKernelType & kernel )
  {
    // set up a decomposable box structuring element which is
    // much efficient with van Herk / Gil Werman filters
    kernel = FlatKernelType::Box( radius );
    assert( kernel.GetDecomposable() );
  }
};
}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKernelImageFilter.hxx"
#endif

#endif
