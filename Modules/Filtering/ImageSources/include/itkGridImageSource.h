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
#ifndef itkGridImageSource_h
#define itkGridImageSource_h

#include "itkGenerateImageSource.h"
#include "itkFixedArray.h"
#include "itkKernelFunctionBase.h"
#include "itkVectorContainer.h"

#include "vnl/vnl_vector.h"

namespace itk
{
/** \class GridImageSource
 * \brief Generate an n-dimensional image of a grid.
 *
 * GridImageSource generates an image of a grid.
 * From the abstract...
 * "Certain classes of images find disparate use amongst members of the
 * ITK community for such purposes as visualization, simulation,
 * testing, etc. Currently there exists two derived classes from the
 * ImageSource class used for generating specific images for various
 * applications, viz. RandomImageSource and GaussianImageSource. We
 * propose to add to this set with the class GridImageSource which,
 * obviously enough, produces a grid image. Such images are useful for
 * visualizing deformation when used in conjunction with the
 * WarpImageFilter, simulating magnetic resonance tagging images, or
 * creating optical illusions with which to amaze your friends."
 *
 * The output image may be of any dimension.
 *
 * \author Tustison N., Avants B., Gee J. University of Pennsylvania
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/475
 *
 * \ingroup DataSources
 * \ingroup ITKImageSources
 */
template< typename TOutputImage >
class ITK_TEMPLATE_EXPORT GridImageSource
   :public GenerateImageSource< TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef GridImageSource                     Self;
  typedef GenerateImageSource< TOutputImage > Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(GridImageSource, GenerateImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  typedef double RealType;

  /** Dimensionality of the output image */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Typedef for the output image types. */
  typedef TOutputImage                         ImageType;
  typedef typename TOutputImage::RegionType    ImageRegionType;
  typedef typename TOutputImage::PixelType     PixelType;
  typedef typename TOutputImage::SpacingType   SpacingType;
  typedef typename TOutputImage::PointType     OriginType;
  typedef typename TOutputImage::DirectionType DirectionType;
  typedef typename TOutputImage::SizeType      SizeType;

  typedef KernelFunctionBase<double>           KernelFunctionType;
  /** Other convenient types. */
  typedef FixedArray< RealType, itkGetStaticConstMacro(ImageDimension) >
  ArrayType;
  typedef FixedArray< bool, itkGetStaticConstMacro(ImageDimension) >
  BoolArrayType;
  typedef vnl_vector< RealType >                           PixelArrayType;
  typedef VectorContainer< SizeValueType, PixelArrayType > PixelArrayContainerType;

  /** Set/Get kernel function used to create the grid. */
  itkSetObjectMacro(KernelFunction, KernelFunctionType);
  itkGetConstReferenceObjectMacro(KernelFunction, KernelFunctionType);

  /** Set/Get the standard deviation of the Gaussians or width of the box
   * functions.*/
  itkSetMacro(Sigma, ArrayType);
  itkGetConstReferenceMacro(Sigma, ArrayType);

  /** Set/Get the grid spacing of the peaks. */
  itkSetMacro(GridSpacing, ArrayType);
  itkGetConstReferenceMacro(GridSpacing, ArrayType);

  /** Set/Get the grid offset. */
  itkSetMacro(GridOffset, ArrayType);
  itkGetConstReferenceMacro(GridOffset, ArrayType);

  /** Set/Get the dimensions which are gridded. */
  itkSetMacro(WhichDimensions, BoolArrayType);
  itkGetConstReferenceMacro(WhichDimensions, BoolArrayType);

  /** Set/Get the scale factor to multiply the true value of the grid. */
  itkSetMacro(Scale, RealType);
  itkGetConstReferenceMacro(Scale, RealType);

protected:
  GridImageSource();
  // ~GridImageSource(){} default implementation ok
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void
  ThreadedGenerateData(const ImageRegionType &
                       outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

  virtual void BeforeThreadedGenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GridImageSource);

  /** Internal variable to speed up the calculation of pixel values. */
  typename PixelArrayContainerType::Pointer m_PixelArrays;

  typename KernelFunctionType::Pointer m_KernelFunction;

  ArrayType m_Sigma;

  ArrayType m_GridSpacing;

  ArrayType m_GridOffset;

  BoolArrayType m_WhichDimensions;

  RealType m_Scale;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGridImageSource.hxx"
#endif

#endif
