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
#ifndef itkImageKernelOperator_h
#define itkImageKernelOperator_h

#include "itkNeighborhoodOperator.h"

#include "itkImage.h"

namespace itk
{
/**
 * \class ImageKernelOperator
 * \brief A NeighborhoodOperator whose coefficients are from an image.
 *
 * This code was contributed in the Insight Journal paper:
 *
 * "Image Kernel Convolution"
 * by Tustison N., Gee J.
 * https://hdl.handle.net/1926/1323
 * http://www.insight-journal.org/browse/publication/208
 *
 *
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \sa Neighborhood
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 */
template< typename TPixel, unsigned int VDimension = 2,
          typename TAllocator = NeighborhoodAllocator< TPixel > >
class ITK_TEMPLATE_EXPORT ImageKernelOperator :
  public NeighborhoodOperator< TPixel, VDimension, TAllocator >
{
public:
  /** Standard class typedefs. */
  typedef ImageKernelOperator                                    Self;
  typedef NeighborhoodOperator< TPixel, VDimension, TAllocator > Superclass;

  typedef Image< TPixel, VDimension >            ImageType;
  typedef typename Superclass::SizeType          SizeType;
  typedef typename Superclass::CoefficientVector CoefficientVector;

  itkTypeMacro(ImageKernelOperator, NeighborhoodOperator);

  /** Constructor. */
  ImageKernelOperator() {}

  /** Copy constructor */
  ImageKernelOperator(const Self & orig):
    Neighborhood< TPixel, VDimension, TAllocator >(orig)
  {}

  /** Assignment operator. */
  Self & operator=(const Self & orig)
  {
    Superclass::operator=(orig);
    return *this;
  }

  /** Set the image kernel. Only images with odd size in all
   * dimensions are allowed. If an image with an even size is passed
   * as an argument, an exception will be thrown. */
  void SetImageKernel(const ImageType *kernel);

  /** Get the image kernel. */
  const ImageType * GetImageKernel() const;

  /** Prints information about the object. */
  virtual void PrintSelf(std::ostream & os, Indent i) const ITK_OVERRIDE
  {
    os << i << "ImageKernelOperator { this=" << this
       << "} "  << std::endl;
    Superclass::PrintSelf( os, i.GetNextIndent() );
  }

protected:
  /** Calculates operator coefficients. */
  CoefficientVector GenerateCoefficients() ITK_OVERRIDE;

  /** Arranges coefficients spatially in the memory buffer. */
  void Fill(const CoefficientVector & coeff) ITK_OVERRIDE;

private:
  typename ImageType::ConstPointer m_ImageKernel;

};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageKernelOperator.hxx"
#endif

#endif
