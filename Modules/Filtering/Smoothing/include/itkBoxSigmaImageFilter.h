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
#ifndef itkBoxSigmaImageFilter_h
#define itkBoxSigmaImageFilter_h

#include "itkBoxImageFilter.h"

namespace itk
{
/**
 * \class BoxSigmaImageFilter
 * \brief Implements a fast rectangular sigma filter using the
 * accumulator approach
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://hdl.handle.net/1926/555
 * http://www.insight-journal.org/browse/publication/160
 *
 * \author Gaetan Lehmann
 * \ingroup ITKSmoothing
 */

template< typename TInputImage, typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT BoxSigmaImageFilter:
  public BoxImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef BoxSigmaImageFilter                         Self;
  typedef BoxImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(BoxSigmaImageFilter, BoxImageFilter);

  /** Image related typedefs. */
  typedef TInputImage                                InputImageType;
  typedef TOutputImage                               OutputImageType;
  typedef typename TInputImage::RegionType           RegionType;
  typedef typename TInputImage::SizeType             SizeType;
  typedef typename TInputImage::IndexType            IndexType;
  typedef typename TInputImage::PixelType            PixelType;
  typedef typename TInputImage::OffsetType           OffsetType;
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename TOutputImage::PixelType           OutputPixelType;

  /** Image related typedefs. */
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimension,
                   ( Concept::SameDimension< itkGetStaticConstMacro(InputImageDimension),
                                             itkGetStaticConstMacro(OutputImageDimension) > ) );

  // End concept checking
#endif

protected:
  BoxSigmaImageFilter();
  ~BoxSigmaImageFilter() override {}

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const OutputImageRegionType &
                             outputRegionForThread,
                             ThreadIdType threadId) override;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BoxSigmaImageFilter);
};                                   // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBoxSigmaImageFilter.hxx"
#endif

#endif
