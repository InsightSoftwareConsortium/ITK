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
#ifndef itkRecursiveMultiResolutionPyramidImageFilter_h
#define itkRecursiveMultiResolutionPyramidImageFilter_h

#include "itkMultiResolutionPyramidImageFilter.h"
#include "vnl/vnl_matrix.h"

namespace itk
{
/** \class RecursiveMultiResolutionPyramidImageFilter
 * \brief Creates a multi-resolution pyramid using a recursive implementation.
 *
 * RecursiveMultiResolutionPyramidImageFilter creates an image pryamid
 * according to a user defined multi-resolution schedule.
 *
 * If a schedule is downward divisible, a fast recursive implementation is
 * used to generate the output images. If the schedule is not downward
 * divisible the superclass (MultiResolutionPyramidImageFilter)
 * implementation is used instead. A schedule is downward divisible if at
 * every level, the shrink factors are divisible by the shrink factors at the
 * next level for the same dimension.
 *
 * See documentation of MultiResolutionPyramidImageFilter
 * for information on how to specify a multi-resolution schedule.
 *
 * Note that unlike the MultiResolutionPyramidImageFilter,
 * RecursiveMultiResolutionPyramidImageFilter will not smooth the output at
 * the finest level if the shrink factors are all one and the schedule
 * is downward divisible.
 *
 * This class is templated over the input image type and the output image type.
 *
 * This filter uses multithreaded filters to perform the smoothing and
 * downsampling.
 *
 * This filter supports streaming.
 *
 * \sa MultiResolutionPyramidImageFilter
 *
 * \ingroup PyramidImageFilter MultiThreaded Streamed
 * \ingroup ITKRegistrationCommon
 *
 * \wiki
 * \wikiexample{ImageProcessing/RecursiveMultiResolutionPyramidImageFilter,Construct a multiresolution pyramid from an image}
 * \endwiki
 */
template<
  typename TInputImage,
  typename TOutputImage
  >
class ITK_TEMPLATE_EXPORT RecursiveMultiResolutionPyramidImageFilter:
  public MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef RecursiveMultiResolutionPyramidImageFilter Self;
  typedef MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RecursiveMultiResolutionPyramidImageFilter,
               MultiResolutionPyramidImageFilter);

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Inherit types from the superclass.. */
  typedef typename Superclass::InputImageType         InputImageType;
  typedef typename Superclass::OutputImageType        OutputImageType;
  typedef typename Superclass::InputImagePointer      InputImagePointer;
  typedef typename Superclass::OutputImagePointer     OutputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;

  /** Given one output whose requested region has been set,
   * this method sets the requtested region for the remaining
   * output images.
   * The original documentation of this method is below.
   * \sa ProcessObject::GenerateOutputRequestedRegion(); */
  virtual void GenerateOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

  /** RecursiveMultiResolutionPyramidImageFilter requires a larger input
   * requested region than the output requested regions to accommodate the
   * shrinkage and smoothing operations.  As such,
   * MultiResolutionPyramidImageFilter needs to provide an implementation for
   * GenerateInputRequestedRegion().  The original documentation of this
   * method is below.  \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

protected:
  RecursiveMultiResolutionPyramidImageFilter();
  ~RecursiveMultiResolutionPyramidImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate the output data. */
  void GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RecursiveMultiResolutionPyramidImageFilter);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveMultiResolutionPyramidImageFilter.hxx"
#endif

#endif
