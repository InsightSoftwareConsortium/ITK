/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveMultiResolutionPyramidImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkRecursiveMultiResolutionPyramidImageFilter_h
#define __itkRecursiveMultiResolutionPyramidImageFilter_h

#include "itkMultiResolutionPyramidImageFilter.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

/**
 * \class RecursiveMultiResolutionPyramidImageFilter
 * \brief Create multi-resolutin pyramid using a recursive implementation.
 *
 * RecursiveMultiResolutionPyramidImageFilter creates an
 * image pryamid according to a users defined 
 * multi-resolution schedule.
 *
 * If a schedule is downward divisible are fast recursive implementation
 * is used to generate the outputs. If the schedule is not downward
 * divisible the superclass MultiResolutionPyramidImageFilter implementation
 * is used instead. A schedule is downward divisible if at every level,
 * the shrink factors are divisible by the shrink factors at the
 * next level for the same dimension.
 * 
 * See documentation of MultiResolutionPyramidImageFilter
 * for information on how to specify a multi-resolution schedule.
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
 */
template <
class TInputImage, 
class TOutputImage
>
class ITK_EXPORT RecursiveMultiResolutionPyramidImageFilter : 
  public MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef RecursiveMultiResolutionPyramidImageFilter  Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef MultiResolutionPyramidImageFilter<TInputImage,TOutputImage>  Superclass;

  /**
   * SmartPointer typedef support
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory
   */
  itkNewMacro(Self);

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(RecursiveMultiResolutionPyramidImageFilter, MultiResolutionPyramidImageFilter);

  /**
   * ImageDimension enumeration
   */
  enum{ ImageDimension = Superclass::ImageDimension };

  /**
   * Inherit types from Superclass
   */
  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::InputImagePointer InputImagePointer;
  typedef typename Superclass::OutputImagePointer OutputImagePointer;

  /**
   * Given one output whose requested region has been set, set
   * the requtested region for the remaining objects.
   * The original documentation of this method is below.
   *
   * \sa ProcessObject::GenerateOutputRequestedRegion();
   */
  virtual void GenerateOutputRequestedRegion(DataObject *output);

  /**
   * MultiResolutionPyramid requires all of the input to be in the
   * buffer. As such, MultiResolutionPyramid needs to provide an
   * implemenation for GenerateInputRequestedRegion().
   * The original documenation of this method is below.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion()
   */
  virtual void GenerateInputRequestedRegion();

protected:
  RecursiveMultiResolutionPyramidImageFilter();
  ~RecursiveMultiResolutionPyramidImageFilter() {};
  RecursiveMultiResolutionPyramidImageFilter(const Self&) {};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream&os, Indent indent) const;

  void GenerateData();

};


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveMultiResolutionPyramidImageFilter.txx"
#endif

#endif


