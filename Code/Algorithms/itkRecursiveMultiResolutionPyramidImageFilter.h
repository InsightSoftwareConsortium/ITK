/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRecursiveMultiResolutionPyramidImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkRecursiveMultiResolutionPyramidImageFilter_h
#define __itkRecursiveMultiResolutionPyramidImageFilter_h

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
 * This class is templated over the input image type and the output image type.
 *
 * This filter uses multithreaded filters to perform the smoothing and
 * downsampling.
 *
 * This filter supports streaming.
 *
 * \sa MultiResolutionPyramidImageFilter
 *
 * \ingroup PyramidImageFilter Multithreaded Streamed 
 */
template <
  class TInputImage, 
  class TOutputImage
  >
class ITK_EXPORT RecursiveMultiResolutionPyramidImageFilter : 
    public MultiResolutionPyramidImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef RecursiveMultiResolutionPyramidImageFilter  Self;
  typedef MultiResolutionPyramidImageFilter<TInputImage,TOutputImage>  
  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RecursiveMultiResolutionPyramidImageFilter, 
               MultiResolutionPyramidImageFilter);

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Inherit types from the superclass.. */
  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::InputImagePointer InputImagePointer;
  typedef typename Superclass::OutputImagePointer OutputImagePointer;
  typedef typename Superclass::InputImageConstPointer InputImageConstPointer;

  /** Given one output whose requested region has been set, 
   * this method sets the requtested region for the remaining
   * output images.
   * The original documentation of this method is below.
   * \sa ProcessObject::GenerateOutputRequestedRegion(); */
  virtual void GenerateOutputRequestedRegion(DataObject *output);

  /** RecursiveMultiResolutionPyramidImageFilter requires a larger input
   * requested region than the output requested regions to accomdate the
   * shrinkage and smoothing operations.  As such,
   * MultiResolutionPyramidImageFilter needs to provide an implementation for
   * GenerateInputRequestedRegion().  The original documentation of this
   * method is below.  \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

protected:
  RecursiveMultiResolutionPyramidImageFilter();
  ~RecursiveMultiResolutionPyramidImageFilter() {};
  void PrintSelf(std::ostream&os, Indent indent) const;

  /** Generate the output data. */
  void GenerateData();

private:
  RecursiveMultiResolutionPyramidImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRecursiveMultiResolutionPyramidImageFilter.txx"
#endif

#endif


