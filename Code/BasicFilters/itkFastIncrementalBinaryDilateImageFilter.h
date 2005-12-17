/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastIncrementalBinaryDilateImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFastIncrementalBinaryDilateImageFilter_h
#define __itkFastIncrementalBinaryDilateImageFilter_h

#include <vector>
#include <queue>
#include "itkBinaryDilateImageFilter.h"
#include "itkImage.h"
#include "itkNumericTraits.h"
#include "itkNeighborhoodIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhood.h"
#include "itkImageBoundaryCondition.h"
#include "itkImageRegionIterator.h"
#include "itkConceptChecking.h"

namespace itk
{
/**
 * \class FastIncrementalBinaryDilateImageFilter
 * \brief Fast binary dilation
 *
 * FastIncrementalBinaryDilateImageFilter is a binary dilation
 * morphologic operation. This implementation is based on the papers:
 *
 * L.Vincent "Morphological transformations of binary images with
 * arbitrary structuring elements", and
 *
 * N.Nikopoulos et al. "An efficient algorithm for 3d binary
 * morphological transformations with 3d structuring elements 
 * for arbitrary size and shape". IEEE Transactions on Image
 * Processing. Vol. 9. No. 3. 2000. pp. 283-286.
 *
 * This filter is maintained for backward compatibility. It is now a
 * subclass of BinaryDilateImageFilter (the fast incremental binary
 * dilate algorithm is now in BinaryDilateImageFilter).
 *
 * \deprecated
 * \sa BinaryDilateImageFilter
 */
template <class TInputImage, class TOutputImage, class TKernel>
class ITK_EXPORT FastIncrementalBinaryDilateImageFilter :
    public BinaryDilateImageFilter< TInputImage, TOutputImage, TKernel >
{
public:
  /** Extract dimension from input and output image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Extract the dimension of the kernel */
  itkStaticConstMacro(KernelDimension, unsigned int,
                      TKernel::NeighborhoodDimension);
  
  /** Convenient typedefs for simplifying declarations. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef TKernel KernelType;

  /** Standard class typedefs. */
  typedef FastIncrementalBinaryDilateImageFilter Self;
  typedef BinaryDilateImageFilter< InputImageType, OutputImageType, KernelType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastIncrementalBinaryDilateImageFilter, ImageToImageFilter);

  
protected:
  FastIncrementalBinaryDilateImageFilter() {};
  virtual ~FastIncrementalBinaryDilateImageFilter(){}
  
private:
  FastIncrementalBinaryDilateImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#endif
