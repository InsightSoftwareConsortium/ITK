/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrthogonalSwath2DPathFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkOrthogonalSwath2DPathFilter_h
#define _itkOrthogonalSwath2DPathFilter_h

#include "itkPathAndImageToPathFilter.h"

namespace itk
{
  
/** \class OrthogonalSwath2DPathFilter
 * \brief Filter that optimizes a 2D path relative to an image.
 *
 * OrthogonalSwath2DPathFilter produces a chain code representation of a path
 * that is optimal with respect to an image and an original Fourier series path
 * (sometimes referred to as an "initial contour").  A rectangular "swath" image
 * is extracted from the input image by interpolating image pixels orthogonal to
 * the initial contour while walking along the initial contour.  The swath image
 * is then processed by a filter of the user's choosing before dynamic
 * programming is used to find the "optimal" path through the image, where
 * "optimal" is defined by a user-specified "index metrit" function (not to be
 * confused with registration metrics):
 *
 * merit of including an index in a path = f(processed swath image, the index)
 * 

2D Swath will use merit of location and direction of current step in input path

 * \ingroup PathFilters
 */
template <class TParametricPath, class TImage>
class ITK_EXPORT OrthogonalSwath2DPathFilter : public
PathAndImageToPathFilter< TParametricPath, TImage,
                          OrthogonallyCorrected2DParametricPath >
{
public:
  /** Standard class typedefs. */
  typedef OrthogonalSwath2DPathFilter                         Self;
  typedef PathAndImageToPathFilter< TParametricPath, TImage,
                      OrthogonallyCorrected2DParametricPath > Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(OrthogonalSwath2DPathFilter, PathAndImageToPathFilter);

  /** Some convenient typedefs. */
  typedef TParametricPath                       InputPathType;
  typedef typename InputPathType::Pointer       InputPathPointer;
  typedef typename InputPathType::InputType     InputPathInputType;
  typedef OrthogonallyCorrected2DParametricPath OutputPathType;
  typedef typename OutputPathType::Pointer      OutputPathPointer;
  typedef typename OutputPathType::InputType    OutputPathInputType;
  typedef typename InputPathType::IndexType     IndexType;
  typedef typename InputPathType::OffsetType    OffsetType;
  
protected:
  OrthogonalSwath2DPathFilter();
  virtual ~OrthogonalSwath2DPathFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData(void);

private:
  OrthogonalSwath2DPathFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOrthogonalSwath2DPathFilter.txx"
#endif

#endif
