/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtractOrthogonalSwath2DImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkExtractOrthogonalSwath2DImageFilter_h
#define __itkExtractOrthogonalSwath2DImageFilter_h

#include "itkImageAndPathToImageFilter.h"
#include "itkParametricPath.h"

namespace itk
{
  
/** \class ExtractOrthogonalSwath2DImageFilter
 * \brief Extracts a rectangular "swath" image from the input image along the path.
 *
 * Extracts a rectangular "swath" image from the input image by interpolating
 * image pixels orthogonal to the parametric path while walking along the
 * path.  The input and output images must be of the same type.
 * 
 * \ingroup   ImageFilters
 * \ingroup   PathFilters
 */
template <class TImage>
class ITK_EXPORT ExtractOrthogonalSwath2DImageFilter : public
ImageAndPathToImageFilter<TImage,ParametricPath<2>,TImage> 
{
public:
  /** Standard class typedefs. */
  typedef ExtractOrthogonalSwath2DImageFilter  Self;
  typedef ImageAndPathToImageFilter<TImage,ParametricPath<2>,TImage>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ExtractOrthogonalSwath2DImageFilter, ImageAndPathToImageFilter);

  /** Some convenient typedefs. */
  typedef           TImage                        ImageType;
  typedef typename  ImageType::Pointer            ImagePointer;
  typedef typename  ImageType::ConstPointer       ImageConstPointer;
  typedef typename  ImageType::RegionType         ImageRegionType; 
  typedef typename  ImageType::PixelType          ImagePixelType;
  typedef           ParametricPath<2>             PathType;
  typedef typename  PathType::ConstPointer        PathConstPointer;
  typedef typename  PathType::InputType           PathInputType;
  typedef typename  PathType::OutputType          PathOutputType;
  typedef typename  PathType::IndexType           PathIndexType;
  typedef typename  PathType::OffsetType          PathOffsetType;


protected:
  ExtractOrthogonalSwath2DImageFilter() {};
  virtual ~ExtractOrthogonalSwath2DImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData(void);

private:
  ExtractOrthogonalSwath2DImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExtractOrthogonalSwath2DImageFilter.txx"
#endif

#endif
