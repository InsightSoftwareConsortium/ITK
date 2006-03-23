/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryPruningImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryPruningImageFilter_h
#define __itkBinaryPruningImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkImageRegionIteratorWithIndex.h>
#include <itkNeighborhoodIterator.h>

namespace itk
{

/** \class BinaryPruningImageFilter
*
* This class is parametrized over the type of the input image
* and the type of the output image.
*
* This filter remove spurs pixel of less than a certain length input image.
* 
*
* The input is assumed to be a binary image.

*
* This filter is sequential pruning algorithm and known to be computational time
* dependable of the image size.  The algorithm is the N-dimensional version
* of the  given for two dimensions in:
* 
* Rafael C. Gonzales and Richard E. Woods. 
* Digital Image Processing. 
* Addison Wesley, 491-494, (1993).
*
* \sa MorphologyImageFilter
* \sa BinaryPruningImageFilter
* \sa BinaryErodeImageFilter
* \sa BinaryDilateImageFilter
* \sa BinaryThinningImageFilter
* \ingroup ImageEnhancement MathematicalMorphologyImageFilters
*/

template <class TInputImage,class TOutputImage>
class ITK_EXPORT BinaryPruningImageFilter :
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef BinaryPruningImageFilter    Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( BinaryPruningImageFilter, ImageToImageFilter );

  /** Type for input image. */
  typedef   TInputImage       InputImageType;

  /** Type for output image: Skelenton of the object.  */
  typedef   TOutputImage      OutputImageType;

  /** Type for the region of the input image. */
  typedef typename InputImageType::RegionType   RegionType;

  /** Type for the index of the input image. */
  typedef typename RegionType::IndexType  IndexType;

  /** Type for the index of the input image. */
  typedef typename InputImageType::PixelType PixelType ;

  /** Type for the size of the input image. */
  typedef typename RegionType::SizeType   SizeType;

  /** Pointer Type for input image. */
  typedef typename InputImageType::ConstPointer InputImagePointer;

  /** Pointer Type for the output image. */
  typedef typename OutputImageType::Pointer OutputImagePointer;
  
  /** Neighborhood iterator type */
  typedef NeighborhoodIterator<TInputImage> NeighborhoodIteratorType ;
  
  /** Get Skelenton by thinning image. */
  OutputImageType * GetPruning(void);

  /** Set/Get the iteration value */
  itkSetMacro(Iteration, unsigned int);
  itkGetMacro(Iteration, unsigned int);

  /** ImageDimension enumeration   */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension );
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension );

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Start concept checking */
  itkConceptMacro(SameDimensionCheck,
    (Concept::SameDimension<InputImageDimension, OutputImageDimension>));
  itkConceptMacro(SameTypeCheck,
    (Concept::SameType<PixelType, typename TOutputImage::PixelType>));
  itkConceptMacro(AdditiveOperatorsCheck,
    (Concept::AdditiveOperators<PixelType>));
  itkConceptMacro(IntConvertibleToPixelTypeCheck,
    (Concept::Convertible<int, PixelType>));
  /** End concept checking */
#endif

protected:
  BinaryPruningImageFilter();
  virtual ~BinaryPruningImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Compute thinning Image. */
  void GenerateData();

  /** Prepare data. */
  void PrepareData();

  /**  Compute thinning Image. */
  void ComputePruneImage();


private:   
  BinaryPruningImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  unsigned int                  m_Iteration;

}; // end of BinaryThinningImageFilter class

} //end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryPruningImageFilter.txx"
#endif

#endif
