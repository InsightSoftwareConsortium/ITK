/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionGrowImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkRegionGrowImageFilter_h
#define _itkRegionGrowImageFilter_h

#include "itkObject.h"
#include "itkImageToImageFilter.h"

namespace itk
{

/** \class RegionGrowImageFilter
 * \brief Base class for RegionGrowImageFilter object
 * 
 * itkRegionGrowImageFilter is the base class for the RegionGrowImageFilter objects. It provides
 * the basic function definitons that are inherent to a RegionGrowImageFilter objects.
 * It is templated over the type of input and output image. 
 *
 * This object defines the interface for those algorithm that perform 
 * feture/object segmentation by merging regions (parts of the image) that 
 * are similar in nature based on some metric. As a result parts of the image
 * which belong to the same object gets merged and the region grows. 
 * 
 * As an example regarding using this class to implementation of advanced
 * region growing algorithm, itkRegionGrowImageFilterKLM class has been
 * derived from this class. The virtual function ApplyRegionGrowImageFilter()
 * provides the interface to the outside world to extend/enhance the scope of
 * the current algorithm or write other region growing algorithms. The
 * function MergeRegions is interface for the operation that merges two
 * regions.
 * 
 * The local variables m_RowGridSize and m_ColGridSize are used to define
 * the inital small regions that the image is fragmented (atomic regions). 
 * For an 12 x 12 input image, m_RowGridSize and  m_ColGridSize when set equal 
 * to 3 will result in 16 initial regions. The default value is set equal to 2.
 * The user can sets the number of desired regions via the m_MaxNumRegions
 * parameter and the algorithm tries to perform region merging until there
 * are only m_MaxNumRegions. If m_MaxNumRegions is more than the number of
 * initial blocks, no region merging occurs.
 *
 * These blocks are important as the labels associated with these blocks keep
 * changing during the region growing process and at the end, while generating
 * the results, each of these atomic blocks are revisited and the blocks
 * with same labels are considered to belong to the same region.
 *
 * This object supports data handling of multiband images. The object
 * accepts images in vector format, where each pixel is a vector and each 
 * element of the vector corresponds to an entry from 1 particular band of
 * a multiband dataset. The input to this object is assumed to be a multiband
 * vector image, and the output is defined by specific algorithm 
 * implementation. The second template parameter is used to generate the 
 * the output image and can be modified according the algorithm 
 * specific output type.
 *
 * We expect the user to provide the input to the routine in vector format. 
 * A single band image is treated as a vector image with a single element 
 * for every vector.
 *
 * \ingroup RegionGrowingSegmentation 
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT RegionGrowImageFilter : 
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef RegionGrowImageFilter   Self;
  typedef Object Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RegionGrowImageFilter,Object);

  /** Type definition for the input image. */
  typedef TInputImage                           InputImageType;
  typedef typename TInputImage::Pointer         InputImagePointer;
  typedef typename TInputImage::ConstPointer    InputImageConstPointer;

  /** Type definition for the input image pixel type. */
  typedef typename TInputImage::PixelType InputImagePixelType;

  /** Type definition for the output image. */
  typedef TOutputImage                          OutputImageType;
  typedef typename TOutputImage::Pointer        OutputImagePointer;

  /** Type definition for the input image pixel type. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /** Set/Get the number of regions desired. */
  itkSetMacro(MaximumNumberOfRegions, unsigned int);
  itkGetMacro(MaximumNumberOfRegions, unsigned int);

  /** Set/Get the row grid size of the initial regions in the image. */
  itkSetMacro(RowGridSize, unsigned int);
  itkGetMacro(RowGridSize, unsigned int);

  /** Set/Get the column grid size of the initial regions in the image. */
  itkSetMacro(ColGridSize, unsigned int);
  itkGetMacro(ColGridSize, unsigned int);

  /** Set/Get the column grid size of the initial regions in the image. */
  itkSetMacro(SliceGridSize, unsigned int);
  itkGetMacro(SliceGridSize, unsigned int);

  /** Define a virtual RegionGrowImageFilter function. */
  virtual void ApplyRegionGrowImageFilter(){};

  /** Merge two regions. */
  virtual void MergeRegions(){};

protected:
  RegionGrowImageFilter();
  ~RegionGrowImageFilter();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  RegionGrowImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  unsigned int    m_MaximumNumberOfRegions;
  unsigned int    m_RowGridSize;
  unsigned int    m_ColGridSize;
  unsigned int    m_SliceGridSize;

}; // class RegionGrowImageFilter

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegionGrowImageFilter.txx"
#endif



#endif









