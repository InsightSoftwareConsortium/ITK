/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionGrowImageFilter.h
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
 
 * As an example regarding using this class to implementation of advanced
 * region growing algorithm, itkRegionGrowImageFilterKLM class has been derived from
 * this class. The virtual function ApplyRegionGrowImageFilter() provides the interface
 * to the outside world to extend/enhance the scope of the current algorithm
 * or write other region growing algorithms. The function MergeRegions is
 * interface for the operation that merges two regions. 
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
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT RegionGrowImageFilter : 
  public ImageToImageFilter<TInputImage,TOutputImage>
{

public:
  /**
   * Standard "Self" typedef.
   */
  typedef RegionGrowImageFilter   Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef Object Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(RegionGrowImageFilter,Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Type definition for the input image.
   */
  typedef typename TInputImage::Pointer   InputImageType;

  /**
   * Type definition for the input image pixel type.
   */
  typedef typename TInputImage::PixelType InputImagePixelType;

  /**
   * Type definition for the output image.
   */
  typedef typename TOutputImage::Pointer   OutputImageType;

  /**
   * Type definition for the input image pixel type.
   */
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /**
   * Set the number of regions desired.
   */
    itkSetMacro(MaxNumRegions, unsigned int);

  /**
   * Get the number of regions desired.
   */
  itkGetMacro(MaxNumRegions, unsigned int);

  /**
   * Set the row grid size of the initial regions in the image.
   */
  itkSetMacro(RowGridSize, unsigned int);

  /**
   * Get the row grid size of the initial regions in the image.
   */
  itkGetMacro(RowGridSize, unsigned int);

  /**
   * Set the column grid size of the initial regions in the image.
   */
  itkSetMacro(ColGridSize, unsigned int);

  /**
   * Get the column grid size of the initial regions in the image.
   */
  itkGetMacro(ColGridSize, unsigned int);

  /**
   * Define a virtual RegionGrowImageFilter function.
   */
  virtual void ApplyRegionGrowImageFilter(){};

  /**
   * Merge two regions
   */
  virtual void MergeRegions(){};

protected:
  /**
   * Constructor
   */
  RegionGrowImageFilter();

  /**
   * Destructor
   */
  ~RegionGrowImageFilter();

  /**
   * Copy constructor
   */
  RegionGrowImageFilter(const Self&) {}

  /**
   * Assignment operator
   */
  void operator=(const Self&) {}

  /**
   * Print self identity
   */      
  void PrintSelf(std::ostream& os, Indent indent);

private:
  unsigned int    m_MaxNumRegions;
  unsigned int    m_RowGridSize;
  unsigned int    m_ColGridSize;

}; // class RegionGrowImageFilter


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegionGrowImageFilter.txx"
#endif



#endif









