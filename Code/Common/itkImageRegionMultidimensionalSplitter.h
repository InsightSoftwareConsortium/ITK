/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionMultidimensionalSplitter.h
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
#ifndef __itkImageRegionMultidimensionalSplitter_h
#define __itkImageRegionMultidimensionalSplitter_h

#include "itkObject.h"
#include "itkRegion.h"
#include "itkObjectFactory.h"
#include "itkIndex.h"
#include "itkSize.h"
#include "itkImageRegionSplitter.h"

namespace itk
{

/** \class ImageRegionMultidimensionalSplitter
 * \brief Divide a region into several pieces.
 *
 * ImageRegionMultidimensionalSplitter divides an ImageRegion into
 * smaller regions.  ImageRegionMultidimensionalSplitter is used by
 * the StreamingImageFilter to divide a requested output region into a
 * series of smaller requests of the pipeline.  This object has two
 * basic methods: GetNumberOfSplits() and GetSplit().
 * 
 * GetNumberOfSplits() is used to determine how may subregions a given
 * region can be divided.  You call GetNumberOfSplits with an argument
 * that is the number of subregions you want.  If the image region can
 * support that number of subregions, that number is returned.
 * Otherwise, the maximum number of splits a region can support will
 * be returned.  For example, if a region splitter class only divides
 * a region into horizontal slabs, then the maximum number of splits
 * will be the number of rows in the region.
 *
 * GetSplit() returns the ith of N subregions (as an ImageRegion object).
 *
 * ImageRegionMultidimensionalSplitter class divides a region
 * into hypercubes (as opposed to the slabs used by the superclass
 * ImageRegionSplitter). In other words, it splits every dimension of the
 * region to form ND rectangular prisms.
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataProcessing
 */

template <unsigned int VImageDimension>
class ITK_EXPORT ImageRegionMultidimensionalSplitter: public ImageRegionSplitter<VImageDimension>
{
public:
  /** 
   * Standard "Self" typedef.
   */
  typedef ImageRegionMultidimensionalSplitter              Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageRegionSplitter<VImageDimension>  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageRegionMultidimensionalSplitter,ImageRegionSplitter);

  /**
   * Dimension of the image available at compile time.
   */
  enum { ImageDimension = VImageDimension };
  
  /** 
   * Index typedef support. An index is used to access pixel values.
   */
  typedef Index<VImageDimension>  IndexType;

  /** 
   * Size typedef support. A size is used to define region bounds.
   */
  typedef Size<VImageDimension>  SizeType;

  /**
   * Region typedef support.  
   */
  typedef ImageRegion<VImageDimension> RegionType;

  /**
   * How many pieces can the specifed region be split? A given region
   * cannot always be divided into the requested number of pieces.  For
   * instance, if the numberOfPieces exceeds the number of pixels along
   * a certain dimensions, then some splits will not be possible. This
   * method returns a number less than or equal to the requested number
   * of pieces. 
   */
  virtual unsigned int GetNumberOfSplits(const RegionType &region,
                                         unsigned int requestedNumber);

  /**
   * Get a region definition that represents the ith piece a specified region.
   * The "numberOfPieces" specified should be less than or equal to what
   * GetNumberOfSplits() returns.
   */
  virtual RegionType GetSplit(unsigned int i, unsigned int numberOfPieces,
                              const RegionType &region);

protected:
  ImageRegionMultidimensionalSplitter() {};
  ~ImageRegionMultidimensionalSplitter() {};
  ImageRegionMultidimensionalSplitter(const ImageRegionMultidimensionalSplitter&) {};
  void operator=(const ImageRegionMultidimensionalSplitter&) {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegionMultidimensionalSplitter.txx"
#endif

#endif

