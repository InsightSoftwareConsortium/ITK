/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImportImageFilter.h
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
#ifndef __itkImportImageFilter_h
#define __itkImportImageFilter_h

#include "itkImageSource.h"
#include "itkImportImageContainer.h"


namespace itk
{

/** \class ImportImageFilter
 * \brief Import data from a standard C array into an itk::Image
 *
 * ImportImageFilter provides a mechanism for importing data into an itk::Image.
 * ImportImageFilter is an image source, so it behaves like any other pipeline
 * object.
 *
 * This class is templated over the pixel type and the image dimension of
 * the output image.  The filter prescribes a ImageImportContainer to be
 * be used as the pixel container for the output image.
 * 
 * \ingroup ImageFilters
 *
 */

template <typename TPixel, unsigned int VImageDimension=2>
class ITK_EXPORT ImportImageFilter:
    public ImageSource<Image<TPixel,
                             VImageDimension,
                             DefaultImageTraits< TPixel,
                                                 VImageDimension,
                                                 ImportImageContainer<unsigned long, TPixel> > > >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImportImageFilter   Self;

  /**
   * Typedef for the output image.  
   */
  typedef Image<TPixel,
                VImageDimension,
                DefaultImageTraits< TPixel,
                                    VImageDimension,
                                    ImportImageContainer<unsigned long, TPixel> > > OutputImageType;
  
  /** 
   * Index typedef support. An index is used to access pixel values.
   */
  typedef Index<VImageDimension>  IndexType;

  /** 
   * Size typedef support. A size is used to define region bounds.
   */
  typedef Size<VImageDimension>  SizeType;

  /** 
   * Region typedef support. A region is used to specify a subset of an image.
   */
  typedef ImageRegion<VImageDimension>  RegionType;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<OutputImageType>  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * typename typedef for the output image PixelType
   */
  typedef TPixel OutputImagePixelType;
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImportImageFilter,ImageSource);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  
  /**
   * Get the pointer from which the image data is imported.
   */
  TPixel *GetImportPointer();

  /**
   * Set the pointer from which the image data is imported.  "num" is
   * the number of pixels in the block of memory. If
   * "LetSourceManageMemory" is false, then the application retains
   * the responsibility of freeing the memory for this image data.  If
   * "LetSourceManageMemory" is true, then this class will free the
   * memory when this object is destroyed.
   */
  void SetImportPointer(TPixel *ptr, unsigned long num,
                        bool LetSourceManageMemory);
  

  /**
   * Set the region object that defines the size and starting index
   * for the imported image. This will serve as the LargestPossibleRegion,
   * the BufferedRegion, and the RequestedRegion.
   * \sa ImageRegion
   */
  void SetRegion(const RegionType &region)
    { if (m_Region != region) {m_Region = region; this->Modified();} };
  
  /**
   * Get the region object that defines the size and starting index
   * for the imported image. This will serve as the LargestPossibleRegion,
   * the BufferedRegion, and the RequestedRegion.
   * \sa ImageRegion
   */
  const RegionType& GetRegion() const
    { return m_Region;};
  
  /** 
   * Set the spacing (size of a pixel) of the image.
   * \sa GetSpacing()
   */
  itkSetVectorMacro(Spacing, const float, VImageDimension);

  /** 
   * Get the spacing (size of a pixel) of the image.
   * \sa SetSpacing()
   */
  itkGetVectorMacro(Spacing, const float, VImageDimension);
  
  /** 
   * Set the origin of the image.
   * \sa GetOrigin()
   */
  itkSetVectorMacro(Origin, const float, VImageDimension);

  /** 
   * Get the origin of the image.
   * \sa SetOrigin()
   */
  itkGetVectorMacro(Origin, const float, VImageDimension);


protected:
 ImportImageFilter();
  ~ImportImageFilter();
  ImportImageFilter(const ImportImageFilter &) {};
  void operator=(const ImportImageFilter&) {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * This filter does not actually "produce" any data, rather it "wraps"
   * the user supplied data into an itk::Image. 
   */
  virtual void GenerateData();

  /**
   * This is a source, so it must set the spacing, size, and largest possible
   * region for the output image that it will produce.
   * \sa ProcessObject::GenerateOutputInformation()
   */
  virtual void GenerateOutputInformation();

  /**
   * This filter can only produce the amount of data that it is given,
   * so we must override ProcessObject::EnlargeOutputRequestedRegion()
   * (The default implementation of a source produces the amount of
   * data requested.  This source, however, can only produce what it is
   * given.)
   *
   * \sa ProcessObject::EnlargeOutputRequestedRegion()
   */
  virtual void EnlargeOutputRequestedRegion(DataObject *output);

private:  
  RegionType  m_Region;
  double   m_Spacing[VImageDimension];
  double   m_Origin[VImageDimension];
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImportImageFilter.txx"
#endif

#endif




