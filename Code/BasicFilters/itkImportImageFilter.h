/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImportImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImportImageFilter_h
#define __itkImportImageFilter_h

#include "itkImageSource.h"
#include "itkImage.h"

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
 * the output image.
 * 
 * \ingroup IOFilters
 */
template <typename TPixel, unsigned int VImageDimension=2>
class ITK_EXPORT ImportImageFilter: 
    public ImageSource< Image<TPixel,VImageDimension> >
{
public:
  /** Typedef for the output image.   */
  typedef Image<TPixel,VImageDimension> OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  
  /** Standard class typedefs. */
  typedef ImportImageFilter   Self;
  typedef ImageSource<OutputImageType>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImportImageFilter,ImageSource);

  /** Index typedef support. An index is used to access pixel values. */
  typedef Index<VImageDimension>  IndexType;

  /** Size typedef support. A size is used to define region bounds. */
  typedef Size<VImageDimension>  SizeType;

  /** Region typedef support. A region is used to specify a 
   * subset of an image. */
  typedef ImageRegion<VImageDimension>  RegionType;

  /** Type of the output image pixel type. */
  typedef TPixel OutputImagePixelType;
  
  /** Get the pointer from which the image data is imported. */
  TPixel *GetImportPointer();

  /** Set the pointer from which the image data is imported.  "num" is
   * the number of pixels in the block of memory. If
   * "ImportImageFilterWillOwnTheMemory" is false, then the this filter will
   * not free the memory in its destructor and the application providing the
   * buffer retains the responsibility of freeing the memory for this image
   * data.  If "ImportImageFilterWillOwnTheMemory" is true, then this class
   * will free the memory when this object is destroyed. */
  void SetImportPointer(TPixel *ptr, unsigned long num,
                        bool ImportImageFilterWillOwnTheMemory);

  /** Set the region object that defines the size and starting index
   * for the imported image. This will serve as the LargestPossibleRegion,
   * the BufferedRegion, and the RequestedRegion.
   * \sa ImageRegion */
  void SetRegion(const RegionType &region)
  { if (m_Region != region) {m_Region = region; this->Modified();} };
  
  /** Get the region object that defines the size and starting index
   * for the imported image. This will serve as the LargestPossibleRegion,
   * the BufferedRegion, and the RequestedRegion.
   * \sa ImageRegion */
  const RegionType& GetRegion() const
  { return m_Region;};
  
  /** Set the spacing (size of a pixel) of the image.
   * \sa GetSpacing() */
  itkSetVectorMacro(Spacing, const double, VImageDimension);
  itkSetVectorMacro(Spacing, const float, VImageDimension);

  /** Get the spacing (size of a pixel) of the image.
   * \sa SetSpacing() */
  itkGetVectorMacro(Spacing, const double, VImageDimension);
  
  /** Set the origin of the image.
   * \sa GetOrigin() */
  itkSetVectorMacro(Origin, const double, VImageDimension);
  itkSetVectorMacro(Origin, const float, VImageDimension);

  /** Get the origin of the image.
   * \sa SetOrigin() */
  itkGetVectorMacro(Origin, const double, VImageDimension);

protected:
  ImportImageFilter();
  ~ImportImageFilter();
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** This filter does not actually "produce" any data, rather it "wraps"
   * the user supplied data into an itk::Image.  */
  virtual void GenerateData();

  /** This is a source, so it must set the spacing, size, and largest possible
   * region for the output image that it will produce.
   * \sa ProcessObject::GenerateOutputInformation() */
  virtual void GenerateOutputInformation();

  /** This filter can only produce the amount of data that it is given,
   * so we must override ProcessObject::EnlargeOutputRequestedRegion()
   * (The default implementation of a source produces the amount of
   * data requested.  This source, however, can only produce what it is
   * given.)
   *
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  virtual void EnlargeOutputRequestedRegion(DataObject *output);

private:  
  ImportImageFilter(const ImportImageFilter &); //purposely not implemented
  void operator=(const ImportImageFilter&); //purposely not implemented

  RegionType  m_Region;
  double   m_Spacing[VImageDimension];
  double   m_Origin[VImageDimension];

  TPixel*  m_ImportPointer;
  bool     m_ThisImportImageFilterWillOwnTheMemory;
  unsigned long m_Size;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImportImageFilter.txx"
#endif

#endif




