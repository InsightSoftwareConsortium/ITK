/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSeriesReader.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageSeriesReader_h
#define __itkImageSeriesReader_h

#include "itkImageSource.h"
#include "itkImageIOBase.h"
#include "itkExceptionObject.h"
#include "itkSize.h"
#include "itkImageRegion.h"
#include <vector>
#include <string>

namespace itk
{

/** \brief Data source that reads image data from a series of disk files.
 *
 * This class builds an n-dimension image from multiple n-1
 * dimension image files. The files stored in a vector of strings
 * are read using the ImageFileReader. File format may vary between
 * the files, but the image data must have the same Size for all
 * dimensions. 
 *
 * \sa DICOMSeriesFileNames
 * \sa NumericSeriesFileNames
 * \ingroup IOFilters
 *
 */

template <class TOutputImage>
class ITK_EXPORT ImageSeriesReader : public ImageSource<TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageSeriesReader          Self;
  typedef ImageSource<TOutputImage>  Superclass;
  typedef SmartPointer<Self>         Pointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageSeriesReader, ImageSource);

  /** The size of the output image. */
  typedef typename TOutputImage::SizeType  SizeType;

  /** The region of the output image. */
  typedef typename TOutputImage::RegionType  ImageRegionType;

  /** The pixel type of the output image. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;
  
  /** Set the vector of strings that contains the file names. Files
   * are processed in sequential order. */
  void SetFileNames (const std::vector<std::string> &name)
  {
    if ( m_FileNames != name)
      {
      m_FileNames = name;
      this->Modified();        
      }
  };
  const std::vector<std::string> & GetFileNames()
  {
    return m_FileNames;
  }

  /** Set the first file name to be processed. This deletes previous
   * filenames. */
  void SetFileName (std::string &name)
  {
    m_FileNames.clear();
    m_FileNames.push_back(name);
  }

  /** Add a single filename to the list of files. To add a vector of
   * filenames, use the AddFileNames method. */
  void AddFileName (std::string &name)
  {
    m_FileNames.push_back(name);
  }

  /** ReverseOrderOn changes the order of travesal of the file names
   * from last to first */
  itkSetMacro(ReverseOrder,bool);
  itkGetMacro(ReverseOrder,bool);
  itkBooleanMacro(ReverseOrder);

  /** Set/Get the ImageIO helper class. By default, the
   * ImageSeriesReader uses the factory mechanism of the
   * ImageFileReader to determine the file type. This method can be
   * used to specify which IO to use. */
  itkSetObjectMacro(ImageIO,ImageIOBase);
  itkGetObjectMacro(ImageIO,ImageIOBase);

  /** Prepare the allocation of the output image during the first back
   * propagation of the pipeline. */
  virtual void GenerateOutputInformation(void);

  /** Give the reader a chance to indicate that it will produce more
   * output than it was requested to produce. ImageSeriesReader cannot
   * currently read a portion of an image (since the ImageIO objects
   * cannot read a portion of an image), so the ImageSeriesReader must
   * enlarge the RequestedRegion to the size of the image on disk. */
  virtual void EnlargeOutputRequestedRegion(DataObject *output);
  
protected:
  ImageSeriesReader() : m_ImageIO(0), m_ReverseOrder(false) {};
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** Does the real work. */
  virtual void GenerateData();

  /** The image format, 0 will use the factory mechnism. */
  ImageIOBase::Pointer m_ImageIO;

  /** Select the traversal order. */
  bool m_ReverseOrder;

  /** A list of filenames to be processed. */
  std::vector<std::string> m_FileNames;

private:
  ImageSeriesReader(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

} //namespace ITK

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSeriesReader.txx"
#endif

#endif // __itkImageSeriesReader_h

