/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSource.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkImageSource_h
#define __itkImageSource_h

#include "itkProcessObject.h"

namespace itk
{

/** \class ImageSource
 *  \brief Base class for all process objects that output image data.
 *
 * ImageSource is the base class for all process objects that output
 * image data. Specifically, this class defines the GetOutput() method
 * that returns a pointer to the output image. The class also defines
 * some internal private data memebers that are used to manage streaming
 * of data.
 */
template <class TOutputImage>
class ITK_EXPORT ImageSource : public ProcessObject
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageSource         Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ProcessObject  Superclass;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);  

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ImageSource,ProcessObject);

  /** 
   * Some typedefs.
   */
  typedef TOutputImage OutputImage;
  typedef typename OutputImage::Pointer OutputImagePointer;

  /** 
   * Get the image output of this process object. 
   */
  OutputImagePointer GetOutput();
  OutputImagePointer GetOutput(unsigned int idx);

  /** 
   * Set the image output of this process object. 
   */
  void SetOutput(OutputImage *output);

protected:
  ImageSource();
  virtual ~ImageSource() {}
  ImageSource(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);
  
  /**
   * Requested region of image is specified as structured regions.  
   * Since all DataObjects should be able to set RequestedRegion in
   * unstructured form, just copy output->RequestedRegion all inputs.
   */
  void GenerateInputRequestedRegion(DataObject *output);
  
private:
  /**
   * Used by streaming: The requested region of the output being processed
   * by the execute method. Set in the GenerateInputRequestedRegion method.
   */
  int m_GenerateDataRegion;
  int m_GenerateDataNumberOfRegions;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSource.txx"
#endif

#endif
  
