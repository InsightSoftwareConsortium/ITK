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
/**
 * ImageSource is the base class for all process objects that output
 * image data.
 */
#ifndef __itkImageSource_h
#define __itkImageSource_h

#include "itkProcessObject.h"

namespace itk
{

template <class TOutputImage>
class ITK_EXPORT ImageSource : public ProcessObject
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef ImageSource         Self;
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
   * Get the image output of this process object. 
   */
  TOutputImage *GetOutput();
  TOutputImage *GetOutput(unsigned int idx);

  /** 
   * Set the image output of this process object. 
   */
  void SetOutput(TOutputImage *output);

protected:
  ImageSource();
  virtual ~ImageSource() {}
  ImageSource(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);
  
  /**
   * Update extent of Image is specified in pieces.  
   * Since all DataObjects should be able to set UpdateExent as pieces,
   * just copy output->UpdateExtent  all Inputs.
   */
  void ComputeInputUpdateExtents(DataObject *output);
  
private:
  /**
   * Used by streaming: The extent of the output being processed
   * by the execute method. Set in the ComputeInputUpdateExtents method.
   */
  int m_ExecutePiece;
  int m_ExecuteNumberOfPieces;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSource.txx"
#endif

#endif
  
