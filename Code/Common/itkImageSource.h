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
 * itkImageSource is the base class for all process objects that output
 * image data.
 */
#ifndef __itkImageSource_h
#define __itkImageSource_h

#include "itkProcessObject.h"

template <class TOutputImage>
class ITK_EXPORT itkImageSource : public itkProcessObject
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef itkSmartPointer< itkImageSource<TOutputImage> > Pointer;

  /** 
   * Create the source with one output initially 
   */
  static Pointer New();

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkImageSource,itkProcessObject);

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
  itkImageSource();
  virtual ~itkImageSource() {};
  itkImageSource(const itkImageSource&) {};
  void operator=(const itkImageSource&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);
  
  // Update extent of Image is specified in pieces.  
  // Since all DataObjects should be able to set UpdateExent as pieces,
  // just copy output->UpdateExtent  all Inputs.
  void ComputeInputUpdateExtents(itkDataObject *output);
  
private:
  // Used by streaming: The extent of the output being processed
  // by the execute method. Set in the ComputeInputUpdateExtents method.
  int m_ExecutePiece;
  int m_ExecuteNumberOfPieces;
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSource.cxx"
#endif

#endif





