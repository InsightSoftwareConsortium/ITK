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
///superclass of all producing images as output
/**
 * itkImageSource is the base class for all process objects that output
 * image data.
 */
#ifndef __itkImageSource_h
#define __itkImageSource_h

#include "itkDataObject.h"
#include "itkImage.h"

template < class vtkImage<T> >
class ITK_EXPORT itkImageSource : public itkProcessObject
{
public:
  /** Smart pointer typedef support */
  typedef itkSmartPointer< itkImageSource<T> > Pointer;

  /** Create the source with one output initially */
  static Pointer New();

  /** Get the output of this process object. */
  itkImage<T> *GetOutput();
  itkImage<T> *GetOutput(int idx)
    {return (itkImage<T> *) this->itkProcessObject::GetOutput(idx); };
  void SetOutput(itkImage<T> *output);

protected:
  itkImageSource();
  ~itkImageSource() {};
  itkImageSource(const itkImageSource&) {};
  void operator=(const itkImageSource&) {};
  
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

#endif





