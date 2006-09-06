/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPyImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPyImageFilter_h
#define _itkPyImageFilter_h

#include "itkImageToImageFilter.h"

// The python header defines _POSIX_C_SOURCE without a preceding #undef
#undef _POSIX_C_SOURCE
#include <Python.h>

namespace itk
{

/** \Class PyImageFilter
 *  \brief Command subclass that calls a Python callable object, e.g.
 *  a Python function.
 * 
 * With this class, arbitrary Python callable objects (e.g. functions)
 * can be associated with an instance to be used in AddObserver calls.
 * This is analogous to itk::TclCommand, but then a tad more flexible. ;)
 *
 * This class was contributed by Charl P. Botha <cpbotha |AT| ieee.org>
 */


template <class TInputImage, class TOutputImage>
class ITK_EXPORT PyImageFilter : public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef PyImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PyImageFilter, ImageToImageFilter);

  /** Some convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef typename    InputImageType::Pointer    InputImagePointer;
  typedef typename    InputImageType::RegionType InputImageRegionType;
  typedef typename    InputImageType::PixelType  InputImagePixelType;
  typedef TOutputImage OutputImageType;
  typedef typename     OutputImageType::Pointer    OutputImagePointer;
  typedef typename     OutputImageType::RegionType OutputImageRegionType;
  typedef typename     OutputImageType::PixelType  OutputImagePixelType;

  /** ImageDimension enumeration */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);


  void SetPyGenerateData(PyObject *obj);

protected:
  PyImageFilter();
  virtual ~PyImageFilter();
  virtual void GenerateData();

private:
  PyImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  PyObject *obj;

};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPyImageFilter.txx"
#endif

#endif // _itkPyImageFilter_h

