/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageToImage.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFilterImageToImage_h
#define __itkFilterImageToImage_h

#include "itkImageSource.h"

namespace itk
{

/** \class FilterImageToImage
 * \brief 
 *
 * FilterImageToImage is the base class for all process objects that output
 * image data and require image data as input. Specifically, this class
 * defines the SetInput() method for defining the input to a filter.
 *
 * This class provides the infrastructure for supporting multithreaded
 * processing of images.  If a filter provides an implementation of
 * GenerateData(), the image processing will run in a single thread and the
 * implementation is responsible for allocating its output data.  If a filter
 * provides an implementation of ThreadedGenerateData() instead, the image
 * will be divided into a number of pieces, a number of threads will be
 * spawned, and ThreadedGenerateData() will be called in each thread.  Here,
 * the output memory will be allocated by this superclass prior to calling
 * ThreadedGenerateData().
 *
 * FilterImageToImage provides an implementation of
 * GenerateInputRequestedRegion().  The base assumption to this point in the
 * heirarchy is that a process object would ask for the largest possible
 * region on input in order to produce any output.  Imaging filters,
 * however, can usually answer this question more precisely.  The default
 * implementation of GenerateInputRequestedRegion() in this class is to
 * request an input that matches the size of the requested output.  If a
 * filter requires more input (say a filter that uses neighborhood
 * information) or less input (for instance a magnify filter), then these
 * filters will have to provide another implmentation of this method. By
 * convention, such implementations should call the Superclass' method
 * first.
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT FilterImageToImage : public ImageSource<TOutputImage>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef FilterImageToImage  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef ImageSource<TOutputImage>  Superclass;

  /** 
   * Smart pointer typedef support 
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FilterImageToImage,ImageSource);

  /** 
   * Some typedefs.
   */
  typedef TInputImage InputImage;
  typedef typename InputImage::Pointer InputImagePointer;
  typedef typename InputImage::Region InputImageRegion; 
  typedef typename InputImage::PixelType InputImagePixelType; 

  /** 
   * Set the image input of this process object. 
   */
  void SetInput(InputImage *input);

  /** 
   * Get the image input of this process object. 
   */
  InputImagePointer GetInput();
  InputImagePointer GetInput(unsigned int idx);

protected:
  FilterImageToImage();
  ~FilterImageToImage();
  FilterImageToImage(const FilterImageToImage&) {};
  void operator=(const FilterImageToImage&) {};
  void PrintSelf(std::ostream& os, Indent indent);

  /** 
   * What is the input requested region that is required to produce the
   * output requested region? The base assumption for image processing
   * filters is that the input requested region can be set to match the
   * output requested region.  If a filter requires more input (for instance
   * a filter that uses neighborhoods needs more input than output to avoid
   * introducing artificial boundary conditions) or less input (for instance 
   * a magnify filter) will have to override this method.  In doing so, it
   * should call its superclass' implementation as its first step. Note that
   * this imaging filters operate differently than the classes to this
   * point in the class hierachy.  Up till now, the base assumption has been
   * that the largest possible region will be requested of the input.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion(),
   *     ImageSource::GenerateInputRequestedRegion()
   */
  virtual void GenerateInputRequestedRegion();
  
private:

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFilterImageToImage.txx"
#endif

#endif
