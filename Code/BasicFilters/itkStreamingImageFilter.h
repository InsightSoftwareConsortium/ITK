/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStreamingImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkStreamingImageFilter_h
#define __itkStreamingImageFilter_h

#include "itkImageSource.h"
#include "itkImageToImageFilter.h"
#include "itkImageRegionSplitter.h"

namespace itk
{

/** \class StreamingImageFilter
 * \brief Pipeline object to control data streaming for large data processing.
 *
 * StreamingImageFilter is a pipeline object that allows the user to control
 * how data is pulled through the pipeline.  To generate its
 * OutputRequestedRegion, this filter will divide the output into several
 * pieces (controlled by SetNumberOfStreamDivisions), and call the upstream
 * pipeline for each piece, tiling the individual outputs into one large
 * output. This reduces the memory footprint for the application since
 * each filter does not have to process the entire dataset at once.
 * This filter will produce the entire output as one image, but the upstream
 * filters will do their processing in pieces.
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataProcessing 
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT StreamingImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef StreamingImageFilter  Self;
  typedef ImageToImageFilter<TInputImage, TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(StreamingImageFilter,ImageToImageFilter);

  /** Some typedefs for the input and output. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType; 
  typedef typename InputImageType::PixelType InputImagePixelType; 
  typedef TOutputImage OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename OutputImageType::RegionType OutputImageRegionType; 
  typedef typename OutputImageType::PixelType OutputImagePixelType; 
  typedef typename Superclass::DataObjectPointer DataObjectPointer;

  /** Dimension of input image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      InputImageType::ImageDimension);

  /** SmartPointer to a region splitting object */
  typedef ImageRegionSplitter<itkGetStaticConstMacro(InputImageDimension)>
  SplitterType;
  typedef typename SplitterType::Pointer RegionSplitterPointer;
  
  /** Set the number of pieces to divide the input.  The upstream pipeline
   * will be executed this many times. */
  itkSetMacro(NumberOfStreamDivisions,unsigned int);

  /** Get the number of pieces to divide the input. The upstream pipeline
   * will be executed this many times. */
  itkGetConstReferenceMacro(NumberOfStreamDivisions,unsigned int);

  /** Set the helper class for dividing the input into chunks. */
  itkSetObjectMacro(RegionSplitter, SplitterType);

  /** Get the helper class for dividing the input into chunks. */
  itkGetObjectMacro(RegionSplitter, SplitterType);

  /** Override UpdateOutputData() from ProcessObject to divide upstream
   * updates into pieces. This filter does not have a GenerateData()
   * or ThreadedGenerateData() method.  Instead, all the work is done
   * in UpdateOutputData() since it must update a little, execute a little,
   * update some more, execute some more, etc. */
  virtual void UpdateOutputData(DataObject *output);
  
protected:
  StreamingImageFilter();
  ~StreamingImageFilter();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  StreamingImageFilter(const StreamingImageFilter&); //purposely not implemented
  void operator=(const StreamingImageFilter&); //purposely not implemented

  unsigned int m_NumberOfStreamDivisions;
  RegionSplitterPointer m_RegionSplitter;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStreamingImageFilter.txx"
#endif

#endif
