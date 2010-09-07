/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnchorOpenCloseImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkAnchorOpenCloseImageFilter_h
#define __itkAnchorOpenCloseImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkProgressReporter.h"
#include "itkAnchorOpenCloseLine.h"
#include "itkAnchorErodeDilateLine.h"
#include "itkBresenhamLine.h"

namespace itk
{
/**
 * \class AnchorOpenCloseImageFilter
 * \brief class to implement openings and closings using anchor
 * methods.
 *
 * Anchor methods directly implement opening/closing by line structuring elements, and
 * erosion/dilation is slightly more complicated. This class used line
 * structuring elements to produce more complex shaped SEs, and must
 * be instantiated with a decomposable structuring element type such
 * as FlatStructuringElement. The direct implementation of openings by
 * lines gives a mechanism to short cut the decomposition slightly -
 * e.g. in the case of a rectangle the basic decomposition is Ex Ey Ez
 * Dz Dy Dz, which can be changed to Ex Ey Oz Dy Dx, where Ex, Dx and Ox
 * indicate erosions, dilations and openings along the x
 * direction. Because anchor operations do openings directly, this is
 * a saving of one pass through the filter. Unfortunately it results
 * in more complex template parameters because the appropriate
 * comparison operations need to be passed in. The less
 *
 */
template< class TImage, class TKernel,
          class LessThan, class GreaterThan, class LessEqual, class GreaterEqual >
//          class THistogramCompare,
//          class TFunction1, class TFunction2>
class ITK_EXPORT AnchorOpenCloseImageFilter:
  public ImageToImageFilter< TImage, TImage >
{
public:
  /** Standard class typedefs. */
  typedef AnchorOpenCloseImageFilter           Self;
  typedef ImageToImageFilter< TImage, TImage > Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

  /** Some convenient typedefs. */
  /** Kernel typedef. */
  typedef TKernel                    KernelType;
  typedef typename KernelType::LType KernelLType;

  typedef TImage                                InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  /** ImageDimension constants */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(AnchorOpenCloseImageFilter,
               ImageToImageFilter);

  void SetKernel(const KernelType & kernel)
  {
    m_Kernel = kernel;
    m_KernelSet = true;
  }

protected:
  AnchorOpenCloseImageFilter();
  ~AnchorOpenCloseImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Multi-thread version GenerateData. */
  void  ThreadedGenerateData(const InputImageRegionType & outputRegionForThread,
                             int threadId);

  /** GrayscaleMorphologicalOpeningImageFilter need to make sure they request enough of an
   * input image to account for the structuring element size.  The input
   * requested region is expanded by the radius of the structuring element.
   * If the request extends past the LargestPossibleRegion for the input,
   * the request is cropped by the LargestPossibleRegion. */
  void GenerateInputRequestedRegion();

  InputImagePixelType m_Boundary1, m_Boundary2;
private:
  AnchorOpenCloseImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);             //purposely not implemented

  TKernel m_Kernel;
  bool    m_KernelSet;
  typedef BresenhamLine< itkGetStaticConstMacro(InputImageDimension) > BresType;
  typedef typename BresType::OffsetArray                               BresOffsetArray;

  // the class that operates on lines -- does the opening in one
  // operation. The classes following are named on the assumption that
  // we are doing an opening

//  typedef AnchorOpenCloseLine<InputImagePixelType, THistogramCompare,
// TFunction1, TFunction2> AnchorLineOpenType;
  typedef AnchorOpenCloseLine< InputImagePixelType, LessThan, GreaterEqual, LessEqual > AnchorLineOpenType;

  typedef AnchorErodeDilateLine< InputImagePixelType, LessThan, LessEqual > AnchorLineErodeType;

  // the class that does the dilation
  typedef AnchorErodeDilateLine< InputImagePixelType, GreaterThan, GreaterEqual > AnchorLineDilateType;

  void DoFaceOpen(InputImageConstPointer input,
                  InputImagePointer output,
                  InputImagePixelType border,
                  KernelLType line,
                  AnchorLineOpenType & AnchorLineOpen,
                  const BresOffsetArray LineOffsets,
                  InputImagePixelType *outbuffer,
                  const InputImageRegionType AllImage,
                  const InputImageRegionType face);
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAnchorOpenCloseImageFilter.txx"
#endif

#endif
