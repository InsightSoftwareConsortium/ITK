/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelOverlayImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelOverlayImageFilter_h
#define __itkLabelOverlayImageFilter_h

#include "itkLabelOverlayFunctor.h"
#include "itkBinaryFunctorImageFilter.h"
#include "itkConceptChecking.h"

namespace itk
{
/** \class LabelOverlayImageFilter
 * \brief Apply a colormap to a label image and put it on top of the
 *  input image
 *
 * Apply a colormap to a label image and put it on top of the input image.
 * The set of colors is a good selection of distinct colors. The opacity of
 * the label image can be defined by the user. The user can also choose if
 * the want to use a background and which label value is the background.
 * A background label produce a gray pixel with the same intensity
 * than the input one.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction,
 * INRA de Jouy-en-Josas, France.
 *
 * This class was contributed to the Insight Journal
 *     http://insight-journal.org/midas/handle.php?handle=1926/172
 *
 *
 * \sa ScalarToRGBPixelFunctor LabelToRGBImageFilter
 * \ingroup Multithreaded
 *
 */
template< typename  TInputImage, class TLabelImage, typename  TOutputImage >
class ITK_EXPORT LabelOverlayImageFilter:
  public
  BinaryFunctorImageFilter< TInputImage, TLabelImage, TOutputImage,
                            Functor::LabelOverlayFunctor<
                              typename TInputImage::PixelType,
                              typename TLabelImage::PixelType,
                              typename TOutputImage::PixelType >   >
{
public:
  /** Standard class typedefs. */
  typedef LabelOverlayImageFilter Self;

  typedef BinaryFunctorImageFilter< TInputImage, TLabelImage, TOutputImage,
                                    Functor::LabelOverlayFunctor<
                                      typename TInputImage::PixelType,
                                      typename TLabelImage::PixelType,
                                      typename TOutputImage::PixelType >   >  Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef TOutputImage OutputImageType;
  typedef TLabelImage  LabelImageType;
  typedef TInputImage  InputImageType;

  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TLabelImage::PixelType  LabelPixelType;
  typedef typename TInputImage::PixelType  InputPixelType;

  /** Runtime information support. */
  itkTypeMacro(LabelOverlayImageFilter, BinaryFunctorImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set the label image */
  void SetLabelImage(const TLabelImage *input);

  /** Get the label image */
  const LabelImageType * GetLabelImage() const;

  /** Set/Get the opacity of the colored label image. The value must be
   * between 0 and 1
   */
  itkSetMacro(Opacity, double);
  itkGetConstReferenceMacro(Opacity, double);

  /** Set/Get the background value */
  itkSetMacro(BackgroundValue, LabelPixelType);
  itkGetConstReferenceMacro(BackgroundValue, LabelPixelType);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( OutputHasPixelTraitsCheck,
                   ( Concept::HasPixelTraits< OutputPixelType > ) );
  itkConceptMacro( OutputPixelShouldHaveValueType,
                   ( Concept::HasValueType< OutputPixelType > ) );
  itkConceptMacro( OutputPixelShouldHaveBracketOperator,
                   ( Concept::BracketOperator<
                       OutputPixelType,
                       unsigned int,
                       typename OutputPixelType::ValueType > ) );
  /** End concept checking */
#endif

  /** Empty the color LUT container */
  void ResetColors();

  /** Get number of colors in the LUT container */
  unsigned int GetNumberOfColors() const;

  /** type of the color component */
  typedef typename OutputPixelType::ComponentType ComponentType;

  /** Add color to the LUT container */
  void AddColor(ComponentType r, ComponentType g, ComponentType b);

protected:
  LabelOverlayImageFilter();
  virtual ~LabelOverlayImageFilter() {}

  /** Process to execute before entering the multithreaded section */
  void BeforeThreadedGenerateData(void);

  /** Print internal ivars */
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  LabelOverlayImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);          //purposely not implemented

  double         m_Opacity;
  LabelPixelType m_BackgroundValue;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelOverlayImageFilter.txx"
#endif

#endif
