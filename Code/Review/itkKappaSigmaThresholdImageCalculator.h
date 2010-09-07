/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKappaSigmaThresholdImageCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkKappaSigmaThresholdImageCalculator_h
#define __itkKappaSigmaThresholdImageCalculator_h

#include "itkMacro.h"
#include "itkImage.h"

namespace itk
{
/** \class KappaSigmaThresholdImageCalculator
 * \brief Compute moments of an n-dimensional image.
 *
 *
 * \author Gaetan Lehmann
 *
 * This class was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/367
 *
 * \ingroup Operators
 *
 */
template< class TInputImage, class TMaskImage >
class ITK_EXPORT KappaSigmaThresholdImageCalculator:public Object
{
public:
  /** Standard class typedefs. */
  typedef KappaSigmaThresholdImageCalculator Self;
  typedef Object                             Superclass;
  typedef SmartPointer< Self >               Pointer;
  typedef SmartPointer< const Self >         ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(KappaSigmaThresholdImageCalculator, Object);

  /** Extract the dimension of the image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard image type within this class. */
  typedef TInputImage InputImageType;
  typedef TMaskImage  MaskImageType;

  /** Standard image type pointer within this class. */
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename MaskImageType::Pointer       MaskImagePointer;
  typedef typename MaskImageType::ConstPointer  MaskImageConstPointer;

  typedef typename InputImageType::PixelType InputPixelType;
  typedef typename MaskImageType::PixelType  MaskPixelType;

  /** Set the input image. */
  itkSetConstObjectMacro(Image, InputImageType);

  /** Set the input mask */
  itkSetConstObjectMacro(Mask, MaskImageType);

  itkSetMacro(MaskValue, MaskPixelType);
  itkGetConstMacro(MaskValue, MaskPixelType);

  itkSetMacro(SigmaFactor, double);
  itkGetConstMacro(SigmaFactor, double);

  itkSetMacro(NumberOfIterations, unsigned int);
  itkGetConstMacro(NumberOfIterations, unsigned int);

  /** Compute moments of a new or modified image.
   * This method computes the moments of the image given as a
   * parameter and stores them in the object.  The values of these
   * moments and related parameters can then be retrieved by using
   * other methods of this object. */
  void Compute(void);

  const InputPixelType & GetOutput() const;

protected:
  KappaSigmaThresholdImageCalculator();
  virtual ~KappaSigmaThresholdImageCalculator() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  KappaSigmaThresholdImageCalculator(const Self &); //purposely not implemented
  void operator=(const Self &);                     //purposely not implemented

  bool           m_Valid;             // Have moments been computed yet?
  MaskPixelType  m_MaskValue;
  double         m_SigmaFactor;
  unsigned int   m_NumberOfIterations;
  InputPixelType m_Output;

  InputImageConstPointer m_Image;
  MaskImageConstPointer  m_Mask;
};  // class KappaSigmaThresholdImageCalculator
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKappaSigmaThresholdImageCalculator.txx"
#endif

#endif /* __itkKappaSigmaThresholdImageCalculator_h */
