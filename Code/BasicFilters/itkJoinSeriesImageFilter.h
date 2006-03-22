/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJoinSeriesImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkJoinSeriesImageFilter_h
#define __itkJoinSeriesImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{

/** \class JoinSeriesImageFilter
 * \brief Join N-D images into an (N+1)-D image
 *
 * This filter is templated over the input image type and the output image
 * type. The pixel type of them must be the same and the input dimension
 * must be less than the output dimension.
 * When the input images are N-dimensinal, they are joined in order and
 * the size of the N+1'th dimension of the output is same as the number of
 * the inputs. The spacing and the origin (where the first input is placed)
 * for the N+1'th dimension is specified in this filter. The output image 
 * informations for the first N dimensions are taken from the first input.
 * Note that all the inputs should have the same information.
 *
 * \ingroup GeometricTransforms
 * \ingroup Multithreaded
 * \ingroup Streamed
 *
 * \author Hideaki Hiraki
 *
 * Contributed in the users list
 * http://public.kitware.com/pipermail/insight-users/2004-February/006542.html
 *
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT JoinSeriesImageFilter:
    public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef JoinSeriesImageFilter  Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(JoinSeriesImageFilter, ImageToImageFilter);

  /** Compiler can't inherit typedef? */
  typedef typename Superclass::InputImageType InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef typename OutputImageType::Pointer OutputImagePointer;
  typedef typename InputImageType::RegionType InputImageRegionType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;

  /** Compiler can't inherit ImageDimension enumeration? */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TOutputImage::ImageDimension);

  /** Set/Get spacing of the new dimension */
  itkSetMacro(Spacing, double);
  itkGetMacro(Spacing, double);

  /** Set/Get origin of the new dimension */
  itkSetMacro(Origin, double);
  itkGetMacro(Origin, double);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck,
     (Concept::Convertible<typename TInputImage::PixelType,
                           typename TOutputImage::PixelType>));
  /** End concept checking */
#endif

protected:
  JoinSeriesImageFilter();
  ~JoinSeriesImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Overrides GenerateOutputInformation() in order to produce
   * an image which has a different information than the first input.
   * \sa ProcessObject::GenerateOutputInformaton() */
  virtual void GenerateOutputInformation();

  /** Overrides GenerateInputRequestedRegion() in order to inform
   * the pipeline execution model of different input requested regions
   * than the output requested region.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion();

  /** JoinSeriesImageFilter can be implemented as a multithreaded filter.
   * \sa ImageSource::ThreadedGenerateData(),
   *     ImageSource::GenerateData() */
  virtual void ThreadedGenerateData(const OutputImageRegionType&
                                    outputRegionForThread, int threadId );

private:
  JoinSeriesImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** IndexValueType is used to switch among the inputs and
   * is used as the index value of the new dimension */
  typedef unsigned int IndexValueType;

  double m_Spacing;
  double m_Origin;

};

  
} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkJoinSeriesImageFilter.txx"
#endif
  
#endif
