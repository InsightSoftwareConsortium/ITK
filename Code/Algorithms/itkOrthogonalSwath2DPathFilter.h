/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrthogonalSwath2DPathFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkOrthogonalSwath2DPathFilter_h
#define _itkOrthogonalSwath2DPathFilter_h

#include "itkPathAndImageToPathFilter.h"
#include "itkOrthogonallyCorrected2DParametricPath.h"
// Filters used internally
#include "itkExtractOrthogonalSwath2DImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkDerivativeImageFilter.h"


namespace itk
{
/** \class OrthogonalSwath2DPathFilter
 * \brief Filter that optimizes a 2D path relative to an image.
 *
 * OrthogonalSwath2DPathFilter produces an OrthogonallyCorrected2DParametricPath
 * representation of a path that is optimal with respect to an image and an
 * original Fourier series path (sometimes referred to as an "initial contour"). 
 * A rectangular "swath" image is extracted from the input image by interpolating
 * image pixels orthogonal to the initial contour while walking along the initial
 * contour.  The swath image is then processed by a merit filter of the user's
 * choosing before dynamic programming is used to find the "optimal" path through
 * the image, where the optimality of an index along a path is the value of the
 * swath image at that pixel after the swath has been processed by the user's
 * filter.  The vertical (y-axis) partial derivative is often a good choice for
 * a merit filter.  The user will probably also want to smooth the input image
 * before passing it to this filter.
 * 
 * \ingroup PathFilters
 */
template <class TFourierSeriesPath, class TImage>
class ITK_EXPORT OrthogonalSwath2DPathFilter : public
PathAndImageToPathFilter< TFourierSeriesPath, TImage,
                          OrthogonallyCorrected2DParametricPath >
{
public:
  /** Standard class typedefs. */
  typedef OrthogonalSwath2DPathFilter                         Self;
  typedef PathAndImageToPathFilter< TFourierSeriesPath, TImage,
                      OrthogonallyCorrected2DParametricPath > Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(OrthogonalSwath2DPathFilter, PathAndImageToPathFilter);

  /** Some convenient typedefs. */
  typedef TFourierSeriesPath                    InputPathType;
  typedef typename InputPathType::Pointer       InputPathPointer;
  typedef typename InputPathType::InputType     InputPathInputType;
  
  typedef TImage                                ImageType;
  typedef typename ImageType::Pointer           ImagePointer;
  
  typedef OrthogonallyCorrected2DParametricPath OutputPathType;
  typedef typename OutputPathType::Pointer      OutputPathPointer;
  typedef typename OutputPathType::InputType    OutputPathInputType;
  typedef typename OutputPathType::OrthogonalCorrectionTableType
                                                OrthogonalCorrectionTableType;
  typedef typename OutputPathType::OrthogonalCorrectionTablePointer
                                                OrthogonalCorrectionTablePointer;
  
  typedef typename InputPathType::IndexType     IndexType;
  typedef typename InputPathType::OffsetType    OffsetType;
  typedef typename ImageType::SizeType          SizeType;

  typedef Image<double, 2>                                  FloatImageType;
  typedef typename FloatImageType::Pointer                  FloatImagePointer;
  typedef ExtractOrthogonalSwath2DImageFilter<ImageType>        SwathFilterType;
  typedef RescaleIntensityImageFilter<ImageType,FloatImageType> CastFilterType;
  typedef DerivativeImageFilter<FloatImageType,FloatImageType>  MeritFilterType;
  
protected:
  OrthogonalSwath2DPathFilter();
  virtual ~OrthogonalSwath2DPathFilter();
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData(void);

private:
  OrthogonalSwath2DPathFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  typename SwathFilterType::Pointer m_SwathFilter;
  typename CastFilterType::Pointer m_CastFilter;
  typename MeritFilterType::Pointer m_MeritFilter;
  
  SizeType m_SwathSize;
  
  // Find the "L" for the maximum merit over the range L-1 to L+1 at F & x.
  // This value is both returned and stored in m_StepValues.
  // The merits for F & x at L-1 to L+1 must have already been calculated.
  unsigned int FindAndStoreBestErrorStep(unsigned int x, unsigned int F,
                                                         unsigned int L);
  
  // m_StepValues & m_MeritValues are stored as datatype[x][F][L] which requres
  // cols*rows*rows bytes of storage where rows and cols are the dimensions of
  // the processed image.
  // 
  // This ordering of elements is most efficient when L is incremented in the
  // inner-most loop and x is incremented in the outer-most loop.
  // 
  // m_StepValues & m_MeritValues should always be accessed using the StepValue()
  // and MeritValue() access functions.  StepValue() and MeritValue() can each be
  // used on both the left and right hand of assignments for reads & writes, ex: 
  // StepValue(1,1,1) = 2+MeritValue(0,0,3);
  inline int &StepValue(int   f, int l, int x);
  inline double &MeritValue(int  f, int l, int x);
  int *m_StepValues; // best y=error coordinate @ x of image for (0,F) -> (x+1,L)
  double *m_MeritValues;
  
  int *m_OptimumStepsValues;  // best step (e value) sequence for a closed path
  OrthogonalCorrectionTablePointer m_FinalOffsetValues;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOrthogonalSwath2DPathFilter.txx"
#endif

#endif
