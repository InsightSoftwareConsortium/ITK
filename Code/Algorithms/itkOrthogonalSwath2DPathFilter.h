/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOrthogonalSwath2DPathFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkOrthogonalSwath2DPathFilter_h
#define _itkOrthogonalSwath2DPathFilter_h

#include "itkPathAndImageToPathFilter.h"
#include "itkOrthogonallyCorrected2DParametricPath.h"


namespace itk
{
/** \class OrthogonalSwath2DPathFilter
 * \brief Filter that optimizes a 2D path relative to an image.
 *
 * OrthogonalSwath2DPathFilter produces an OrthogonallyCorrected2DParametricPath
 * representation of a path that is optimal with respect to an image and an
 * original Fourier series path (sometimes referred to as an "initial contour"). 
 * Usage is a little complex.  The input image must be preprocessed with
 * ExtractOrthogonalSwath2DImageFilter (the user may want to smooth the image
 * first).  The user should then use the resulting swath image to produce a new
 * "merit" swath image of the EXACT same size as the swath image produced in the
 * preceeding step.  Each pixel value in the merit swath image indicates the
 * local merit of having the path pass through that swath index (taking the
 * absolute value of the vertical partial-derivative of the swath image is often
 * a good way to do this). Both the merit swath image and the path used to
 * extract the swath image should then be passed as inputs to this filter which
 * will search through the merit swath image using dynamic programming to find
 * the absolutely optimum (in terms of the swath image) path.  The test file
 * itkOrthogonalSwath2DPathFilterTest.cxx provides a good usage example.
 * 
 * \ingroup PathFilters
 */
template <class TFourierSeriesPath, class TSwathMeritImage>
class ITK_EXPORT OrthogonalSwath2DPathFilter : public
PathAndImageToPathFilter< TFourierSeriesPath, TSwathMeritImage,
                          OrthogonallyCorrected2DParametricPath >
{
public:
  /** Standard class typedefs. */
  typedef OrthogonalSwath2DPathFilter                         Self;
  typedef PathAndImageToPathFilter< TFourierSeriesPath, TSwathMeritImage,
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
  
  typedef TSwathMeritImage                      ImageType;
  typedef typename ImageType::ConstPointer      ImageConstPointer;
  
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

protected:
  OrthogonalSwath2DPathFilter();
  virtual ~OrthogonalSwath2DPathFilter();
  void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData(void);

private:
  OrthogonalSwath2DPathFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
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
  inline int &StepValue(int   f, int l, int x)
  {
    int rows=m_SwathSize[1];
    return m_StepValues[ (x*rows*rows) + (f*rows) + (l) ];
  }

  inline double &MeritValue(int  f, int l, int x)
  {
    int rows=m_SwathSize[1];
    return m_MeritValues[ (x*rows*rows) + (f*rows) + (l) ];
  }

  int *m_StepValues; // best y=error coordinate @ x of image for (0,F) -> (x+1,L)
  double *m_MeritValues;
  
  int *m_OptimumStepsValues;  // best step (e value) sequence for a closed path
  OrthogonalCorrectionTablePointer m_FinalOffsetValues;

  SizeType m_SwathSize;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkOrthogonalSwath2DPathFilter.txx"
#endif

#endif
