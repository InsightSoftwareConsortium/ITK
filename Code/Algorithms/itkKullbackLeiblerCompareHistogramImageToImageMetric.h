/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKullbackLeiblerCompareHistogramImageToImageMetric.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkKullbackLeiblerCompareHistogramImageToImageMetric_h
#define __itkKullbackLeiblerCompareHistogramImageToImageMetric_h

#include "itkCompareHistogramImageToImageMetric.h"

namespace itk
{
  /** \class KullbackLeiblerCompareHistogramImageToImageMetric 
    *  \brief Computes the Kubler Lieblach(KL) metric between the histogram
    *  of the two images to be registered and a training histogram.
    *
    *  This class is templated over the type of the fixed and moving
    *  images to be compared.
    *
    *  This class computers the KL-metric by comparing the histograms
    *  of the testing histogram formed by the overlap of intensities in
    *  the images, to a training histogram. It is based on the
    *  following paper:
    *
    *  Albert C.S. Chung, William M. Wells III, Alexander Norbash, and
    *  W. Eric L.  Grimson, Multi-modal Image Registration by
    *  Minimising Kullback-Leibler Distance, In Medical Image Computing
    *  and Computer-Assisted Intervention - MICCAI 2002, LNCS 2489,
    *  pp. 525 - 532.
    *
    *  The metric is given by KL(P_test||P_train) 
    *  = Sum_{i1,i2} P_test(i1,i2) vcl_log(P_test(i1,i2)/P_train(i1,i2))
    *  where P_test and P_train are probabilities given my normalized
    *  histograms, and i1 and i2 are the intensity bins in the histogram.
    *
    *  \par PARAMETERS 
    *  Epsilon is added to every bin in both histograms. This prevents
    *  division by zero problems. Epsilon should generally be set to a
    *  number smaller than one divided by the total number bins in
    *  the histogram. So, for a 256 by 256 histogram, Epsilon should be
    *  much less than 1e-5. Tests have shown that choices of epsilon are 
    *  not very important as long as it is small enough. The default is 1e-12.
    *  I doubt you will need to change it.
    *
    *  \author Samson Timoner
    *
    *  \par SUPPORT
    *  This work was supported by the Functional Imaging Research in
    *  Schizophrenia Testbed (FIRST) Biomedical Informatics Research
    *  Network (BIRN, http://www.nbirn.net), which is funded by the
    *  National Center for Research Resources at the National
    *  Institutes of Health (NIH).  This work is also funded by the
    *  Neuroimage Analysis Center (P41 RR13218).
    *
    *  \ingroup RegistrationMetrics 
    */

template <class TFixedImage, class TMovingImage>
class ITK_EXPORT KullbackLeiblerCompareHistogramImageToImageMetric :
public CompareHistogramImageToImageMetric<TFixedImage, TMovingImage>
{
 public:
  /** Standard class typedefs. */
  typedef KullbackLeiblerCompareHistogramImageToImageMetric Self;
  typedef CompareHistogramImageToImageMetric<TFixedImage, TMovingImage> Superclass;
  typedef SmartPointer<Self>                                     Pointer;
  typedef SmartPointer<const Self>                               ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(KullbackLeiblerCompareHistogramImageToImageMetric,
    HistogramImageToImageMetric);

  /** Types transferred from the base class */
  typedef typename Superclass::RealType                 RealType;
  typedef typename Superclass::TransformType            TransformType;
  typedef typename Superclass::TransformPointer         TransformPointer;
  typedef typename Superclass::ConstPointer             TransformConstPointer;
  typedef typename Superclass::TransformParametersType
    TransformParametersType;
  typedef typename Superclass::TransformJacobianType    TransformJacobianType;
  typedef typename Superclass::GradientPixelType        GradientPixelType;

  typedef typename Superclass::MeasureType              MeasureType;
  typedef typename Superclass::DerivativeType           DerivativeType;
  typedef typename Superclass::FixedImageType           FixedImageType;
  typedef typename Superclass::MovingImageType          MovingImageType;
  typedef typename Superclass::FixedImageConstPointer   FixedImageConstPointer;
  typedef typename Superclass::MovingImageConstPointer
    MovingImageConstPointer;

  typedef typename Superclass::HistogramType            HistogramType;
  typedef typename Superclass::HistogramSizeType        HistogramSizeType;
  typedef typename Superclass::MeasurementVectorType
    HistogramMeasurementVectorType;

  typedef typename Superclass::HistogramFrequencyType   HistogramFrequencyType;
  typedef typename Superclass::HistogramIteratorType    HistogramIteratorType;
  typedef typename Superclass::HistogramPointerType     HistogramPointerType;
  typedef typename Superclass::InterpolatorType         InterpolatorType;
  typedef typename Superclass::InterpolatorPointer      InterpolatorPointer;

  /** Set epsilon, which is added to each bin in both Histogram */
  itkSetMacro( Epsilon, double );

  /** Get epsilon, the histogram frequency to use if the frequency is 0 */
  itkGetConstReferenceMacro( Epsilon, double );

  /** Return the number of parameters required by the Transform */
  unsigned int GetNumberOfParameters(void) const 
  { return this->GetTransform()->GetNumberOfParameters(); }
 
  /** Forms the histogram of the training images to prepare to evaluate the */
  /** metric. Must set all parameters first */
  void Initialize() throw (ExceptionObject);

protected:
  /** Constructor is protected to ensure that \c New() function is used to
      create instances. */
  KullbackLeiblerCompareHistogramImageToImageMetric();
  virtual ~KullbackLeiblerCompareHistogramImageToImageMetric(){}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Form the Histogram for the Training data */
  void FormTrainingHistogram() throw (ExceptionObject);

  /** Evaluates the mutual information from the histogram. */
  virtual MeasureType EvaluateMeasure(HistogramType& histogram) const;

  double                  m_Epsilon;

private:
  // Purposely not implemented.
  KullbackLeiblerCompareHistogramImageToImageMetric(Self const&);
  void operator=(Self const&); // Purposely not implemented.
};

} // End namespace itk.

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKullbackLeiblerCompareHistogramImageToImageMetric.txx"
#endif

#endif // __itkKullbackLeiblerCompareHistogramImageToImageMetric_h
