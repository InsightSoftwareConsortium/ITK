/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkKappaSigmaThresholdImageCalculator_h
#define itkKappaSigmaThresholdImageCalculator_h

#include "itkMacro.h"
#include "itkImage.h"

namespace itk
{
/** \class KappaSigmaThresholdImageCalculator
 * \brief Computes a Kappa-Sigma-Clipping threshold for an image.
 *
 * When an image is mostly composed of background pixels, most of the automatic
 * thresholding methods fail to produce a relevant threshold.  This is mainly
 * because one mode (e.g the background) is over-represented in the
 * histogram. The basic idea of Kappa-Sigma-Clipping is to find the properties
 * of the over-represented mode and re-compute a threshold with only the pre-thresholded
 * pixels (thus rejecting significantly different pixels iteratively). This
 * algorithm does not converge to a specific value, hence a number of iterations
 * must be provided.
 *
 * On each iteration, the new threshold [t_(i+1)] is computed as follows (using only
 * pixels thresholded with [t_i]):
 *
 * t_(i+1) = Mean_(i+1)  +  Kappa * Sigma_(i+1)
 *
 * \author Gaetan Lehmann
 * \note This class was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/367
 *
 * \ingroup Operators
 * \ingroup ITKThresholding
 */
template< typename TInputImage, typename TMaskImage >
class ITK_TEMPLATE_EXPORT KappaSigmaThresholdImageCalculator:public Object
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

  /** Set an optional input mask to only consider in the computation pixels with
   * a specific mask value (MaskValue). If no mask is set (default), the entire
   * image will be considered. */
  itkSetConstObjectMacro(Mask, MaskImageType);

  /** Set the mask value used to select which pixels will be considered in the
   * computation (e.g. only pixels which satisfy (m_Mask->GetPixel(Index()) == m_MaskValue)
   * are considered). */
  itkSetMacro(MaskValue, MaskPixelType);
  itkGetConstMacro(MaskValue, MaskPixelType);

  /** Set the Sigma multiplier (Kappa) to adjust the pixel rejection rate. */
  itkSetMacro(SigmaFactor, double);
  itkGetConstMacro(SigmaFactor, double);

  /** Set the number of rejection passes. */
  itkSetMacro(NumberOfIterations, unsigned int);
  itkGetConstMacro(NumberOfIterations, unsigned int);

  /** Run and compute threshold. */
  void Compute();

  /** Get the computed threshold. */
  const InputPixelType & GetOutput() const;

protected:
  KappaSigmaThresholdImageCalculator();
  virtual ~KappaSigmaThresholdImageCalculator() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(KappaSigmaThresholdImageCalculator);

  bool           m_Valid;             // Have moments been computed yet?
  MaskPixelType  m_MaskValue;
  double         m_SigmaFactor;
  unsigned int   m_NumberOfIterations;
  InputPixelType m_Output;

  InputImageConstPointer m_Image;
  MaskImageConstPointer  m_Mask;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkKappaSigmaThresholdImageCalculator.hxx"
#endif

#endif /* itkKappaSigmaThresholdImageCalculator_h */
