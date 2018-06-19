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
#ifndef itkObjectnessMeasureImageFilter_h
#define itkObjectnessMeasureImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/** \class ObjectnessMeasureImageFilter
 * \brief Enhance M-dimensional objects in N-dimensional images.
 *
 * This filter is a generalization of Frangi's vesselness measurement
 * for detecting M-dimensional object in N-dimensional space. For
 * example a vessel is a 1-D object in 3-D space. The filter
 * can enhance blob-like structures (M=0), vessel-like structures
 * (M=1), 2D plate-like structures (M=2), hyper-plate-like structures
 * (M=3) in N-dimensional images, with M<N.
 *
 * This filter takes a scalar image as input and produces a real
 * valued image as output which contains the objectness measure at
 * each pixel. Internally, it computes a Hessian via discrete central
 * differences. Before applying this filter it is expected that a
 * Gaussian smoothing filter at an appropriate scale (sigma) was
 * applied to the input image.
 *
 * The enhancement is based on the eigenvalues of the Hessian
 * matrix. For the Frangi's vesselness case were M=1 and N=3 we have
 * the 3 eigenvalues such that
 * \f$ | \lambda_1 | < | \lambda_2 | < |\lambda_3 | \f$. The formula
 * follows:
 *
 *  \f[
 * R_A = \frac{|\lambda_2|}{|\lambda_3|}, \;
 * R_B = \frac{|\lambda_2|}{|\lambda_2\lambda_3|}, \;
 * S = \sqrt{\lambda_1^2+\lambda_2^2+\lambda_3^2}
 *  \f]
 *  \f[
 *     V_{\sigma}=
 *     \begin{cases}
 *      (1-e^{-\frac{R_A^2}{2\alpha^2}}) \cdot e^{\frac{R_B^2}{2\beta^2}} \cdot (1-e^{-\frac{S^2}{2\gamma^2}}) & \text{if } \lambda_2<0 \text{ and } \lambda_3<0 \text{,}\\
 *      0 & \text{otherwise}
 *    \end{cases}
 *  \f]
 *
 * \par References
 * Antiga, L. Generalizing vesselness with respect to dimensionality and shape. https://hdl.handle.net/1926/576
 *
 * \par
 * Frangi, AF, Niessen, WJ, Vincken, KL, & Viergever, MA (1998). Multiscale Vessel
 * Enhancement Filtering. In Wells, WM, Colchester, A, & Delp, S, Editors, MICCAI '98
 * Medical Image Computing and Computer-Assisted Intervention, Lecture Notes in Computer
 * Science, pages 130-137, Springer Verlag, 1998.
 *
 *
 * \sa itk::HessianToObjectnessMeasureImageFilter
 *
 * \ingroup SimpleITKFilters
 */
template< typename TInputImage, typename TOutputImage >
class ObjectnessMeasureImageFilter
  : public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef ObjectnessMeasureImageFilter Self;

  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  typedef typename Superclass::InputImageType  InputImageType;
  typedef typename Superclass::OutputImageType OutputImageType;

  typedef double                                   InternalType;

  /** Image dimension */
  itkStaticConstMacro(ImageDimension, unsigned int, InputImageType::ImageDimension);


  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(ObjectnessMeasureImageFilter, ImageToImageFilter);


  /** Set/Get Alpha, the weight corresponding to R_A
   * (the ratio of the smallest eigenvalue that has to be large to the larger ones).
   * Smaller values lead to increased sensitivity to the object dimensionality. */
  itkSetMacro(Alpha, double);
  itkGetConstMacro(Alpha, double);

  /** Set/Get Beta, the weight corresponding to R_B
   * (the ratio of the largest eigenvalue that has to be small to the larger ones).
   * Smaller values lead to increased sensitivity to the object dimensionality. */
  itkSetMacro(Beta, double);
  itkGetConstMacro(Beta, double);

  /** Set/Get Gamma, the weight corresponding to S
   * (the Frobenius norm of the Hessian matrix, or second-order structureness) */
  itkSetMacro(Gamma, double);
  itkGetConstMacro(Gamma, double);

  /** Toggle scaling the objectness measure with the magnitude of the largest
    absolute eigenvalue */
  itkSetMacro(ScaleObjectnessMeasure, bool);
  itkGetConstMacro(ScaleObjectnessMeasure, bool);
  itkBooleanMacro(ScaleObjectnessMeasure);

  /** Set/Get the dimensionality of the object (0: points (blobs),
   * 1: lines (vessels), 2: planes (plate-like structures), 3: hyper-planes.
   * ObjectDimension must be smaller than ImageDimension. */
  itkSetMacro(ObjectDimension, unsigned int);
  itkGetConstMacro(ObjectDimension, unsigned int);

  /** Enhance bright structures on a dark background if true, the opposite if
    false. */
  itkSetMacro(BrightObject, bool);
  itkGetConstMacro(BrightObject, bool);
  itkBooleanMacro(BrightObject);


protected:
  ObjectnessMeasureImageFilter();

  ~ObjectnessMeasureImageFilter();


  void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ObjectnessMeasureImageFilter);

  double       m_Alpha;
  double       m_Beta;
  double       m_Gamma;
  unsigned int m_ObjectDimension;
  bool         m_BrightObject;
  bool         m_ScaleObjectnessMeasure;

};

}


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkObjectnessMeasureImageFilter.hxx"
#endif

#endif // itkObjectnessMeasureImageFilter_h
