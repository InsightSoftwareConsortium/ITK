/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRGBGibbsPriorFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkRGBGibbsPriorFilter_h
#define __itkRGBGibbsPriorFilter_h

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

#include "itkImageToImageFilter.h"
#include "itkSupervisedClassifier.h"
#include "itkMRFImageFilter.h"

namespace itk
{

/** \class RGBGibbsPriorFilter
 * \brief 
 *
 * RGBGibbsPriorFilter applies Gibbs Prior model for the segmentation 
 * of MRF images. The core of the method is based on the minimization of a
 * Gibbsian energy function.
 * This energy function f can be divided into three part: 
 *   f = f_1 + f_2 + f_3;
 * f_1 is related to the object homogeneity,
 * f_2 is related to the boundary smoothness,
 * f_3 is related to the noise model. 
 * The two force components f_1 and f_3 are minimized by the GradientEnergy 
 * method while f_2 is minized by the GibbsTotalEnergy method.
 *
 * \ingroup MRFFilters */
template <class TInputImage, class TClassifiedImage>
class ITK_EXPORT RGBGibbsPriorFilter : public MRFImageFilter<TInputImage, 
  TClassifiedImage>
{
public:
  /** Standard "Self" typedef.*/
  typedef RGBGibbsPriorFilter  Self;

  /** Standard "Superclass" typedef.*/
  typedef MRFImageFilter<TInputImage, TClassifiedImage> Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RGBGibbsPriorFilter,MRFImageFilter);

  /** A smart pointer to the input image type. */
  typedef typename TInputImage::Pointer              InputImageType;  

  /** Type definition for the input image pixel type. */
  typedef typename TInputImage::PixelType            InputPixelType;

  /** Type definitions for the training image. */
  typedef typename TClassifiedImage::Pointer         TrainingImageType;

  /** Type definitions for the labelled image.
   *  It is derived from the training image.*/
  typedef typename TClassifiedImage::Pointer         LabelledImageType;
     
  /** Type definition for the classified image index type. */
  typedef typename TClassifiedImage::IndexType       LabelledImageIndexType;

  /**
   * Type definitions for classifier to be used for the Gibbs lavbelling.
   */
  typedef Classifier<TInputImage,TClassifiedImage> ClassifierType;

  /** The type of input pixel. */
  typedef typename TInputImage::PixelType    InputImageVecType;
  typedef typename TInputImage::IndexType    IndexType;

  /** Set the image required for training type classifiers. */
  void SetTrainingImage(TrainingImageType image);

  /** Set the labelled image.*/
  void SetLabelledImage(LabelledImageType LabelledImage);

  /** Get the labelled image. */
  LabelledImageType GetLabelledImage()
    { return m_LabelledImage; }

  /** Set the pointer to the classifer being used. */
  void SetClassifier( typename ClassifierType::Pointer ptrToClassifier );

  /** Set the Number of classes. */
  itkSetMacro(NumberOfClasses, unsigned int);

  /** Get the Number of classes. */
  itkGetMacro(NumberOfClasses, unsigned int);

  /** Set/Get the number of iteration of the Iterated Conditional Mode
   * (ICM) algorithm. A default value is set at 50 iterations. */
  itkSetMacro(MaximumNumberOfIterations, unsigned int);

  /** Get the number of iterations of the Iterated Conditional Mode
   * (ICM) algorithm.*/
  itkGetMacro(MaximumNumberOfIterations, unsigned int);

  /** Set the threshold for the object size. */
  itkSetMacro(ClusterSize, unsigned int);

  /** Set the label for the object region. */ 
  itkSetMacro(ObjectLabel, unsigned int);

  /** Extract the input image dimension. */
  enum {ImageDimension = TInputImage::ImageDimension };

  itkSetMacro(StartPoint, IndexType); 

  itkSetMacro(BoundaryGradient, unsigned int);

  /** Specify the type of matrix to use. */
  typedef vnl_matrix<double> MatrixType; 

protected:
  RGBGibbsPriorFilter();
  ~RGBGibbsPriorFilter() {};

  void Allocate(); /** allocate memory space for the filter. */

  virtual void MinimizeFunctional();
  virtual void GenerateData();
  virtual void ApplyGibbsLabeller();
  virtual void ApplyGPImageFilter();


private:
  RGBGibbsPriorFilter(const Self&); 
  void operator=(const Self&); 
  
  typedef typename TInputImage::SizeType InputImageSizeType;
  typename ClassifierType::Pointer m_ClassifierPtr;

  InputImageType      m_InputImage;    /** the input */
  InputImageType      m_MediumImage;   /** the medium image to store intermedium result */
  TrainingImageType   m_TrainingImage; /** image to train the filter. */
  LabelledImageType   m_LabelledImage; /** output */

  float m_BoundaryWt; /** weight for H_1 */
  float m_GibbsPriorWt; /** weight for H_2 */
  int   m_StartRadius;  /** define the start region of the object. */
  int   m_Temp;         /** for SA algo. */
  IndexType m_StartPoint; /** the seed of object */
  int m_BoundaryGradient; /** the threshold for the existence of a boundary. */
  int m_RecursiveNum;     /** number of SA iterations. */
  unsigned int      *m_LabelStatus; /** array for the state of each pixel. */

  int         m_imgWidth; /** image size. */
  int         m_imgHeight;
  int         m_imgDepth;
  int         m_ClusterSize; /** region size smaller than the threshold will be erased. */
  int         m_ObjectLabel; /** the label for object region. */
  int         m_VecDim;      /** the channel number in the image. */
  int         m_NumberOfClasses; /** the number of class need to be classified. */
  unsigned int      m_MaximumNumberOfIterations; /** number of the iteration. */
  InputPixelType    m_LowPoint;  /** the point give lowest value of H-1 in neighbor. */

  unsigned short    *m_Region;   /** for region erase. */
  unsigned short    *m_RegionCount;  /** for region erase. */

  void  GibbsTotalEnergy(int i); /** calculate H_2. */
  float GibbsEnergy(int i, int k, int k1); /** calculate the energy in each cluster. */
  int Sim(int a, int b);         /** method to return 1 when a equal to b. */
  int LabelRegion(int i, int l, int change);  /** help to erase the small region. */
  void  RegionEraser();                       /** erase the small region. */
  void  GenerateMediumImage();                /** create the intermedium image. */
  void  GreyScalarBoundary(LabelledImageIndexType Index3D); /** calculate H_1. */

};

} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGBGibbsPriorFilter.txx"
#endif
#endif
