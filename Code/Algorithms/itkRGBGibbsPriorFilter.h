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

//#include "itkObject.h"
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
 * RGBGibbsPriorFilter is a class that apply Gibbs Prior model into 
 * segmentation of MRF images. The core of the method is to minimize
 * a Gibbsian form energy function.
 * The function can be divided into three part: f = f_1 + f_2 + f_3;
 * f_1 for the object homogeneity, f_2 for the boundary smoothness,
 * f_3 is the noise model. f_1 and f_3 is minimized in the function
 * GradientEnergy and f_2 is minized in the function GibbsTotalEnergy
 *
 * \ingroup MRFFilters
 */
template <class TInputImage, class TClassifiedImage>
class ITK_EXPORT RGBGibbsPriorFilter : public MRFImageFilter<TInputImage, 
  TClassifiedImage>
{
public:
  /** Standard class typedefs. */
  typedef RGBGibbsPriorFilter  Self;
  typedef MRFImageFilter<TInputImage, TClassifiedImage> Superclass;
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

  /** Type definitions for the labelled image.  It is derived from the
   * training image. */
  typedef typename TClassifiedImage::Pointer         LabelledImageType;
      
  /** Type definition for the classified image index type. */
  typedef typename TClassifiedImage::IndexType       LabelledImageIndexType;

  /** Type definitions for classifier to be used for the MRF labelling. */
  typedef Classifier<TInputImage,TClassifiedImage> ClassifierType;

  /** The type of input pixel. */
  typedef typename TInputImage::PixelType    InputImageVecType;

  /** Set the image required for training type classifiers. */
  void SetTrainingImage(TrainingImageType image);

  /** Set/Get the labelled image.  */
  void SetLabelledImage(LabelledImageType LabelledImage);
  LabelledImageType GetLabelledImage()
    { return m_LabelledImage; }

  /** Set the pointer to the classifer being used. */
  void SetClassifier( typename ClassifierType::Pointer ptrToClassifier );

  /** Set/Get the number of classes to segment. . */
  itkSetMacro(NumberOfClasses, unsigned int);
  itkGetMacro(NumberOfClasses, unsigned int);

  /** Set/Get the number of iteration of the Iterated Conditional Mode
   * (ICM) algorithm. A default value is set at 50 iterations. */
  itkSetMacro(MaximumNumberOfIterations, unsigned int);
  itkGetMacro(MaximumNumberOfIterations, unsigned int);

  /**  Threshold of the object size. */
  itkSetMacro(ClusterSize, unsigned int);

  /**  The label of object region. */  
  itkSetMacro(ObjectLabel, unsigned int);

  /** Extract the input image dimension. */
  enum {ImageDimension = TInputImage::ImageDimension };

  /** \todo Document. */
  void SetStartPoint (int x, int y, int z); 
  itkSetMacro(BoundaryGradient, unsigned int);
  void Advance();

  /** Specify the type of matrix to use. */
  typedef vnl_matrix<double> MatrixType; 

protected:
  RGBGibbsPriorFilter();
  ~RGBGibbsPriorFilter() {};

  void Allocate();

  virtual void MinimizeFunctional();
  virtual void GenerateData();
  virtual void ApplyGibbsLabeller();
  virtual void ApplyGPImageFilter();

//  void Allocate();

private:
  RGBGibbsPriorFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  InputImageType    m_InputImage;
  TrainingImageType   m_TrainingImage;
  LabelledImageType   m_LabelledImage;
  unsigned int m_NumberOfClasses;
  unsigned int m_MaximumNumberOfIterations;
  typename ClassifierType::Pointer m_ClassifierPtr;
  unsigned int m_BoundaryGradient;
  int m_GibbsNeighborsThreshold; 
  float m_BoundaryWt; 
  float m_GibbsPriorWt; 
  int m_StartRadius;
  int m_RecursiveNum;
  unsigned int *m_LabelStatus;

  typedef typename TInputImage::SizeType InputImageSizeType;
  InputImageType    m_MediumImage;

  float m_NewRegionThreshold;
  int m_Temp;

//  Parameter definitions
   
  int m_StartPoint[3]; 
  int m_StartModelSize; 

  int m_imgWidth;
  int m_imgHeight;
  int m_imgDepth;
  unsigned int m_ClusterSize;
  unsigned int m_ObjectLabel;
  int m_VecDim;
  InputPixelType m_LowPoint;

  unsigned short *m_Region;
  unsigned short *m_RegionCount;

  void  GibbsTotalEnergy(int i);
  float GibbsEnergy(int i, int k, int k1);
  int Sim(int a, int b);
  int LabelRegion(int i, int l, int change);
  void  RegionEraser();
  void  GenerateMediumImage();

  void  GreyScalarBoundary(LabelledImageIndexType Index3D); 

};

} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGBGibbsPriorFilter.txx"
#endif
#endif
