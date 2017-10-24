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
#ifndef itkRGBGibbsPriorFilter_h
#define itkRGBGibbsPriorFilter_h

#include "vnl/vnl_vector.h"
#include "vnl/vnl_matrix.h"

#include "itkMRFImageFilter.h"

namespace itk
{
/** \class RGBGibbsPriorFilter
 * \brief The RGBGibbsPriorFilter applies Gibbs Prior model for the segmentation
 * of MRF images.
 *
 * The core of the method is based on the minimization of a Gibbsian energy function.
 * This energy function f can be divided into three part:
 *   f = f_1 + f_2 + f_3;
 * f_1 is related to the object homogeneity,
 * f_2 is related to the boundary smoothness,
 * f_3 is related to the constraint of the observation (or the noise model).
 * The two force components f_1 and f_3 are minimized by the GradientEnergy
 * method while f_2 is minized by the GibbsTotalEnergy method.
 *
 * This filter only works with 3D images.
 *
 * \ingroup MRFFilters
 * \ingroup ITKMarkovRandomFieldsClassifiers
 */
template< typename TInputImage, typename TClassifiedImage >
class ITK_TEMPLATE_EXPORT RGBGibbsPriorFilter:public MRFImageFilter< TInputImage,
                                                            TClassifiedImage >
{
public:
  /** Standard "Self" typedef. */
  typedef RGBGibbsPriorFilter                             Self;
  typedef MRFImageFilter< TInputImage, TClassifiedImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RGBGibbsPriorFilter, MRFImageFilter);

  /** Types from superclass.  */
  typedef typename Superclass::InputImagePixelType           InputImagePixelType;
  typedef typename Superclass::InputImageRegionConstIterator InputImageRegionConstIterator;
  typedef typename Superclass::InputImageRegionIterator      InputImageRegionIterator;
  typedef typename Superclass::LabelledImageRegionIterator   LabelledImageRegionIterator;
  typedef typename Superclass::LabelledImagePixelType        LabelledImagePixelType;
  typedef typename Superclass::IndexValueType                IndexValueType;

  /** A smart pointer to the input image type. */
  typedef TInputImage                        InputImageType;
  typedef typename TInputImage::Pointer      InputImagePointer;
  typedef typename TInputImage::ConstPointer InputImageConstPointer;

  /** Type definition for the input image pixel type. */
  typedef typename TInputImage::PixelType InputPixelType;

  /** Type definitions for the training image. */
  typedef TClassifiedImage                   ClassifiedImageType;
  typedef typename TClassifiedImage::Pointer TrainingImageType;

  /** Type definitions for the labelled image.
   *  It is derived from the training image. */
  typedef typename TClassifiedImage::Pointer LabelledImageType;

  /** Type definition for the classified image index type. */
  typedef typename TClassifiedImage::IndexType LabelledImageIndexType;

  /** Type used as identifier for the Labels
   \warning -1 cannot be used as the identifier for unlabeled pixels
   the NumericTraits<>::max() value is used for indicating unlabeled pixels */
  typedef unsigned int LabelType;

  /** Type definitions for classifier to be used for the MRF lavbelling. */
  typedef ImageClassifierBase< TInputImage, TClassifiedImage > ClassifierType;

  /** The type of input pixel. */
  typedef typename TInputImage::PixelType InputImageVecType;
  typedef typename TInputImage::IndexType IndexType;

  /** Set the image required for training type classifiers. */
  void SetTrainingImage(TrainingImageType image);

  /** Set the labelled image. */
  void SetLabelledImage(LabelledImageType LabelledImage);

  /** Get the labelled image. */
  LabelledImageType GetLabelledImage()
  { return m_LabelledImage; }

  /** Set the pointer to the classifer being used. */
  void SetClassifier(typename ClassifierType::Pointer ptrToClassifier);

  /** Set the Number of classes. */
  virtual void SetNumberOfClasses( const unsigned int numberOfClasses ) ITK_OVERRIDE
    {
    itkDebugMacro("setting NumberOfClasses to " << numberOfClasses );
    if ( this->m_NumberOfClasses != numberOfClasses )
      {
      this->m_NumberOfClasses = numberOfClasses;
      this->Modified();
      }
    }

  /** Get the Number of classes. */
  virtual unsigned int GetNumberOfClasses() const ITK_OVERRIDE
    {
    return this->m_NumberOfClasses;
    }

  /** Set/Get the number of iteration of the Iterated Conditional Mode
   * (ICM) algorithm. A default value is set at 50 iterations. */
  virtual void SetMaximumNumberOfIterations( const unsigned int numberOfIterations ) ITK_OVERRIDE
    {
    itkDebugMacro("setting MaximumNumberOfIterations to " << numberOfIterations);
    if ( this->m_MaximumNumberOfIterations != numberOfIterations )
      {
      this->m_MaximumNumberOfIterations = numberOfIterations;
      this->Modified();
      }
    }

  /** Get the number of iterations of the Iterated Conditional Mode
   * (ICM) algorithm. */
  virtual unsigned int GetMaximumNumberOfIterations() const ITK_OVERRIDE
    {
    return this->m_MaximumNumberOfIterations;
    }

  /** Set the threshold for the object size. */
  itkSetMacro(ClusterSize, unsigned int);

  /** Set the label for the object region. */
  itkSetMacro(ObjectLabel, LabelType);

  /** Extract the input image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  itkSetMacro(StartPoint, IndexType);

  itkSetMacro(BoundaryGradient, unsigned int);

  itkSetMacro(ObjectThreshold, double);

  /** set and get the value for Clique weights */
  itkSetMacro(CliqueWeight_1, double);
  itkGetConstMacro(CliqueWeight_1, double);
  itkSetMacro(CliqueWeight_2, double);
  itkGetConstMacro(CliqueWeight_2, double);
  itkSetMacro(CliqueWeight_3, double);
  itkGetConstMacro(CliqueWeight_3, double);
  itkSetMacro(CliqueWeight_4, double);
  itkGetConstMacro(CliqueWeight_4, double);
  itkSetMacro(CliqueWeight_5, double);
  itkGetConstMacro(CliqueWeight_5, double);
  itkSetMacro(CliqueWeight_6, double);
  itkGetConstMacro(CliqueWeight_6, double);

  /** Specify the type of matrix to use. */
  typedef vnl_matrix< double > MatrixType;

protected:
  RGBGibbsPriorFilter();
  ~RGBGibbsPriorFilter() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void Allocate(); /** allocate memory space for the filter. */

  virtual void MinimizeFunctional() ITK_OVERRIDE;

  virtual void GenerateData() ITK_OVERRIDE;

  virtual void ApplyGibbsLabeller();

  virtual void ApplyGPImageFilter();

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( SameDimension,
                   ( Concept::SameDimension< itkGetStaticConstMacro(InputImageType::ImageDimension),
                                             itkGetStaticConstMacro(ClassifiedImageType::ImageDimension) > ) );
  itkConceptMacro( DimensionShouldBe3,
                   ( Concept::SameDimension< itkGetStaticConstMacro(InputImageType::ImageDimension), 3 > ) );
  // End concept checking
#endif

private:
  RGBGibbsPriorFilter(const Self &);
  void operator=(const Self &);

  typedef typename TInputImage::SizeType InputImageSizeType;

  InputImageConstPointer m_InputImage;                /** the input */
  TrainingImageType      m_TrainingImage;             /** image to train the
                                                        filter. */
  LabelledImageType      m_LabelledImage;             /** output */
  unsigned int           m_NumberOfClasses;           /** the number of class
                                                        need to be classified.
                                                        */
  unsigned int           m_MaximumNumberOfIterations; /** number of the
                                                        iteration. */

  typename ClassifierType::Pointer m_ClassifierPtr;

  unsigned int m_BoundaryGradient; /** the threshold for the existence of a
                                     boundary. */
  double       m_BoundaryWeight;   /** weight for H_1 */
  double       m_GibbsPriorWeight; /** weight for H_2 */
  int          m_StartRadius;      /** define the start region of the object. */
  int          m_RecursiveNumber;  /** number of SA iterations. */
  LabelType *  m_LabelStatus;      /** array for the state of each pixel. */

  InputImagePointer m_MediumImage;    /** the medium image to store intermedium
                                        result */

  unsigned int m_Temp;            /** for SA algo. */
  IndexType    m_StartPoint;      /** the seed of object */

  unsigned int   m_ImageWidth;    /** image size. */
  unsigned int   m_ImageHeight;
  unsigned int   m_ImageDepth;
  unsigned int   m_ClusterSize;  /** region size smaller than the threshold will
                                   be erased. */
  LabelType      m_ObjectLabel;  /** the label for object region. */
  unsigned int   m_VecDim;       /** the channel number in the image. */
  InputPixelType m_LowPoint;     /** the point give lowest value of H-1 in
                                   neighbor. */

  unsigned short *m_Region;      /** for region erase. */
  unsigned short *m_RegionCount; /** for region erase. */

  /** weights for different clique configuration. */
  double m_CliqueWeight_1;  /** weight for cliques that v/h smooth boundayr */
  double m_CliqueWeight_2;  /** weight for clique that has an intermadiate
                              smooth boundary */
  double m_CliqueWeight_3;  /** weight for clique that has a diagonal smooth
                              boundary */
  double m_CliqueWeight_4;  /** weight for clique consists only object pixels */
  double m_CliqueWeight_5;  /** weight for clique consists only background
                              pixels */
  double m_CliqueWeight_6;  /** weight for clique other than these */

  /** calculate H_2. */
  void  GibbsTotalEnergy(int i);

  /** calculate the energy in each cluster. */
  double GibbsEnergy(unsigned int i, unsigned int k, unsigned int k1);

  int Sim(int a, int b);         /** method to return 1 when a equal to b. */

  unsigned int LabelRegion(int i, int l, int change);  /** help to erase the
                                                         small region. */

  void  RegionEraser();                       /** erase the small region. */

  void  GenerateMediumImage();                /** create the intermedium image.
                                                */

  void  GreyScalarBoundary(LabelledImageIndexType Index3D); /** calculate H_1.
                                                              */

  double m_ObjectThreshold;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRGBGibbsPriorFilter.hxx"
#endif
#endif
