/*=========================================================================
 *
 *  Copyright NumFOCUS
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
/**
 *\class RGBGibbsPriorFilter
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
 * method while f_2 is minimized by the GibbsTotalEnergy method.
 *
 * This filter only works with 3D images.
 *
 * \ingroup MRFFilters
 * \ingroup ITKMarkovRandomFieldsClassifiers
 */
template <typename TInputImage, typename TClassifiedImage>
class ITK_TEMPLATE_EXPORT RGBGibbsPriorFilter : public MRFImageFilter<TInputImage, TClassifiedImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(RGBGibbsPriorFilter);

  /** Standard "Self" type alias. */
  using Self = RGBGibbsPriorFilter;
  using Superclass = MRFImageFilter<TInputImage, TClassifiedImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(RGBGibbsPriorFilter, MRFImageFilter);

  /** Types from superclass.  */
  using InputImagePixelType = typename Superclass::InputImagePixelType;
  using InputImageRegionConstIterator = typename Superclass::InputImageRegionConstIterator;
  using InputImageRegionIterator = typename Superclass::InputImageRegionIterator;
  using LabelledImageRegionIterator = typename Superclass::LabelledImageRegionIterator;
  using LabelledImagePixelType = typename Superclass::LabelledImagePixelType;
  using IndexValueType = typename Superclass::IndexValueType;

  /** A smart pointer to the input image type. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;

  /** Type definition for the input image pixel type. */
  using InputPixelType = typename TInputImage::PixelType;

  /** Type definitions for the training image. */
  using ClassifiedImageType = TClassifiedImage;
  using TrainingImageType = typename TClassifiedImage::Pointer;

  /** Type definitions for the labelled image.
   *  It is derived from the training image. */
  using LabelledImageType = typename TClassifiedImage::Pointer;

  /** Type definition for the classified image index type. */
  using LabelledImageIndexType = typename TClassifiedImage::IndexType;

  /** Type used as identifier for the Labels
   \warning -1 cannot be used as the identifier for unlabeled pixels
   the NumericTraits<>::max() value is used for indicating unlabeled pixels */
  using LabelType = unsigned int;

  /** Type definitions for classifier to be used for the MRF labeling. */
  using ClassifierType = ImageClassifierBase<TInputImage, TClassifiedImage>;

  /** The type of input pixel. */
  using InputImageVecType = typename TInputImage::PixelType;
  using IndexType = typename TInputImage::IndexType;

  /** Set the image required for training type classifiers. */
  void
  SetTrainingImage(TrainingImageType image);

  /** Set the labelled image. */
  void
  SetLabelledImage(LabelledImageType LabelledImage);

  /** Get the labelled image. */
  LabelledImageType
  GetLabelledImage()
  {
    return m_LabelledImage;
  }

  /** Set the pointer to the classifier being used. */
  void
  SetClassifier(typename ClassifierType::Pointer ptrToClassifier);

  /** Set the Number of classes. */
  void
  SetNumberOfClasses(const unsigned int numberOfClasses) override
  {
    itkDebugMacro("setting NumberOfClasses to " << numberOfClasses);
    if (this->m_NumberOfClasses != numberOfClasses)
    {
      this->m_NumberOfClasses = numberOfClasses;
      this->Modified();
    }
  }

  /** Get the Number of classes. */
  unsigned int
  GetNumberOfClasses() const override
  {
    return this->m_NumberOfClasses;
  }

  /** Set/Get the number of iteration of the Iterated Conditional Mode
   * (ICM) algorithm. A default value is set at 50 iterations. */
  void
  SetMaximumNumberOfIterations(const unsigned int numberOfIterations) override
  {
    itkDebugMacro("setting MaximumNumberOfIterations to " << numberOfIterations);
    if (this->m_MaximumNumberOfIterations != numberOfIterations)
    {
      this->m_MaximumNumberOfIterations = numberOfIterations;
      this->Modified();
    }
  }

  /** Get the number of iterations of the Iterated Conditional Mode
   * (ICM) algorithm. */
  unsigned int
  GetMaximumNumberOfIterations() const override
  {
    return this->m_MaximumNumberOfIterations;
  }

  /** Set the threshold for the object size. */
  itkSetMacro(ClusterSize, unsigned int);

  /** Set the label for the object region. */
  itkSetMacro(ObjectLabel, LabelType);

  /** Extract the input image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

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
  using MatrixType = vnl_matrix<double>;

protected:
  RGBGibbsPriorFilter();
  ~RGBGibbsPriorFilter() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  Allocate(); /** allocate memory space for the filter. */

  void
  MinimizeFunctional() override;

  void
  GenerateData() override;

  virtual void
  ApplyGibbsLabeller();

  virtual void
  ApplyGPImageFilter();

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(
    SameDimension,
    (Concept::SameDimension<Self::InputImageType::ImageDimension, Self::ClassifiedImageType::ImageDimension>));
  itkConceptMacro(DimensionShouldBe3, (Concept::SameDimension<Self::InputImageType::ImageDimension, 3>));
  // End concept checking
#endif

private:
  using InputImageSizeType = typename TInputImage::SizeType;

  InputImageConstPointer m_InputImage;            /** the input */
  TrainingImageType      m_TrainingImage;         /** image to train the
                                                    filter. */
  LabelledImageType m_LabelledImage;              /** output */
  unsigned int      m_NumberOfClasses{ 0 };       /** the number of class
                                                 need to be classified.
                                                 */
  unsigned int m_MaximumNumberOfIterations{ 10 }; /** number of the
                                                iteration. */

  typename ClassifierType::Pointer m_ClassifierPtr;

  unsigned int m_BoundaryGradient{ 7 }; /** the threshold for the existence of a
                                       boundary. */
  double      m_BoundaryWeight{ 1 };    /** weight for H_1 */
  double      m_GibbsPriorWeight{ 1 };  /** weight for H_2 */
  int         m_StartRadius{ 10 };      /** define the start region of the object. */
  int         m_RecursiveNumber{ 0 };   /** number of SA iterations. */
  LabelType * m_LabelStatus{ nullptr }; /** array for the state of each pixel. */

  InputImagePointer m_MediumImage; /** the medium image to store intermedium
                                     result */

  unsigned int m_Temp{ 0 };  /** for SA algo. */
  IndexType    m_StartPoint; /** the seed of object */

  unsigned int m_ImageWidth{ 0 }; /** image size. */
  unsigned int m_ImageHeight{ 0 };
  unsigned int m_ImageDepth{ 0 };
  unsigned int m_ClusterSize{ 10 };  /** region size smaller than the threshold will
                                   be erased. */
  LabelType      m_ObjectLabel{ 1 }; /** the label for object region. */
  unsigned int   m_VecDim{ 0 };      /** the channel number in the image. */
  InputPixelType m_LowPoint;         /** the point give lowest value of H-1 in
                                       neighbor. */

  unsigned short * m_Region{ nullptr };      /** for region erase. */
  unsigned short * m_RegionCount{ nullptr }; /** for region erase. */

  /** weights for different clique configuration. */
  double m_CliqueWeight_1{ 0.0 }; /** weight for cliques that v/h smooth boundary */
  double m_CliqueWeight_2{ 0.0 }; /** weight for clique that has an intermadiate
                               smooth boundary */
  double m_CliqueWeight_3{ 0.0 }; /** weight for clique that has a diagonal smooth
                               boundary */
  double m_CliqueWeight_4{ 0.0 }; /** weight for clique consists only object pixels */
  double m_CliqueWeight_5{ 0.0 }; /** weight for clique consists only background
                               pixels */
  double m_CliqueWeight_6{ 0.0 }; /** weight for clique other than these */

  /** calculate H_2. */
  void
  GibbsTotalEnergy(int i);

  /** calculate the energy in each cluster. */
  double
  GibbsEnergy(unsigned int i, unsigned int k, unsigned int k1);

  int
  Sim(int a, int b); /** method to return 1 when a equal to b. */

  unsigned int
  LabelRegion(int i, int l, int change); /** help to erase the
                                           small region. */

  void
  RegionEraser(); /** erase the small region. */

  void
  GenerateMediumImage(); /** create the intermedium image.
                          */

  void
  GreyScalarBoundary(LabelledImageIndexType Index3D); /** calculate H_1.
                                                       */

  double m_ObjectThreshold{ 5.0 };
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkRGBGibbsPriorFilter.hxx"
#endif
#endif
