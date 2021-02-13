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
#ifndef itkVoronoiSegmentationImageFilterBase_h
#define itkVoronoiSegmentationImageFilterBase_h

#include "itkImageToImageFilter.h"
#include "itkVoronoiDiagram2DGenerator.h"
#include "itkImage.h"

namespace itk
{
/** \class VoronoiSegmentationImageFilterBase
 * \brief Base class for VoronoiSegmentationImageFilter
 *
 * Voronoi SegmentationImageFilter is a class of segmentation algorithms that
 * works on 2D image.
 * Begin with certain number of seeds, VoronoiSegmentationImageFilter
 * first partition the image plane to voronoi regions, and testing each
 * region by some homogeneity operators, which need to be implemented in the private
 * method:
 *      virtual bool TestHomogeneity(IndexList &Plist);
 * after testing, all the regions are classified as either "internal" or "external"
 * region and the "boundary" regions was defined as an "external" region that has at
 * least one "internal" region as its neighbor.
 * the algorithm then added seed points to the "boundary" regions (on the edges) and
 * recursively "split" the boundary region until all the "boundary" become sufficiently
 * small.
 * the output of the segmentation can be either a binary object, which is the collection
 * of all the "internal" region. Or a binary boundary delineate, which is defined as
 * the connected lines between seed points of "boundary" region.
 * This class is a base class for voronoi segmentation, single channel or multiple channel
 * image segmentation can be implemented by deriving imagefilters from this class, by
 * implementing the virtual methods
 *
 * Detailed information about this algorithm can be found in:
 *  " Semi-automated color segmentation of anatomical tissue,"
 *   C. Imelinska, M. Downes, and W. Yuan
 *  Computerized Medical Imaging and Graphics, Vor.24, pp 173-180, 2000.
 *
 * \ingroup HybridSegmentation
 * \ingroup ITKVoronoi
 */
template <typename TInputImage, typename TOutputImage, typename TBinaryPriorImage = Image<unsigned char, 2>>
class ITK_TEMPLATE_EXPORT VoronoiSegmentationImageFilterBase : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VoronoiSegmentationImageFilterBase);

  /** Standard class type aliases. */
  using Self = VoronoiSegmentationImageFilterBase;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VoronoiSegmentationImageFilterBase, ImageToImageFilter);

  /** Get the image dimension from the template parameter. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename TInputImage::Pointer;
  using InputImageConstPointer = typename TInputImage::ConstPointer;
  using IndexType = typename TInputImage::IndexType;
  using SizeType = typename TInputImage::SizeType;
  using RegionType = typename TInputImage::RegionType;
  using PixelType = typename TInputImage::PixelType;

  using OutputImageType = TOutputImage;
  using OutputPixelType = typename TOutputImage::PixelType;

  using VoronoiDiagram = VoronoiDiagram2D<double>;
  using VoronoiDiagramGenerator = VoronoiDiagram2DGenerator<double>;
  using PointType = typename VoronoiDiagram::PointType;
  using CellType = typename VoronoiDiagram::CellType;
  using CellAutoPointer = typename VoronoiDiagram::CellAutoPointer;
  using VoronoiPointer = typename VoronoiDiagram::Pointer;
  using PointIdIterator = typename CellType::PointIdIterator;
  using SeedsType = typename VoronoiDiagram::SeedsType;
  using SeedsIterator = typename VoronoiDiagram::SeedsIterator;
  using NeighborIdIterator = typename VoronoiDiagram::NeighborIdIterator;
  using EdgeIterator = typename VoronoiDiagram::VoronoiEdgeIterator;
  using EdgeInfo = typename VoronoiDiagram::VoronoiEdge;
  using PointTypeVector = std::vector<PointType>;
  using PointTypeDeque = std::deque<PointType>;
  using BinaryObjectImage = TBinaryPriorImage;
  using BinaryObjectImagePointer = typename BinaryObjectImage::Pointer;
  using IndexList = std::vector<IndexType>;

  /** To output the drawing of Voronoi Diagram (VD) . */
  using VDImage = Image<unsigned char, 2>;
  using VDImagePointer = typename VDImage::Pointer;

  /** Set/Get the initial number of seeds for VD. */
  itkSetMacro(NumberOfSeeds, int);
  itkGetConstMacro(NumberOfSeeds, int);

  /** Set/Get the smallest region to be divided. */
  itkSetMacro(MinRegion, SizeValueType);
  itkGetConstMacro(MinRegion, SizeValueType);

  /** Set/Get the number of iterations to run (if set to 0: the classification
   * run process runs until no more cells can be divided). */
  itkSetMacro(Steps, int);
  itkGetConstMacro(Steps, int);

  /** Get the number of seeds before adding new ones. */
  itkGetConstMacro(LastStepSeeds, int);

  /** Get the number of seeds to add. */
  itkGetConstMacro(NumberOfSeedsToAdded, int);

  /**  */
  itkSetMacro(UseBackgroundInAPrior, bool);
  itkGetConstMacro(UseBackgroundInAPrior, bool);

  /** Enable the generation of the output boundary. */
  itkSetMacro(OutputBoundary, bool);
  itkGetConstMacro(OutputBoundary, bool);

  /** Output the segmentation on every iteration.  Useful for interactive
      sessions. The setting of OutputBoundary determines the type of output. */
  itkSetMacro(InteractiveSegmentation, bool);
  itkGetConstMacro(InteractiveSegmentation, bool);
  itkBooleanMacro(InteractiveSegmentation);

  /** Set/Get the mean deviation. */
  itkSetMacro(MeanDeviation, double);
  itkGetConstMacro(MeanDeviation, double);

  /** Set/Get the region size. */
  itkSetMacro(Size, SizeType);
  itkGetConstMacro(Size, SizeType);

  /** Take a prior from other segmentation node. This should be a
   * binary object. */
  virtual void
  TakeAPrior(const BinaryObjectImage *)
  {}

  /** Perform the segmentation. */
  void
  RunSegment();

  /** Perform the segmentation. */
  void
  RunSegmentOneStep();

  /** Create the output binary result for boundaries.  */
  virtual void
  MakeSegmentBoundary();

  virtual void
  MakeSegmentObject();

  /** Return the Voroni Diagram structure. */
  VoronoiPointer
  GetVoronoiDiagram()
  {
    return m_WorkingVD;
  }

#if !defined(ITK_WRAPPING_PARSER) // generates invalid iterator instantiation
                                  // with msvc
  /** Seeds positions are randomly set.
   * If you need to set seeds position then use the SetSeeds method
   * after the InitializeSegment method .  */
  void
  SetSeeds(int num, SeedsIterator begin)
  {
    m_NumberOfSeeds = num;
    m_WorkingVD->SetSeeds(num, begin);
  }

#endif

  /** Seeds positions are randomly set.
   * If you need to set seeds position then use the SetSeeds method
   * after the InitializeSegment method .  */
  void
  SetSeeds(SeedsType & seeds)
  {
    m_NumberOfSeeds = seeds.size();
    auto it = seeds.begin();
    m_WorkingVD->SetSeeds(m_NumberOfSeeds, it);
  }

  /** Get the point specified by the ID given. */
  PointType
  GetSeed(int SeedID)
  {
    return m_WorkingVD->GetSeed(SeedID);
  }

  /** Draw the Voronoi Diagram structure. */
  void
  DrawDiagram(VDImagePointer result, unsigned char incolor, unsigned char outcolor, unsigned char boundcolor);

  void
  BeforeNextStep();

  /** This filter does not stream and needs the entire image as input.
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  void
  GenerateInputRequestedRegion() override;

  /** This filter does not stream and needs to produce the entire output.
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

protected:
  VoronoiSegmentationImageFilterBase();
  ~VoronoiSegmentationImageFilterBase() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override; // general pipeline function.

  SizeType      m_Size;
  int           m_NumberOfSeeds{ 200 };
  SizeValueType m_MinRegion{ 20 };
  int           m_Steps{ 0 };
  int           m_LastStepSeeds{ 0 };
  int           m_NumberOfSeedsToAdded{ 0 };
  int           m_NumberOfBoundary{ 0 };

  std::vector<SizeValueType> m_NumberOfPixels;
  std::vector<unsigned char> m_Label;

  double m_MeanDeviation{ 0.8 };
  bool   m_UseBackgroundInAPrior{ false };
  bool   m_OutputBoundary{ false }; // if =1 then output the boundaries, if = 0 then
                                    // output the object.
  bool m_InteractiveSegmentation{ false };

  typename VoronoiDiagram::Pointer m_WorkingVD;

  typename VoronoiDiagramGenerator::Pointer m_VDGenerator;

  std::vector<PointType> m_SeedsToAdded;

  // private methods:
  // Classify all the voronoi cells as interior , exterior or boundary.
  virtual void
  ClassifyDiagram();

  // Generate the seeds to be added by dividing the boundary cells.
  virtual void
  GenerateAddingSeeds();

  // Compute the statistics of the pixels inside the cell.
  void
  GetPixelIndexFromPolygon(PointTypeDeque vertlist, IndexList * PixelPool);

  virtual bool
  TestHomogeneity(IndexList &)
  {
    return true;
  }

  void
  FillPolygon(PointTypeDeque vertlist, OutputPixelType color = 1);

  // Draw a straight line to the output image.
  void
  drawLine(PointType p1, PointType p2);

  // Draw the intermedia Voronoi Diagram structure.
  void
  drawVDline(VDImagePointer result, PointType p1, PointType p2, unsigned char color);
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVoronoiSegmentationImageFilterBase.hxx"
#endif

#endif
