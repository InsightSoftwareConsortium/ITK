/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiSegmentationImageFilterBase.h
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
#ifndef _itkVoronoiSegmentationImageFilterBase_h
#define _itkVoronoiSegmentationImageFilterBase_h

#include "itkImageToImageFilter.h"
#include "itkVoronoiDiagram2D.h"
#include "itkVoronoiDiagram2DGenerator.h"
#include "itkImage.h"

namespace itk
{

/** \class VoronoiSegmentationImageFilterBase
 * /breif Base class for VoronoiSegmentationImageFilter
 * Voronoi SegmentationImageFilter is a class of segmenation algorithms that
 * works on 2D image.
 * Begin with certain number of seeds, VoronoiSegmentationImageFilter
 * first partition the image plane to voronoi regions, and testing each
 * region by some homogeneity operators, which need to be implemented in the private
 * method: 
 *      virtual bool TestHomogeneity(IndexList Plist);
 * after testing, all the regions are classified as either "internal" or "external"
 * region and the "boundary" regions was defined as an "external" region that has at 
 * least one "internal" region as its neighbor.
 * the algorithm then added seed points to the "boundary" regions (on the edges) and
 * recursively "split" the boundary region until all the "boundary" become sufficiently
 * small.
 * the output of the segmentation can be either a binary object, which is the collection
 * of all the "internal" region. Or a binary boundary delineate, which is defined as
 * the connected lines between seed points of "boundary" region.
 * This class is a base class for voronoi segmenation, single channel or multiple channel
 * image segmenation can be implemented by deriving imagefilters from this class, by 
 * implementing the virtual methods.
 * 
 * Detail information about this algorithm can be found in:
 *  " Semi-automated color segmentation of anatomical tissue,"
 *   C. Imelinska, M. Downes, and W. Yuan  
 *  Computerized Medical Imaging and Graphics, Vor.24, pp 173-180, 2000.
 *
 * \ingroup HybridSegmentation 
 */
template <class TInputImage, class TOutputImage>
class VoronoiSegmentationImageFilterBase:
public ImageToImageFilter<TInputImage,TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef VoronoiSegmentationImageFilterBase       Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>   Superclass;
  typedef SmartPointer <Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VoronoiSegmentationImageFilterBase,ImageToImageFilter);

  /** Get the image dimension from the template parameter. */
  enum {ImageDimension = TInputImage::ImageDimension };
  
  /** Convenient typedefs. */
  typedef TInputImage InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename TInputImage::IndexType IndexType;
  typedef typename TInputImage::SizeType SizeType;
  typedef typename TInputImage::RegionType RegionType;
  typedef typename TInputImage::PixelType PixelType;
  typedef VoronoiDiagram2D<double> VoronoiDiagram;
  typedef VoronoiDiagram2DGenerator<double> VoronoiDiagramGenerator;
  typedef typename VoronoiDiagram::PointType PointType;
  typedef typename VoronoiDiagram::Cell Cell;
  typedef typename VoronoiDiagram::CellPointer CellPointer;
  typedef typename VoronoiDiagram::Pointer VoronoiPointer;
  typedef typename Cell::PointIdIterator PointIdIterator;
  typedef typename VoronoiDiagram::SeedsType SeedsType;
  typedef typename VoronoiDiagram::SeedsIterator SeedsIterator;
  typedef typename VoronoiDiagram::NeighborIdIterator NeighborIdIterator;
  typedef typename VoronoiDiagram::VoronoiEdgeIterator EdgeIterator;
  typedef typename VoronoiDiagram::VoronoiEdge EdgeInfo;
  typedef std::vector<PointType> PointTypeVector;
  typedef std::deque<PointType> PointTypeDeque;
  typedef itk::Image<bool,2>  BinaryObjectImage;
  typedef typename BinaryObjectImage::Pointer  BinaryObjectImagePointer;
  typedef std::vector<IndexType> IndexList;

  /** For output the drawing of Voronoi Diagram. */ 
  typedef itk::Image<unsigned char,2>  VDImage; 
  typedef typename VDImage::Pointer  VDImagePointer; 
    
  /** Set/Get the initial Number of Seeds for VD. */
  itkSetMacro(NumberOfSeeds, int);
  itkGetMacro(NumberOfSeeds, int);

  /** Set/Get the smallest region to be divided. */
  itkSetMacro(MinRegion, int);
  itkGetMacro(MinRegion, int);

  /** Set/Get the number of iterations to run (0: run until no more to
   *  divide); */
  itkSetMacro(Steps, int);
  itkGetMacro(Steps, int);

  /** Get the number of Seeds before adding new seeds. */
  itkGetMacro(LastStepSeeds, int);

  /** Huh? bad english */
  itkGetMacro(NumberOfSeedsToAdded, int); 

  /** Set/Get the */
  itkSetMacro(UseBackgroundInAPrior, bool);
  itkGetMacro(UseBackgroundInAPrior, bool);

  /** Enable the generation of the output boundary. */
  itkSetMacro(OutputBoundary, bool);
  itkGetMacro(OutputBoundary, bool);

  /** Set/Get the mean deviation. */
  itkSetMacro(MeanDeviation, double);
  itkGetMacro(MeanDeviation, double);

  /** Take a prior from other segmentation node, should be an
   * binary object. */
  virtual void TakeAPrior(BinaryObjectImage* aprior){};
  
  /** Perform the segmentation. */
  void RunSegment(void);

  /** Perform the segmentation. */
  void RunSegmentOneStep(void);

  /** Make the output binary result as boundary.  */
  void MakeSegmentBoundary(void);
  void MakeSegmentObject(void);

  /** Return the Voroni diagram. */
  VoronoiPointer GetVoronoiDiagram(void)
    { return m_WorkingVD; }
    
  /** Normally not used, the seeds are set randomly. 
   * in case that need set customized seeds: 
   * use SetSeeds methods after InitializeSegment.  */ 
  void SetSeeds(int num, SeedsIterator begin)
    { 
    m_NumberOfSeeds = num; 
    m_WorkingVD->SetSeeds(num,begin); 
    }; 
    
  /** Get the point specified by the ID given. */
  PointType GetSeed(int SeedID)
    { return m_WorkingVD->GetSeed(SeedID); } 
      
  /** \todo Document */
  void DrawDiagram(VDImagePointer result,unsigned char incolor, 
                   unsigned char outcolor,unsigned char boundcolor); 
  void BeforeNextStep(void); 
  virtual void Reset(void){}; 

  /** This filter does not stream and needs the entire image as input
   * \sa ProcessObject::GenerateInputRequestedRegion(). */
  virtual void GenerateInputRequestedRegion();
  
  /** This filter does not stream and needs to produce the entire output.
   * \sa ProcessObject::EnlargeOutputRequestedRegion() */
  virtual void EnlargeOutputRequestedRegion(DataObject *output);  

protected:
  VoronoiSegmentationImageFilterBase();
  ~VoronoiSegmentationImageFilterBase();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  void GenerateData(void); //general pipeline function.

  SizeType m_Size;
  int m_NumberOfSeeds;
  int m_MinRegion;
  int m_Steps;
  int m_LastStepSeeds;
  int m_NumberOfSeedsToAdded;
  int m_NumberOfBoundary;
  std::vector<int> m_NumberOfPixels;
  std::vector<unsigned char> m_Label;
  int m_StepsRuned;
  double m_MeanDeviation;
  bool m_UseBackgroundInAPrior;
  bool m_OutputBoundary; //1: output boundary, 0: output object.

  typename VoronoiDiagram::Pointer m_WorkingVD;
  typename VoronoiDiagramGenerator::Pointer m_VDGenerator;

  std::vector<PointType> m_SeedsToAdded;

  // private methods:
  // classify all the voronoi cells as interior or exterior or boundary
  void ClassifyDiagram(void);
  
  // generate the seeds to be added. (by divide the boundary cells)
  void GenerateAddingSeeds(void);
  
  // compute the statistics of the pixels inside the polygon.
  void GetPixelIndexFromPolygon(PointTypeDeque VertList, IndexList *PixelPool);
  virtual bool TestHomogeneity(IndexList Plist){return 1;};
  
  void FillPolygon(PointTypeDeque vertlist);
  
  // draw a straight line to the output image.
  void drawLine(PointType p1,PointType p2);
  
  //used for drawing the intermedia Voronoi Diagram. 
  void drawVDline(VDImagePointer result,PointType p1,PointType p2, unsigned char color);
  
private:
  VoronoiSegmentationImageFilterBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};

}//end namespace


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVoronoiSegmentationImageFilterBase.txx"
#endif

#endif




