  /*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMarchingImageFilter.h
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
#ifndef _itkFastMarchingImageFilter_h
#define _itkFastMarchingImageFilter_h

#include "itkImageSource.h"
#include "itkLevelSet.h"
#include "itkIndex.h"
#include "vnl/vnl_math.h"

#include <functional>
#include <queue>

namespace itk
{

/** \class FastMarchingImageFilter
 * \brief Solve an Eikonal equation using Fast Marching
 *
 * Fast marching solves an Eikonal equation where the speed is always
 * non-negative and depends on the position only. Starting from an 
 * initial position on the front, fast marching systematically move the
 * front forward one grid point at a time.
 *
 * Updates are preformed using an entropy satisfiy scheme where only
 * "upwind" neighborhoods are used. This implementation of Fast Marching
 * uses a std::priority_queue to locate the next proper grid position to
 * update. 
 * 
 * Fast Marching sweeps through N grid points in (N log N) steps to obtain
 * the arrival time value as the front propagate through the grid.
 *
 * Implementation of this class is based on Chapter 8 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * This class is templated over the level set image type and the speed image
 * image type. The initial front is specified by two containers: one containing
 * the known points and one containing the trial points. The speed function
 * can be specified as a speed image or a speed constant.
 *
 * If the speed function is constant and of value one, fast marching
 * results in a approximate distance function from the initial alive points.
 * FastMarchingImageFilter is used in the ReinitializeLevelSetImageFilter object to create
 * a signed distance function from the zero level set.
 *
 * The algorithm can be terminated early by setting an appropriate stopping
 * value. The algorithm terminates when the current arrival time being
 * processed is greater than the stopping value.
 *
 * Possible Improvements:
 * In the current implemenation, std::priority_queue only allows 
 * taking nodes out from the front and putting nodes in from the back.
 * To update a value already on the heap, a new node is added to the heap.
 * The defunct old node is left on the heap. When it is removed from the
 * top, it will be recognized as invalid and not used.
 * Future implementations can implement the heap in a different way
 * allowing the values to be updated. This will generally require 
 * some sift-up and sift-down functions and  
 * an image of back-pointers going from the image to heap in order
 * to locate the node which is to be updated.
 *
 * \sa LevelSetTypeDefault
 */
template <
  class TLevelSet, 
  class TSpeedImage = Image<float,TLevelSet::ImageDimension>
>
class ITK_EXPORT FastMarchingImageFilter :
  public ImageSource<TLevelSet>
{
public:

  /** 
   * Standard "Self" typdedef
   */
  typedef FastMarchingImageFilter Self;

  /**
   * Standard "Superclass" typedef
   */ 
  typedef ImageSource<TLevelSet> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(FastMarchingImageFilter, ImageSource);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * Typedef support of level set method types
   */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::PixelType  PixelType;
  typedef typename LevelSetType::NodeType NodeType;
  typedef typename LevelSetType::NodeContainer NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /**
   * Dimension of the level set.
   */
  enum { SetDimension = LevelSetType::SetDimension};

  /**
   * Index typedef support.
   */
  typedef Index<SetDimension> IndexType;

  /**
   * SpeedImage typedef support.
   */
  typedef TSpeedImage SpeedImageType;

  /**
   * SpeedImagePointer typedef support.
   */
  typedef typename SpeedImageType::Pointer SpeedImagePointer;

  /**
   * Enum of Fast Marching algorithm point types. FarPoints represent far away points;
   * TrialPoints represent points within a narrowband of the propagating front; and
   * AlivePoints represent points which have already been processed.
   */
  enum LabelType { FarPoint, AlivePoint, TrialPoint };

  /**
   * LabelImage typedef support.
   */
  typedef Image<unsigned char, SetDimension> LabelImageType;

  /**
   * LabelImagePointer typedef support.
   */
  typedef typename LabelImageType::Pointer LabelImagePointer;

  /**
   * Set the container of Alive Points representing the initial front.
   * Alive points are represented as a VectorContainer of LevelSetNodes.
   */
  void SetAlivePoints( NodeContainer * points )
    { 
      m_AlivePoints = points; 
      this->Modified(); 
    };

  /**
   * Get the container of Alive Points representing the initial front.
   */
  NodeContainerPointer GetAlivePoints( )
    { return m_AlivePoints; };

  /**
   * Set the container of Trial Points representing the initial front.
   * Trial points are represented as a VectorContainer of LevelSetNodes.
   */
  void SetTrialPoints( NodeContainer * points )
    { 
      m_TrialPoints = points;
      this->Modified();
    };

  /**
   * Get the container of Trial Points representing the initial front.
   */
  NodeContainerPointer GetTrialPoints( )
    { return m_TrialPoints; };

  /**
   * Set the input Speed Image. If the Speed Image is NULL, 
   * the SpeedConstant value is used for the whole level set.
   */
  void SetSpeedImage( SpeedImageType * ptr );

  /**
   * Get the input Speed Image.
   */
  SpeedImagePointer GetSpeedImage() const
    { return m_SpeedImage; };

  /** 
   * Get the point type label image.
   */
  LabelImagePointer GetLabelImage() const
    { return m_LabelImage; };

  /**
   * Set the Speed Constant. If the Speed Image is NULL,
   * the SpeedConstant value is used for the whole level set.
   * By default, the SpeedConstant is set to 1.0.
   */
  void SetSpeedConstant( double value )
    {
    m_SpeedConstant = value;
    m_InverseSpeed = -1.0 * vnl_math_sqr( 1.0 / m_SpeedConstant );
    this->Modified();
    }

  /**
   * Get the Speed Constant.
   */
  itkGetMacro( SpeedConstant, double );

  /**
   * Set the Fast Marching algorithm Stopping Value. The Fast Marching
   * algorithm is terminated when the value of the smallest trial point
   * is greater than the stopping value.
   */
  itkSetMacro( StoppingValue, double );

  /**
   * Get the Fast Marching algorithm Stopping Value.
   */
  itkGetMacro( StoppingValue, double );

  /**
   * Set the Collect Points flag. Instrument the algorithm to collect
   * a container of all nodes which it has visited. Useful for
   * creating Narrowbands for level set algorithms that supports 
   * narrow banding.
   */
  itkSetMacro( CollectPoints, bool );

  /**
   * Get the Collect Points flag
   */
  itkGetMacro( CollectPoints, double );
  itkBooleanMacro( CollectPoints );

  /**
   * Get the container of Processed Points. If the CollectPoints flag
   * is set, the algorithm collects a container of all processed nodes.
   * This is useful for defining creating Narrowbands for level
   * set algorithms that supports narrow banding.
   */
  NodeContainerPointer GetProcessedPoints() const
    { return m_ProcessedPoints; }

  /**
   * Set the output level set size. Defines the size of the output
   * level set.
   */
  void SetOutputSize( const typename LevelSetImageType::SizeType& size )
    { m_OutputSize = size; }

  /**
   * Set the debugging mode
   */
  itkSetMacro( DebugOn, bool );

protected:
  FastMarchingImageFilter();
  ~FastMarchingImageFilter(){};
  FastMarchingImageFilter( const Self& ) {};
  void operator= ( const Self& ) {};
  void PrintSelf( std::ostream& os, Indent indent );

  virtual void Initialize();
  virtual void UpdateNeighbors( IndexType& index );
  virtual double UpdateValue( IndexType& index );

  const typename LevelSetImageType::SizeType& GetOutputSize() const
    { return m_OutputSize; }

  typename LevelSetImageType::ScalarValueType GetLargeValue() const
    { return m_LargeValue; }

  const NodeType& GetNodeUsedInCalculation(unsigned int idx) const
    { return m_NodesUsed[idx]; }

  void GenerateData();
  virtual void GenerateOutputInformation();
  virtual void EnlargeOutputRequestedRegion(DataObject *output);

private:
  NodeContainerPointer                          m_AlivePoints;
  NodeContainerPointer                          m_TrialPoints;

  LabelImagePointer                             m_LabelImage;
  
  SpeedImagePointer                             m_SpeedImage;
  double                                        m_SpeedConstant;
  double                                        m_InverseSpeed;
  double                                        m_StoppingValue;
    
  LevelSetPointer                               m_OutputLevelSet;

  bool                                          m_CollectPoints;
  NodeContainerPointer                          m_ProcessedPoints;

  typename LevelSetImageType::SizeType          m_OutputSize;

  typename LevelSetImageType::ScalarValueType   m_LargeValue;

  std::vector<NodeType>                         m_NodesUsed;

  /**
   * Trial points are stored in a min-heap. This allow efficient access
   * to the trial point with minimum value which is the next grid point
   * the algorithm processes.
   */
  typedef std::vector<NodeType> HeapContainer;
  typedef std::greater<NodeType> NodeComparer;
  typedef std::priority_queue< NodeType, HeapContainer, NodeComparer > HeapType;

  HeapType    m_TrialHeap;

  // temporary debugging flag
  bool m_DebugOn;

};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFastMarchingImageFilter.txx"
#endif

#endif
