/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMarchingImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFastMarchingImageFilter_h
#define _itkFastMarchingImageFilter_h

#include "itkImageToImageFilter.h"
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
 * initial position on the front, fast marching systematically moves the
 * front forward one grid point at a time.
 *
 * Updates are preformed using an entropy satisfy scheme where only
 * "upwind" neighborhoods are used. This implementation of Fast Marching
 * uses a std::priority_queue to locate the next proper grid position to
 * update. 
 * 
 * Fast Marching sweeps through N grid points in (N log N) steps to obtain
 * the arrival time value as the front propagates through the grid.
 *
 * Implementation of this class is based on Chapter 8 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * This class is templated over the level set image type and the speed
 * image type. The initial front is specified by two containers: one
 * containing the known points and one containing the trial
 * points. The speed function can be specified as a speed image or a
 * speed constant. The speed image is set using the method
 * SetInput(). If the speed image is NULL, a constant speed function
 * is used and is specified using method the SetSpeedConstant().
 *
 * If the speed function is constant and of value one, fast marching results
 * in an approximate distance function from the initial alive points.
 * FastMarchingImageFilter is used in the ReinitializeLevelSetImageFilter
 * object to create a signed distance function from the zero level set.
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
 * \ingroup LevelSetSegmentation 
 */
template <
  class TLevelSet, 
  class TSpeedImage = Image<float,::itk::GetImageDimension<TLevelSet>::ImageDimension> >
class ITK_EXPORT FastMarchingImageFilter :
    public ImageToImageFilter<TSpeedImage,TLevelSet>
{
public:
  /** Standard class typdedefs. */
  typedef FastMarchingImageFilter Self;
  typedef ImageSource<TLevelSet> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FastMarchingImageFilter, ImageSource);

  /** Typedef support of level set method types. */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::PixelType  PixelType;
  typedef typename LevelSetType::NodeType NodeType;
  typedef typename LevelSetType::NodeContainer NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /** Dimension of the level set. */
  itkStaticConstMacro(SetDimension, unsigned int,
                      LevelSetType::SetDimension);

  /** Index typedef support. */
  typedef Index<itkGetStaticConstMacro(SetDimension)> IndexType;

  /** SpeedImage typedef support. */
  typedef TSpeedImage SpeedImageType;

  /** SpeedImagePointer typedef support. */
  typedef typename SpeedImageType::Pointer        SpeedImagePointer;
  typedef typename SpeedImageType::ConstPointer   SpeedImageConstPointer;

  /** Enum of Fast Marching algorithm point types. FarPoints represent far
   * away points; TrialPoints represent points within a narrowband of the
   * propagating front; and AlivePoints represent points which have already
   * been processed. */
  enum LabelType { FarPoint, AlivePoint, TrialPoint };

  /** LabelImage typedef support. */
  typedef Image<unsigned char, itkGetStaticConstMacro(SetDimension)> LabelImageType;

  /** LabelImagePointer typedef support. */
  typedef typename LabelImageType::Pointer LabelImagePointer;

  /** Set the container of Alive Points representing the initial front.
   * Alive points are represented as a VectorContainer of LevelSetNodes. */
  void SetAlivePoints( NodeContainer * points )
  { 
    m_AlivePoints = points; 
    this->Modified(); 
  };

  /** Get the container of Alive Points representing the initial front. */
  NodeContainerPointer GetAlivePoints( )
  { return m_AlivePoints; };

  /** Set the container of Trial Points representing the initial front.
   * Trial points are represented as a VectorContainer of LevelSetNodes. */
  void SetTrialPoints( NodeContainer * points )
  { 
    m_TrialPoints = points;
    this->Modified();
  };

  /** Get the container of Trial Points representing the initial front. */
  NodeContainerPointer GetTrialPoints( )
  { return m_TrialPoints; };

  /** Get the point type label image. */
  LabelImagePointer GetLabelImage() const
  { return m_LabelImage; };

  /** Set the Speed Constant. If the Speed Image is NULL,
   * the SpeedConstant value is used for the whole level set.
   * By default, the SpeedConstant is set to 1.0. */
  void SetSpeedConstant( double value )
  {
    m_SpeedConstant = value;
    m_InverseSpeed = -1.0 * vnl_math_sqr( 1.0 / m_SpeedConstant );
    this->Modified();
  }

  /** Get the Speed Constant. */
  itkGetConstMacro( SpeedConstant, double );

  /** Set/Get the Normalization Factor for the Speed Image.
      The values in the Speed Image is divided by this
      factor. This allows the use of images with
      integer pixel types to represent the speed. */
  itkSetMacro( NormalizationFactor, double );
  itkGetMacro( NormalizationFactor, double );

  /** Set the Fast Marching algorithm Stopping Value. The Fast Marching
   * algorithm is terminated when the value of the smallest trial point
   * is greater than the stopping value. */
  itkSetMacro( StoppingValue, double );

  /** Get the Fast Marching algorithm Stopping Value. */
  itkGetConstMacro( StoppingValue, double );

  /** Set the Collect Points flag. Instrument the algorithm to collect
   * a container of all nodes which it has visited. Useful for
   * creating Narrowbands for level set algorithms that supports 
   * narrow banding. */
  itkSetMacro( CollectPoints, bool );

  /** Get thConste Collect Points flag. */
  itkGetConstMacro( CollectPoints, bool );
  itkBooleanMacro( CollectPoints );

  /** Get the container of Processed Points. If the CollectPoints flag
   * is set, the algorithm collects a container of all processed nodes.
   * This is useful for defining creating Narrowbands for level
   * set algorithms that supports narrow banding. */
  NodeContainerPointer GetProcessedPoints() const
  { return m_ProcessedPoints; }

  /** Set the output level set size. Defines the size of the output
   * level set. */
  void SetOutputSize( const typename LevelSetImageType::SizeType& size )
  { m_OutputSize = size; }

  /** Get the output level set size. */
  const typename LevelSetImageType::SizeType & GetOutputSize() const
  { return m_OutputSize; }

protected:
  FastMarchingImageFilter();
  ~FastMarchingImageFilter(){};
  void PrintSelf( std::ostream& os, Indent indent ) const;

  virtual void Initialize( LevelSetImageType * );
  virtual void UpdateNeighbors( const IndexType& index, 
                                const SpeedImageType *, LevelSetImageType * );
  virtual double UpdateValue( const IndexType& index, 
                              const SpeedImageType *, LevelSetImageType * );


  const NodeType& GetNodeUsedInCalculation(unsigned int idx) const
  { return m_NodesUsed[idx]; }

  void GenerateData();

  /** Generate the output image meta information. */
  virtual void GenerateOutputInformation();
  virtual void EnlargeOutputRequestedRegion(DataObject *output);

  /** Get Large Value. This value is used to 
      represent the concept of infinity for the time assigned to pixels that
      have not been visited. This value is set by default to half the 
      max() of the pixel type used to represent the time-crossing map. */
  itkGetConstMacro( LargeValue, PixelType );


private:
  FastMarchingImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  NodeContainerPointer                          m_AlivePoints;
  NodeContainerPointer                          m_TrialPoints;

  LabelImagePointer                             m_LabelImage;
  
  double                                        m_SpeedConstant;
  double                                        m_InverseSpeed;
  double                                        m_StoppingValue;
    
  bool                                          m_CollectPoints;
  NodeContainerPointer                          m_ProcessedPoints;

  typename LevelSetImageType::SizeType          m_OutputSize;
  typename LevelSetImageType::PixelType         m_LargeValue;
  NodeType                                      m_NodesUsed[SetDimension];

  /** Trial points are stored in a min-heap. This allow efficient access
   * to the trial point with minimum value which is the next grid point
   * the algorithm processes. */
  typedef std::vector<NodeType> HeapContainer;
  typedef std::greater<NodeType> NodeComparer;
  typedef std::priority_queue< NodeType, HeapContainer, NodeComparer > HeapType;

  HeapType    m_TrialHeap;

  double    m_NormalizationFactor;
};

} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkFastMarchingImageFilter.txx"
#endif

#endif
