/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetImageFilter.h
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
#ifndef _itkLevelSetImageFilter_h
#define _itkLevelSetImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkLevelSet.h"

namespace itk
{

/** \class LevelSetImageFilter
  * \brief
  *
  * LevelSetImageFilter is the base class for all process objects which evolves
  * an input level set to an output level set according to some set 
  * update equations. This class is templated over the level set type. 
  * Both the input and output level sets are of the same type
  * represented as Images.
  *
  * Typically, level set evolution is a iterative process.
  * This class provides infrastructure for internal iterations. Specifically,
  * this class maintains two buffers to keep intermediate results between
  * iterations. This is a workaround until an IterativeFilter baseclass has
  * been implemented.
  *
  * This class also provides a mechanism for narrowbanding, where only
  * data within a narrow band of interest is processed. 
  *
  * This class is templated over the image type which represents the
  * level set.
  *
  * \ingroup LevelSetSegmentation 
  * \ingroup ImageEnhancement 
  *
  *
  */
template <class TLevelSet>
class ITK_EXPORT LevelSetImageFilter : 
  public ImageToImageFilter<TLevelSet,TLevelSet>
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef LevelSetImageFilter Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef ImageToImageFilter<TLevelSet,TLevelSet> Superclass;

  /**
   * Smart pointer typedef support
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(LevelSetImageFilter, ImageToImageFilter);

  /**
   * LevelSetType typedef support.
   */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::PixelType  PixelType;
  typedef typename LevelSetType::NodeType NodeType;
  typedef typename LevelSetType::NodeContainer NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /**
   * SetDimension enumeration.
   */
  enum { SetDimension = LevelSetType::SetDimension};

  /**
   * Set the evolution time step size. Default is 0.5. The timestep is
   * typically algorithm and application dependent. It should be chosen
   * to meet the CFL condition which requires that no pixels move
   * more than one grid position at each time step.
   */
  itkSetClampMacro( TimeStepSize, double, 0.0, 
    NumericTraits<double>::max());

  /**
   * Get the evolution time step size.
   */
  itkGetMacro( TimeStepSize, double );

  /**
   * Set the narrowbanding flag. If NarrowBanding is turned on, only the
   * pixels in the InputNarrowBand are processed. A NarrowBand is represented
   * as a VectorContainer of LevelSetNodes. NarrowBanding is set to false
   * by default.
   *
   * \sa LevelSetNode
   */
  itkSetMacro( NarrowBanding, bool );
  itkBooleanMacro( NarrowBanding );

  /**
   * Get the narrowbading flag.
   */
  itkGetMacro( NarrowBanding, bool );

  /**
   * Set the narrow bandwidth. Default is 12.
   */
  itkSetClampMacro( NarrowBandwidth, double, 0.0, 
    NumericTraits<double>::max());

  /**
   * Get the narrow bandwidth
   */
  itkGetMacro( NarrowBandwidth, double );

  /**
   * Get the number of pixels in the narrowband.
   */
  int GetNarrowBandSize()
    { 
    if( !m_NarrowBanding ) return 0;
    return m_InputNarrowBand->Size();
    }

  /**
   * Set the input narrowband.
   */
  void SetInputNarrowBand( NodeContainer *ptr );

  /** 
   * Get the input narrowband.
   */
  NodeContainerPointer GetInputNarrowBand( )
    {
    return m_InputNarrowBand;
    }

  /**
   * Set the number of iterations. Default is 10.
   */
  itkSetMacro( NumberOfIterations, unsigned int );

  /**
   * Get the number of iterations.
   */
  itkGetMacro( NumberOfIterations, unsigned int );

protected:
  LevelSetImageFilter();
  ~LevelSetImageFilter(){};
  LevelSetImageFilter(const Self&){};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * Allocate the internal buffers for internal interations.
   */
  virtual void AllocateBuffers(bool outputOnly = false);

  /**
   * Swap the pointers to the two internal pointers. Useful
   * when the output of one iteration is the input to the next
   * iteration.
   */
  virtual void SwapBuffers();

  /**
   * Copy data from the input level set to the internal input
   * buffer.
   */
  virtual void CopyInputToInputBuffer();

  /**
   * Copy data from the internal output buffer to the process
   * buffer.
   */
  virtual void CopyOutputBufferToOutput();

  /**
   * Get a pointer to the internal input buffer.
   */
  LevelSetPointer GetInputBuffer()
    { return m_InputBuffer; }

  /**
   * Get a pointer to the internal output buffer.
   */
  LevelSetPointer GetOutputBuffer()
    { return m_OutputBuffer; }

  /**
   * Generate the output data. Subclasses must provide this method.
   */
  virtual void GenerateData() = 0;

  /**
   * Specify the required input region to satisfiy the output
   * request. The default is to request for the largest
   * possible region for any output. Subclasses should override
   * this method if a different input size is desired.
   *
   * \sa ProcessObject::GenerateInputRequestedRegion()
   */
  virtual void GenerateInputRequestedRegion();

  /**
   * Specify a larger than requested output region. The default
   * is to enlarge the requested region to the largest possible.
   * Subclasses should override this method if a different
   * output region is desired.
   *
   * \sa ProcessObject::EnlargeOutputRequestedRegion();
   */
  virtual void EnlargeOutputRequestedRegion(DataObject * output);

private:
  NodeContainerPointer m_InputNarrowBand;
  double m_TimeStepSize;
  bool   m_NarrowBanding;
  double m_NarrowBandwidth;

  unsigned int m_NumberOfIterations;
  LevelSetPointer m_InputBuffer;
  LevelSetPointer m_OutputBuffer;

};


} //namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetImageFilter.txx"
#endif

#endif
