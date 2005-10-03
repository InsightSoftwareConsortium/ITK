/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRandomNonRepeatingConstIteratorWithIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

       This software is distributed WITHOUT ANY WARRANTY; without even 
       the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
       PURPOSE.  See the above copyright notices for more information.

  =========================================================================*/
#ifndef __itkImageRandomNonRepeatingConstIteratorWithIndex_h
#define __itkImageRandomNonRepeatingConstIteratorWithIndex_h

#include "itkImageConstIteratorWithIndex.h"
#include "itkImage.h"
#include <algorithm>
#include <iostream>
#include "itkMersenneTwisterRandomVariateGenerator.h"


namespace itk
{


  /* 
     The itk::ImageRandomNonRepeatingIterator works by creating a random
     permutation of the image pixels and then using that to control the
     order in which it accesses them.  The classes nodeOfPermutation and
     randomPermutation are used to support that.  randomPermutation is
     basically container which holds nodeOfPermutation objects.  The
     node class overloads the < operator, which allows the sort algorithm
     from the STL to be used on it.
  */
  class nodeOfPermutation
  {
   public:
    unsigned long priority;
    unsigned long index;
    double value;
    nodeOfPermutation ()
    {
      priority=0;
      index=0;
      value=0.0;
    }
    bool operator<( const nodeOfPermutation& b) const
    {
      if(priority==b.priority)
        {
        return value < b.value;
        }
      else
        {
        return priority<b.priority;
        }
    }
  };
  class randomPermutation
  {
   public:
    nodeOfPermutation * permutation;
    Statistics::MersenneTwisterRandomVariateGenerator::Pointer m_Generator;
    unsigned long size;
    randomPermutation(unsigned long sz)
    {
      size=sz;
      permutation=new nodeOfPermutation[size];
      m_Generator = Statistics::MersenneTwisterRandomVariateGenerator::New();
      this->Shuffle();
    }
    void Dump()
    {
      for(unsigned int i=0;i<size;i++)
        {
        std::cout<<permutation[i].value<<" "<<permutation[i].priority
                 <<" "<<permutation[i].index<<";";
        std::cout<<std::endl;
        }
    }
    void SetPriority(unsigned long i,unsigned long priority)
    {
      if(i>size)
        {
        std::cerr<<"Error - i dont have "<<i<<" elements"<<std::endl;
        }
      else
        {
        permutation[i].priority=priority;
        }
    }
    void Shuffle()
    {
      for(unsigned int i=0;i<size;i++)
        {
        permutation[i].value= m_Generator->GetVariateWithClosedRange ( 1.0 );
        permutation[i].index=i;
        }
      std::sort(permutation,permutation+size);
    }
    unsigned long operator[](unsigned long i)
    {
      return permutation[i].index;
    }
    ~randomPermutation()
    {
      delete [] permutation;
    }
    
    /** Reinitialize the seed of the random number generator */
    void ReinitializeSeed()
    {
      m_Generator->Initialize();
    }
    
    void ReinitializeSeed(int seed)
    {
      m_Generator->Initialize ( seed );
    }
  };


  /** \class ImageRandomNonRepeatingConstIteratorWithIndex
   * \brief A multi-dimensional image iterator that visits a random set of pixels
   * within an image region.  All pixels in the image will be visited before any
   * are repeated.  A priority image may be passed to the interator which
   * will cause it to select certain sets of pixels (those with lower priority 
   * values) before others.
   *
   *  This class was contributed by Rupert Brooks, McGill Centre for Intelligent
   *  Machines, Montreal, Canada.  It is heavily based on the  
   *  ImageRandomIterator class. 
   *
   * ImageRandomNonRepeatingConstIteratorWithIndex is a multi-dimensional 
   * iterator class that
   * is templated over image type.  ImageRandomNonRepeatingConstIteratorWithIndex
   * is constrained to walk only within the specified region. When first 
   * instantiated, it creates (and stores) a random permutation of the image
   * pixels.  It then visits each pixel in the order specified by the 
   * permutation.  Thus, iterator++ followed by iterator-- will end up leaving
   * the iterator pointing at the same pixel.  Furthermore, iterating from
   * beginning to end will cover each pixel in the region exactly once.
   *
   * This iterator can be passed an image the same size as the region, which 
   * specifies a priority for the pixels.  Within areas of this priority image
   * that have the same value, the pixel selection will be random.  Otherwise
   * the pixel selection will be in the order of the priority image.  In the
   * extreme, this allows the order of the pixel selection to be completely 
   * specified.
   *
   * ImageRandomNonRepeatingConstIteratorWithIndex assumes a particular layout 
   * of the image data. The is arranged in a 1D array as if it were 
   * [][][][slice][row][col] with
   * Index[0] = col, Index[1] = row, Index[2] = slice, etc.
   *
   *
   * \par MORE INFORMATION
   * For a complete description of the ITK Image Iterators and their API, please
   * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
   * is available in print and as a free .pdf download from http://www.itk.org.
   *
   * \author Rupert Brooks, McGill Centre for Intelligent Machines. Canada
   *
   * \ingroup ImageIterators
   *
   * \sa ImageConstIterator \sa ConditionalConstIterator
   * \sa ConstNeighborhoodIterator \sa ConstShapedNeighborhoodIterator
   * \sa ConstSliceIterator  \sa CorrespondenceDataStructureIterator 
   * \sa FloodFilledFunctionConditionalConstIterator 
   * \sa FloodFilledImageFunctionConditionalConstIterator 
   * \sa FloodFilledImageFunctionConditionalIterator 
   * \sa FloodFilledSpatialFunctionConditionalConstIterator 
   * \sa FloodFilledSpatialFunctionConditionalIterator 
   * \sa ImageConstIterator \sa ImageConstIteratorWithIndex 
   * \sa ImageIterator \sa ImageIteratorWithIndex
   * \sa ImageLinearConstIteratorWithIndex  \sa ImageLinearIteratorWithIndex 
   * \sa ImageRandomNonRepeatingConstIteratorWithIndex  \sa ImageRandomIteratorWithIndex 
   * \sa ImageRegionConstIterator \sa ImageRegionConstIteratorWithIndex 
   * \sa ImageRegionExclusionConstIteratorWithIndex 
   * \sa ImageRegionExclusionIteratorWithIndex 
   * \sa ImageRegionIterator  \sa ImageRegionIteratorWithIndex 
   * \sa ImageRegionReverseConstIterator  \sa ImageRegionReverseIterator 
   * \sa ImageReverseConstIterator  \sa ImageReverseIterator 
   * \sa ImageSliceConstIteratorWithIndex  \sa ImageSliceIteratorWithIndex 
   * \sa NeighborhoodIterator \sa PathConstIterator  \sa PathIterator 
   * \sa ShapedNeighborhoodIterator  \sa SliceIterator 
   * \sa ImageConstIteratorWithIndex
   *
   */
  template<typename TImage>
  class ITK_EXPORT ImageRandomNonRepeatingConstIteratorWithIndex : public ImageConstIteratorWithIndex<TImage>
  {
   public:
    /** Standard class typedefs. */
    typedef ImageRandomNonRepeatingConstIteratorWithIndex Self;
    typedef ImageConstIteratorWithIndex<TImage>  Superclass;
    
    /** Index typedef support. While this was already typdef'ed in the superclass
     * it needs to be redone here for this subclass to compile properly with gcc.
     * Note that we have to rescope Index back to itk::Index to that is it not
     * confused with ImageIterator::Index. */
    typedef typename TImage::IndexType   IndexType;

    /** Region typedef support. While this was already typdef'ed in the superclass
     * it needs to be redone here for this subclass to compile properly with gcc.
     * Note that we have to rescope Region back to itk::ImageRegion so that is
     * it not confused with ImageIterator::Index. */
    typedef typename TImage::RegionType RegionType;
    
    /** Image typedef support. While this was already typdef'ed in the superclass
     * it needs to be redone here for this subclass to compile properly with gcc.
     * Note that we have to rescope Index back to itk::Index to that is it not
     * confused with ImageIterator::Index. */
    typedef TImage ImageType;

    /** PixelContainer typedef support. Used to refer to the container for
     * the pixel data. While this was already typdef'ed in the superclass
     * it needs to be redone here for this subclass to compile properly with gcc. */
    typedef typename TImage::PixelContainer PixelContainer;
    typedef typename PixelContainer::Pointer PixelContainerPointer;

    /** Default constructor. Needed since we provide a cast constructor. */
    ImageRandomNonRepeatingConstIteratorWithIndex();
    ~ImageRandomNonRepeatingConstIteratorWithIndex() { if( m_Permutation ) delete m_Permutation; };
    
    /** Constructor establishes an iterator to walk a particular image and a
     * particular region of that image. */
    ImageRandomNonRepeatingConstIteratorWithIndex(const ImageType *ptr, const RegionType& region);

    /** Constructor that can be used to cast from an ImageIterator to an
     * ImageRandomNonRepeatingConstIteratorWithIndex. Many routines return an ImageIterator but for a
     * particular task, you may want an ImageRandomNonRepeatingConstIteratorWithIndex.  Rather than
     * provide overloaded APIs that return different types of Iterators, itk
     * returns ImageIterators and uses constructors to cast from an
     * ImageIterator to a ImageRandomNonRepeatingConstIteratorWithIndex. */
    ImageRandomNonRepeatingConstIteratorWithIndex( const ImageConstIteratorWithIndex<TImage> &it)
    { 
      this->ImageConstIteratorWithIndex<TImage>::operator=(it); 
      m_Permutation = NULL;
    }
    /** Move an iterator to the beginning of the region. */
    void GoToBegin(void)
    {
      m_NumberOfSamplesDone = 0L;
      this->UpdatePosition();
    }

    /** Move an iterator to one position past the End of the region. */
    void GoToEnd(void)
    {
      m_NumberOfSamplesDone = m_NumberOfSamplesRequested;
      this->UpdatePosition();
    }

    /** Is the iterator at the beginning of the region? */
    bool IsAtBegin(void) const
    {
      return (m_NumberOfSamplesDone == 0L);
    }

    /** Is the iterator at the end of the region? */
    bool IsAtEnd(void) const
    { 
      return (m_NumberOfSamplesDone >= m_NumberOfSamplesRequested);  
    }


    /** The moving image dimension. */
    itkStaticConstMacro( ImageDimension, unsigned int, 
                  ::itk::GetImageDimension< TImage >::ImageDimension );
   

    /** Image with priorities */
    typedef itk::Image< unsigned long, 
                        itkGetStaticConstMacro(ImageDimension )  
                                                >  PriorityImageType;
    
    /** Set the priority image.  The priority image controls the order
        of the random selection.  Pixels of the same priority will be
        ordered randomly, but pixels of lower priority value will be
        selected first.
     **/
    void SetPriorityImage(const PriorityImageType * priorityImage);

    /** Increment (prefix) the selected dimension.
     * No bounds checking is performed. \sa GetIndex \sa operator-- */
    Self & operator++()
    {
      m_NumberOfSamplesDone++;
      this->UpdatePosition();
      return *this;
    }

    /** Decrement (prefix) the selected dimension.
     * No bounds checking is performed. \sa GetIndex \sa operator++ */
    Self & operator--()
    {
      m_NumberOfSamplesDone--;
      this->UpdatePosition();
      return *this;
    }
  
    /** Set/Get number of random samples to get from the image region */
    void SetNumberOfSamples( unsigned long number );
    unsigned long GetNumberOfSamples( void ) const;
    
    /** Reinitialize the seed of the random number generator  */
    void ReinitializeSeed();
    /** Reinitialize the seed of the random number generator with 
        a specific value 
    */
    void ReinitializeSeed(int);

   private:
    void UpdatePosition();
    unsigned long  m_NumberOfSamplesRequested;
    unsigned long  m_NumberOfSamplesDone;
    unsigned long  m_NumberOfPixelsInRegion;
    randomPermutation * m_Permutation;
  };
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRandomNonRepeatingConstIteratorWithIndex.txx"
#endif

#endif 



