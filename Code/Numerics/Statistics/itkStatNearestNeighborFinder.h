/*=========================================================================
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatNearestNeighborFinder.h
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
#ifndef __itkStatNearestNeighborFinder_h
#define __itkStatNearestNeighborFinder_h

#include "itkLightProcessObject.h"
#include "itkStatDistanceFunctions.h"

namespace itk {
  /**
   * \class NearestNeighborFinder
   * \brief Templated K-nearest neighbor search algorithm 
   *
   * This class is templated over the dataset (set of samples)
   * and the data-point (sample) types.
   * Users have to use New() method to create an instance of this class.
   * With the input parameters (number of neighbors - K, pointer to dataset,
   * and pointer to the query point) set by SetInputParams function,
   * it will search k number of nearest neighbors which are determined by 
   * an euclidean distance function. Users can get the vector (std) which
   * has k number of data-point by calling GetOutput() function.
   *
   * NOTE: Current implementaion expects the dataset and data-point types 
   * are containers that support ForwardIterator interface.
   *
   * \sa EuclideanDistance in itkStatDistance.h
   * */

  template<class TDataSet, class TPoint>
  class NearestNeighborFinder: public LightProcessObject
  {
  public:
    /**
     * Save Template parameter TPoint
     */
    typedef TDataSet DataSetType ;

    /**
     * Save Template parameter TPoint
     */
    typedef TPoint PointType ;

    /**
     * return type of the GetOutput() function
     */
    typedef std::vector<PointType> OutputType ;
    
    /** 
     * Standard "Self" typedef.
     */
    typedef NearestNeighborFinder       Self;
    
    /**
     * Standard "Superclass" typedef.
     */
    typedef LightProcessObject  Superclass;

    /** 
     * Smart pointer typedef support. 
     */
    typedef SmartPointer<Self>  Pointer;
    typedef SmartPointer<const Self>  ConstPointer;
    
    /**
     * Method for creation through the object factory.
     */
    itkNewMacro(Self);

    /** 
     * Run-time type information (and related methods).
     */
    itkTypeMacro(NearestNeighborFinder, LightProcessObject);

    /**
     * Error object 
     */
    struct InvalidInputError
    {
      InvalidInputError() {} 
    } ;

    /**
     * Set input paraments
     *
     * User should provides the number of neighbors (K), the pointer to the 
     * dataset (samples), and the pointer to the query point which will be
     * evaluated with every sample in the dataset which is specifed by samples
     * parameter
     *
     * This will throw InvalidInputError (of this class) exception 
     * if the K is smaller than the size of samples or the size of queryPoint 
     * is equal to or less than 0. 
     */
    void SetInputParams(int K, DataSetType* dataSet, PointType* queryPoint) 
      throw (InvalidInputError) ;

    /**
     * Return result
     *
     * No argument required.
     * This will return a vector (std) of data-points which are determined as 
     * K-nearest neighbors.
     *
     * If the parameters haven't been properly set by SetInputParams function,
     * it will throw the InvalidInputError of this class.  
     */
    OutputType GetOutput() throw (InvalidInputError) ;
    
    
  protected:
    /**
     * constructor
     */
    NearestNeighborFinder() ;

    /**
     * struct definition for a neighbor
     *
     * Point stores a data-point (location in dataset)
     * Distance stores a double value which is the euclidean distance
     * between this neighbor and the QueryPoint.
     */
    typedef struct
    {
      PointType Point ;
      double Distance ;
    } NeighborType ;
    
    /**
     * typedef of a vector (std) of the Neighbor struct 
     *
     * This will be used for creation of temporary result of 
     * K-nearest neighbor search
     */
    typedef std::vector<NeighborType> NeighborsType ;

    /**
     * Perform the actual K-nearest neighbor search.
     */
    void GenerateData() ;
    
    /**
     * With the given distance parameter, it will try to find
     * if there is any data-point in the m_Neighbors (temporary result storage)
     * is not closer to the query point than the distance. If any, it will
     * return the iterator of the first such data-point. If not, it will
     * return the iterator of the last element in the M_neighbors.
     * 
     * It is used by GenerateData().
     */
    NeighborsType::iterator FindReplacement(double distance) ;

    /**
     * Extracts only the data-point portion of the final search result from
     * the m_Neighbors and copy them to m_Output which will be used by
     * GetOutput() function.
     *
     * It is used by GenerateData()
     */
    void CopyOutput() ;

    /**
     * Get euclidean between the data-point A and the data-point B.
     */
    double GetDistance(PointType* A, PointType* B) ;
    
  private:
    /**
     * The number of nearest neighbors 
     */
    int m_K ;

    /**
     * Pointer to the query point which will be evaluated 
     * with all data-point in the  dataset (m_DataSet)
     */
    PointType* m_QueryPoint ;

    /**
     * Pointer to the data set
     */
    DataSetType* m_DataSet ;

    /**
     * Temporary storage for the KNN search
     */
    NeighborsType  m_Neighbors ;

    /**
     * Output of the search
     */
    OutputType m_Output ;

    /**
     * flag which indicates the validity of input parameters 
     */
    bool m_ValidInputs ;
    
    /**
     * flag which indicates the status of input paraments
     * If the input parameters has been changed after a search,
     * GetOutput() will perform new search.
     */
    bool m_InputsModified ;
  } ; // end of class
} // end of namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkStatNearestNeighborFinder.txx"
#endif

#endif
