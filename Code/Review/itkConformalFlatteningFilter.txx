/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkConformalFlatteningFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkConformalFlatteningFilter_txx
#define _itkConformalFlatteningFilter_txx

#include "itkConformalFlatteningFilter.h"
#include "itkExceptionObject.h"
#endif

#ifndef _DISPLAY_DEBUG_INFO_
#define _DISPLAY_DEBUG_INFO_

#include <math.h>


namespace itk
{

  // define a function class derived from vnl_cost_function to compute the 
  // conformal flattening mapping by conjugate gradient method.
  template <class matrixDataType>
  class conformalFlatteningFunc : public vnl_cost_function 
  {
  public:  

    conformalFlatteningFunc(vnl_sparse_matrix<matrixDataType> const& A, 
                            vnl_vector<matrixDataType> const& b);

    double f(vnl_vector<matrixDataType> const& x);
    void gradf(vnl_vector<matrixDataType> const& x, vnl_vector<matrixDataType> & g);

    inline unsigned int dim() {return _dim;}

  private:
    vnl_sparse_matrix<matrixDataType> const* _Asparse;
    vnl_vector<matrixDataType> const* _b;
    unsigned int _dim;
  };


  ////////////////////////////////////////////////////////////////////
  // implementation of class conformalFlatteningFunc

  // overload construction function for sparse matrix A
  template <class matrixDataType>
  conformalFlatteningFunc<matrixDataType>::conformalFlatteningFunc(vnl_sparse_matrix<matrixDataType> const& A, 
                                                                   vnl_vector<matrixDataType> const& b)
    : vnl_cost_function(b.size())
  {
    _Asparse = &A;  // The A in Ax = b;
    _b = &b;  // The b in Ax = b;
    _dim = b.size(); // The dimension, i.e., the number of the unknowns.
    
    if (A.rows() != b.size())
    {
      std::cerr<<"The # of rows in A must be the same as the length of b!"<<std::endl;
        exit(-1);
    } // if (A.rows() != b.size())
  } // conformalFlatteningFunc::conformalFlatteningFunc(A, b)


  template <class matrixDataType>
  double conformalFlatteningFunc<matrixDataType>::f(vnl_vector<matrixDataType> const& x) 
  {  
    matrixDataType r;
      vnl_vector<matrixDataType> tmp;
      _Asparse -> pre_mult(x, tmp);
      r = 0.5*inner_product(tmp,x)-inner_product((*_b),x);

    return r;
  } // conformalFlatteningFunc::f(), the unary function to be optimized.
  // The minimizer of this function \frac{1}{2}x^{T}Ax - b^{T}x, is 
  // the solution of Ax=b when A is positive defined, symmetric.

  template <class matrixDataType>
  void conformalFlatteningFunc<matrixDataType>::gradf(vnl_vector<matrixDataType> const& x, 
                                                      vnl_vector<matrixDataType> & g) 
  { 
      vnl_vector<matrixDataType> tmp;
      _Asparse -> mult(x, tmp);
      g = tmp - (*_b);
   } // conformalFlatteningFunc::gradf(), the gradient of the function to be optimized.

  // implementation of class conformalFlatteningFunc
  ////////////////////////////////////////////////////////////////////

  
  /**
   *
   */
  template <class TInputMesh, class TOutputMesh>
  ConformalFlatteningFilter<TInputMesh,TOutputMesh>
  ::ConformalFlatteningFilter()
  {
    _cellHavePntP = 0; //set the p point of delta function in the 0-th cell
    _mapToSphere = true;
    
    _mapScale = 100; // The largest corrdinates of the furthest point in the plane is _mapScale.
  }


  /**
   *
   */
  template <class TInputMesh, class TOutputMesh>
  void 
  ConformalFlatteningFilter<TInputMesh,TOutputMesh>
  ::PrintSelf(std::ostream& os, Indent indent) const
  {
    Superclass::PrintSelf(os,indent);
  }


  /**
   * This method causes the filter to generate its output.
   */
  template <class TInputMesh, class TOutputMesh>
  void 
  ConformalFlatteningFilter<TInputMesh,TOutputMesh>
  ::GenerateData(void) 
  {
    // The commented out typedef's are not used here. Should they be commented out or still be defined for consistancy?
    // typedef typename TInputMesh::PointsContainer  InputPointsContainer;
    // typedef typename TOutputMesh::PointsContainer OutputPointsContainer;

    //    typedef typename TInputMesh::PointsContainerPointer  InputPointsContainerPointer;
    typedef typename TOutputMesh::PointsContainerPointer OutputPointsContainerPointer;

    InputMeshPointer    inputMesh      =  this->GetInput();
    OutputMeshPointer   outputMesh     =  this->GetOutput();
  
    if( !inputMesh )
      {
        itkExceptionMacro(<<"Missing Input Mesh");
      }

    if( !outputMesh )
      {
        itkExceptionMacro(<<"Missing Output Mesh");
      }

    outputMesh->SetBufferedRegion( outputMesh->GetRequestedRegion() );

    //    InputPointsContainerPointer  inPoints  = inputMesh->GetPoints();
    OutputPointsContainerPointer outPoints = outputMesh->GetPoints();

    outPoints->Reserve( inputMesh->GetNumberOfPoints() );
    outPoints->Squeeze();  // in case the previous mesh had 
    // allocated a larger memory

    //    typename InputPointsContainer::ConstIterator  inputPoint  = inPoints->Begin();
    //    typename OutputPointsContainer::Iterator      outputPoint = outPoints->Begin();

    // Create duplicate references to the rest of data on the mesh

    outputMesh->SetPointData(  inputMesh->GetPointData() );
  
    outputMesh->SetCellLinks(  inputMesh->GetCellLinks() );
  
    outputMesh->SetCells(  inputMesh->GetCells() );
    outputMesh->SetCellData(  inputMesh->GetCellData() );
  
    mapping(inputMesh, outputMesh);
    // The actual conformal flattening mapping process. Everything is done here.
  
    unsigned int maxDimension = TInputMesh::MaxTopologicalDimension;

    for( unsigned int dim = 0; dim < maxDimension; dim++ ) 
    {
      outputMesh->SetBoundaryAssignments( dim,
                                          inputMesh->GetBoundaryAssignments( dim ) );
    }
  }// GenerateData()

  template <class TInputMesh, class TOutputMesh>
  void
  ConformalFlatteningFilter<TInputMesh,TOutputMesh>::
  mapping( InputMeshPointer iMesh, OutputMeshPointer oMesh) 
  {
    // The main function realizing the conformal mapping process.
    // It will call two functions:
    // 1. getDb() function generate the matrics for computating Dx=b
    // 2. solveLinearEq() function use the matrics generated above to 
    // compute the mapping function(complex function) by solving the
    //  linear equation for both real and imaginary parts.
    // With the transform function defined on every points of the mesh,
    // assign the real part as the x coordinate and imaginary part as
    // the y coordinate, the z coordinate being left zero. 
    // That's the plane.
    
    // 3. Then by stereographic projection the plane is mapped to a shpere,
    // by:
    // r := sqrt(x*x + y*y); 
    // x:=2*x/(1+r*r); y:=2*y/(1+r*r); z:=2*r*r/(1+r*r) - 1;
    
    std::cerr<<"Begin mapping......"<<std::endl<<std::endl;

    const unsigned int numberOfPoints = iMesh->GetNumberOfPoints();
    vnl_sparse_matrix<CoordRepType> D(numberOfPoints, numberOfPoints);
    vnl_vector<CoordRepType> bR(numberOfPoints, 0);
    vnl_vector<CoordRepType> bI(numberOfPoints, 0);

    std::cerr<<"Calculating matrix D and vector b..."<<std::endl;
    getDb( iMesh, D , bR, bI);  
    
    std::cerr<<"Solving linear equation Dx=b by Conjugate gradient method(real part) ...";
    vnl_vector<CoordRepType> zR = solveLinearEq(D, bR);
    std::cerr<<"Done!"<<std::endl;
    
    std::cerr<<"Solving linear equation Dx=b by Conjugate gradient method(imaginary part) ...";
    vnl_vector<CoordRepType> zI = solveLinearEq(D, bI);
    std::cerr<<"Done!"<<std::endl;
     
    std::cerr<<"Mapping to a plane or a sphere...";
    stereographicProject(zR, zI, oMesh);
    std::cerr<<"Done!"<<std::endl;
        
    return;
  }//  mapping()

        
  template <class TInputMesh, class TOutputMesh>
  void
  ConformalFlatteningFilter<TInputMesh,TOutputMesh>::
  getDb(OutputMeshPointer mesh, 
        vnl_sparse_matrix<CoordRepType> &D,
        vnl_vector<CoordRepType> &bR,
        vnl_vector<CoordRepType> &bI) 
  {        
  // please refer to the .tex file for how the D and b are calculated.  
    int numOfPoints = mesh->GetNumberOfPoints();
    int numOfCells = mesh->GetNumberOfCells();     

    // 1. store the points coordinates: pointXYZ
    std::vector< std::vector<CoordRepType> > pointXYZ( numOfPoints, std::vector<CoordRepType>(3, 0) );

    PointIterator pntIterator = mesh->GetPoints()->Begin();

    for ( int it = 0; it < numOfPoints; ++it, ++pntIterator) 
    {
      PointType pnt = pntIterator.Value();
    
      pointXYZ[it][0] = pnt[0];
      pointXYZ[it][1] = pnt[1];
      pointXYZ[it][2] = pnt[2];
    } // for it

    // 2. store the relationship from point to cell, i.e. for each
    // point, which cells contain it?  For each point in the mesh,
    // generate a vector, storing the id number of triangles containing
    // this point.  The vectors of all the points form a vector:
    // pointCell

    // 3. store the relationship from cell to point, i.e. for each cell,
    // which points does it contains? store in vector: cellPoint
    std::vector< std::vector<int> > pointCell( numOfPoints );
    std::vector< std::vector<int> > cellPoint( numOfCells, std::vector<int>(3, 0) );

    CellIterator cellIt = mesh->GetCells()->Begin();

    for ( int itCell = 0;
          itCell < numOfCells;
          ++itCell, ++cellIt) 
    {
    
      CellType * cellptr = cellIt.Value(); 
      // cellptr will point to each cell in the mesh
      // std::cout << cellptr->GetNumberOfPoints() << std::endl;

      PointIdIterator pntIdIter = cellptr->PointIdsBegin(); 
      //pntIdIter will point to each point in the current cell
      PointIdIterator pntIdEnd = cellptr->PointIdsEnd();

      for (int itPntInCell = 0; pntIdIter != pntIdEnd; ++pntIdIter, ++itPntInCell)
        {
          pointCell[ *pntIdIter ].push_back(itCell);
          cellPoint[itCell][itPntInCell] = *pntIdIter;
        } // for itPntInCell
    } // for itCell

    std::vector< std::vector<int> >::iterator itPointCell;
    std::vector< std::vector<int> >::iterator itPointCellEnd = pointCell.end();
      
    std::cerr<<"  Checking the existence of boundary...";
    for ( itPointCell = pointCell.begin(); itPointCell != itPointCellEnd; ++itPointCell) 
    {
      if ((*itPointCell).size() < 3) 
      {
        // If one node has two or less neighbors, it's on the boundary.
        // This is the sufficient condition, i.e., it may still be a
        // boundary point even having more than 3 neighbors.
        // So, what's the equivalent expression of being a boundary point?
        std::cerr<<"There is boundary in mesh! exiting..."<<std::endl;
        exit(-1);
      }
    } // for itPointCell
    std::cerr<<"No boundary found!"<<std::endl;

    //   //--------------------------------------------------------------
    //   // print out the result for debuging
    //   std::cout<<std::endl;
    //   std::cout<<std::endl;
    //   std::cout<<std::endl;
    //   std::cout<<std::endl;

    //   for (int it = 0; it < numOfPoints; ++it) {
    //     std::cout<<"point# "<<it<<" :"
    //              <<"       X: "<<pointXYZ[it][0]
    //              <<"       Y: "<<pointXYZ[it][1]
    //              <<"       Z: "<<pointXYZ[it][2]<<std::endl;
    //   }

    //   for (int it = 0; it < numOfPoints; ++it) {
    //     std::cout<<"point# "<<it<<" is contained by    "<<pointCell[it].size()<<"   cells:"<<std::endl;
    //     for (std::vector<int>::const_iterator vi = pointCell[it].begin();
    //          vi != pointCell[it].end();
    //          ++vi) {
    //       std::cout<<*vi<<"     ";      
    //     }
    //     std::cout<<std::endl;
    //   }

    //   for (int it = 0; it < numOfCells; ++it) {
    //     std::cout<<"cell# "<<it<<" has points: "
    //              <<cellPoint[it][0]<<"  "
    //              <<cellPoint[it][1]<<"  "
    //              <<cellPoint[it][2]<<std::endl;
    //   }
    //   //---------------------------------------------------------------


    // 1. Iterate point P from 0 to the last point in the mesh. 
    // 2. For each P, find its neighbors, each neighbor must:
    //    1) has at least two triangles containing P and itself ---not the boundary.
    //    2) has larger pointId, to avoid re-calculation.
    // 3. For each of P's neighbors, Q, calculate R, S
    // 4. Write the value in matrix.
    std::vector< std::vector<int> >::iterator itP, itPEnd = pointCell.end();
    int idP = 0;    
    unsigned long numOfEdges = 0;
    for ( itP = pointCell.begin(); itP != itPEnd; ++itP, ++idP) 
    {
      std::vector<int> neighborOfP;
      // for each point P, traverse all cells containing it.
      std::vector<int>::iterator itCell = (*itP).begin();
      std::vector<int>::iterator itCellEnd = (*itP).end();

      for (; itCell != itCellEnd; ++itCell) 
      {
        // for each cell containing P, store the point with larger point Id.
        // only three points, don't use for-loop to save time.
        if ( cellPoint[*itCell][0] > idP ) 
          neighborOfP.push_back(cellPoint[*itCell][0]);
        if ( cellPoint[*itCell][1] > idP ) 
          neighborOfP.push_back(cellPoint[*itCell][1]);
        if ( cellPoint[*itCell][2] > idP ) 
          neighborOfP.push_back(cellPoint[*itCell][2]);
      }// for itCell. Ok, now all neighbors of P is stored in neighborOfP;
    
      sort(neighborOfP.begin(), neighborOfP.end());
      std::vector<int>::iterator it;
      it = unique(neighborOfP.begin(), neighborOfP.end());
      neighborOfP.erase(it, neighborOfP.end());
      
      numOfEdges += neighborOfP.size();

      //-----------------------------------------------
      // print out the neighbors
      //     std::vector<int>::iterator itNeighbor = neighborOfP.begin();
      //    std::vector<int>::iterator itNeighborEnd = neighborOfP.end();
      //     std::cerr<<"The neighbors of "<<idP<<" are: ";
      //     for (; itNeighbor != itNeighborEnd; ++itNeighbor) {
      //       std::cerr<<*itNeighbor<<" , ";
      //     }
      //     std::cerr<<std::endl;
      // ----------------------------------------------------

      // next, from P to each neighbor...
      // note: itP and itQ point at different type of vectors...
      // *itP is a vector containing a list of cell Ids, all of which contains point P
      // idP is the point Id of P
      // *itQ is the point Id of Q (so idP and *itQ are same type)
      std::vector<int>::iterator itQ, itQEnd = neighborOfP.end();
      for ( itQ = neighborOfP.begin(); itQ != itQEnd; ++itQ) 
      {
        // first check whether PQ is a boundary edge:
        std::vector<int> cellsContainingP(*itP), cellsContainingQ(pointCell[*itQ]);
        std::vector<int> cells(cellsContainingP.size() + cellsContainingQ.size());
        std::vector<int>::iterator itv, endIter;

        sort(cellsContainingP.begin(), cellsContainingP.end());
        sort(cellsContainingQ.begin(), cellsContainingQ.end());

        endIter = set_intersection(cellsContainingP.begin(), cellsContainingP.end(),
                                   cellsContainingQ.begin(), cellsContainingQ.end(),
                                   cells.begin());
        cells.erase(endIter, cells.end());
        if (cells.size() != 2) continue;
        // If P and Q are not shared by two triangles, i.e. 1: are not
        // connected by and edge, or, 2: are on the surface boundary
        // thus only shared by one triangle. then skip.  However, in
        // this paper the surface is closed thus there is not boundary.

        // If passed test above, then P and Q are two valid points.
        // i.e. PQ is a valid edge.  i.e. cells now contain two int's,
        // which are the Id of the triangles containing P and Q


        //       //------------------------------------------------------------
        //       //print out valid edge
        //       std::cerr<<idP<<" and "<<*itQ<<" are two valid points"<<std::endl;
        //       std::cerr<<(endIter == cells.end())<<std::endl;
        //       //-----------------------------------------------------------


        // Next we extract R and S from cells
        int itS, itR; // the Id of point S and R;
        for (int it = 0; it < 3; ++it) 
        {
          if (cellPoint[cells[0]][it] != idP && cellPoint[cells[0]][it] != *itQ) 
            itS = cellPoint[cells[0]][it];
          if (cellPoint[cells[1]][it] != idP && cellPoint[cells[1]][it] != *itQ) 
            itR = cellPoint[cells[1]][it];
        }

        std::vector< CoordRepType > P(pointXYZ[idP]), 
          Q(pointXYZ[*itQ]), 
          R(pointXYZ[itR]),
          S(pointXYZ[itS]);

        std::vector< CoordRepType > SP(3), SQ(3), RP(3), RQ(3);
        double SPnorm = 0, SQnorm = 0, RPnorm = 0, RQnorm = 0, SPSQinnerProd = 0, RPRQinnerProd = 0;
        for (int it = 0; it<3; ++it) 
        {
          SP[it] = P[it] - S[it]; SPnorm += SP[it]*SP[it];
          SQ[it] = Q[it] - S[it]; SQnorm += SQ[it]*SQ[it]; SPSQinnerProd += SP[it]*SQ[it];
          RP[it] = P[it] - R[it]; RPnorm += RP[it]*RP[it];
          RQ[it] = Q[it] - R[it]; RQnorm += RQ[it]*RQ[it]; RPRQinnerProd += RP[it]*RQ[it];
        } //it
        SPnorm = sqrt(SPnorm);
        SQnorm = sqrt(SQnorm);
        RPnorm = sqrt(RPnorm);
        RQnorm = sqrt(RQnorm);

        double cosS = SPSQinnerProd / (SPnorm * SQnorm); 
        double cosR = RPRQinnerProd / (RPnorm * RQnorm);
        double ctgS = cosS/sqrt(1-cosS*cosS), ctgR = cosR/sqrt(1-cosR*cosR);

        D(idP, *itQ) = -0.5*(ctgS + ctgR);
        D(idP, idP) += 0.5*(ctgS + ctgR); 
        // add to the diagonal element of this line.

        D(*itQ, idP) = -0.5*(ctgS + ctgR); // symmetric
        D(*itQ, *itQ) += 0.5*(ctgS + ctgR); 
        // add to the diagonal element of this line.
      } // itQ
    } // itP  
    
    
    
    ////////////////////////////////////////////////////////
    // calculate Euler Number to test whether the mesh is genus 0. i.e. Euler Num is 2;
////    std::cout<<"Total number of edges: "<<numOfEdges<<std::endl;
    int eulerNum = numOfPoints - numOfEdges + numOfCells;
    
    std::cerr<<"  Calculating Euler characteristics......"<<std::endl;
    std::cerr<<"    Euler Characteristics = "<<eulerNum<<std::endl;
    std::cerr<<"    genus = "<<(2.0 - eulerNum)/2<<std::endl;
    
    if (eulerNum != 2) 
    {
      std::cerr<<"    Euler characteristics is "<<eulerNum<<", not 2! Not genus 0 surface."<<std::endl<<"exiting..."<<std::endl;
        exit(-1);
    } // if eulerNum

    // compute b = bR + i*bI separately
    std::vector<CoordRepType> A( pointXYZ[ cellPoint[ _cellHavePntP ][ 0 ] ] ), 
      B( pointXYZ[ cellPoint[ _cellHavePntP ][ 1 ] ] ), 
      C( pointXYZ[ cellPoint[ _cellHavePntP ][ 2 ] ] );
    double ABnorm, CA_BAip; // the inner product of vector C-A and B-A;
    ABnorm = (A[0] - B[0]) * (A[0] - B[0])
      + (A[1] - B[1]) * (A[1] - B[1])
      + (A[2] - B[2]) * (A[2] - B[2]);

    CA_BAip = (C[0] - A[0]) * (B[0] - A[0])
      + (C[1] - A[1]) * (B[1] - A[1])
      + (C[2] - A[2]) * (B[2] - A[2]);

    double theta = CA_BAip / ABnorm; 
    // Here ABnorm is actually the square of AB's norm, which is what we
    // want. So don't bother square the square root.
  
    ABnorm = sqrt(ABnorm); // This is real norm of vector AB.

    std::vector<double> E(3);
    for (int it = 0; it < 3; ++it) 
      E[it] = A[it] + theta*(B[it] - A[it]);

    double CEnorm;
    CEnorm = (C[0] - E[0]) * (C[0] - E[0])
      + (C[1] - E[1]) * (C[1] - E[1])
      + (C[2] - E[2]) * (C[2] - E[2]);
    CEnorm = sqrt(CEnorm); // This is real norm of vector CE.

    bR(cellPoint[ _cellHavePntP ][0]) = -1 / ABnorm;
    bR(cellPoint[ _cellHavePntP ][1]) = 1 / ABnorm;

    bI(cellPoint[ _cellHavePntP ][0]) = (1-theta)/ CEnorm;
    bI(cellPoint[ _cellHavePntP ][1]) = theta/ CEnorm;
    bI(cellPoint[ _cellHavePntP ][2]) = -1 / CEnorm;
  
    return; 
  } //getDb()


  template <class TInputMesh, class TOutputMesh>
  typename ConformalFlatteningFilter<TInputMesh,TOutputMesh>::Tvnl_vector
  ConformalFlatteningFilter<TInputMesh,TOutputMesh>
  ::solveLinearEq(vnl_sparse_matrix<CoordRepType> const& A, 
                  vnl_vector<CoordRepType> const& b) 
  {
    // Solve the linear system Ax=b using the Conjugate Gradient method. 
    // So it requires that the matrix A be symmetric and
    // positive defined. In many cases of the numerical computation for
    // the solution of partial differential equations, those properties of
    // A hold. 
    // However, the symmetry and positive define properties are
    // not checked within this but left for the user. Basically, this
    // class optimizes the function y=\frac{1}{2}(x^T)*A*x - (b^T)*x.

    // The above function is defined by the class conformalFlatteningFunc
    // which is derived from the vnl_cost_function.
    conformalFlatteningFunc<CoordRepType> f(A, b);

    vnl_conjugate_gradient cg(f);
    vnl_vector<double> x(f.dim(), 0);
    cg.minimize(x);

    if( typeid(TInputMesh::CoordRepType) == typeid(double) )
      {
      return *(reinterpret_cast< vnl_vector<CoordRepType> * >(&x));
      }
    else
      {
      vnl_vector<CoordRepType> y(f.dim(), 0);
      for (unsigned int i = 0; i < f.dim(); i++) { y[i] = x[i]; }
      return y;
      }
  }// solveLinearEq()

  template <class TInputMesh,class TOutputMesh>
  void
  ConformalFlatteningFilter<TInputMesh,TOutputMesh>
  ::stereographicProject( vnl_vector<CoordRepType> const& zR,
                          vnl_vector<CoordRepType> const& zI,
                          OutputMeshPointer oMesh) 
  {
                            
    CoordRepType xmin = zR(0), xmax = zR(0), ymin = zI(0) , ymax = zI(0);
                            
    const unsigned int numberOfPoints = oMesh->GetNumberOfPoints();
    
    for (int it = 0; it < numberOfPoints;  ++it) 
    {      
      xmin = (xmin<zR(it))?xmin:zR(it);
      xmax = (xmax>zR(it))?xmax:zR(it);
      ymin = (ymin<zI(it))?ymin:zI(it);
      ymax = (ymax>zI(it))?ymax:zI(it);                
    } // for it    
//    std::cout<<"The max X in plane: "<<xmax<<std::endl;
//    std::cout<<"The min X in plane: "<<xmin<<std::endl;
//    std::cout<<"The max Y in plane: "<<ymax<<std::endl;
//    std::cout<<"The min Y in plane: "<<ymin<<std::endl;

    CoordRepType temp1 = ( fabs(xmin)>fabs(xmax) )?fabs(xmin):fabs(xmax); 
    CoordRepType temp2 = ( fabs(ymin)>fabs(ymax) )?fabs(ymin):fabs(ymax);
//    std::cout<<std::max( temp1, temp2 )<<std::endl;
    CoordRepType factor = _mapScale/( ( temp1>temp2 )?temp1:temp2 );

    // the factor is used to re-scale the points in the plane.
    
    
    std::vector<double> x(numberOfPoints), y(numberOfPoints), z(numberOfPoints);
    std::vector<double>::iterator
      itX = x.begin(), 
      itY = y.begin(), 
      itZ = z.begin(), 
      itXend = x.end();
  if (_mapToSphere == true) 
  {
    for (int it = 0; itX != itXend; ++itX, ++itY, ++itZ, ++it) 
    {            
      double r2 = factor*zR(it)*factor*zR(it) + factor*zI(it)*factor*zI(it);
      *itX = 2*factor*zR(it)/(1+r2);
      *itY = 2*factor*zI(it)/(1+r2);
      *itZ = 2*r2/(1+r2) - 1;

      CoordRepType apoint[3] = {*itX, *itY, *itZ};
      //CoordRepType apoint[3] = {zR(it), zI(it), 0}; // map to a plane

      oMesh->SetPoint( it,typename TOutputMesh::PointType( apoint ));
    } // for it
  } // if (_mapToSphere == 1)
  else 
  {
    for (int it = 0; it < numberOfPoints;  ++it) 
    {
              
      CoordRepType apoint[3] = {zR(it), zI(it), 0}; // map to a plane

      oMesh->SetPoint( it,typename TOutputMesh::PointType( apoint ));
    } // for it    
  } //else of if (_mapToSphere == 1)  
}//stereographicProject

template <class TInputMesh, class TOutputMesh>
void 
ConformalFlatteningFilter<TInputMesh,TOutputMesh>::setPointP( int p )
  {
    if (p >= 0 && p < this->GetInput()->GetNumberOfCells() )
    {
      _cellHavePntP = p;
    }
    else
    {
      std::cerr<<"Location of point p exceeds number of cells. Set to default value 0."<<std::endl<<std::endl;
      _cellHavePntP = 0;
    }
  }

template <class TInputMesh, class TOutputMesh>
void 
ConformalFlatteningFilter<TInputMesh,TOutputMesh>::setScale( double scale )
  {
    if (scale > 0)
    {
      _mapScale = scale;
    }
    else
    {
      std::cerr<<"Scale should be larger than 0. Set to 100."<<std::endl<<std::endl;
      _mapScale = 100;
    }
  }

template <class TInputMesh, class TOutputMesh>
void 
ConformalFlatteningFilter<TInputMesh,TOutputMesh>::mapToSphere( void )
  {
    _mapToSphere = true;
  }
  
template <class TInputMesh, class TOutputMesh>
void 
ConformalFlatteningFilter<TInputMesh,TOutputMesh>::mapToPlane( void )
  {
    _mapToSphere = false;
  }

} // end namespace itk



#endif

