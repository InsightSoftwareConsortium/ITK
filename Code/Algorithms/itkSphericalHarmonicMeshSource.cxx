/*=========================================================================

  Author: Christine Xu

=========================================================================*/


#include "itkTriangleCell.h"

#include "itkSphericalHarmonicMeshSource.h"

#include <math.h>
#include <malloc.h>

#include <iostream>

#define X .525731112119133606
#define Z .850650808352039932

#ifndef M_PI
#define M_PI 3.1415926535897932
#endif
#ifndef M_PI_2
#define M_PI_2 1.5707963267948966
#endif

//Vertices, triangles, edges of a single icosahedron
static double vert[12][3] = {
   {-X, 0.0, Z}, {X, 0.0, Z}, {-X, 0.0, -Z}, {X, 0.0, -Z},
   {0.0, Z, X}, {0.0, Z, -X}, {0.0, -Z, X}, {0.0, -Z, -X},
   {Z, X, 0.0}, {-Z, X, 0.0}, {Z, -X, 0.0}, {-Z, -X, 0.0}
};
static int triang[20][3] = {
   {0,4,1}, {0,9,4}, {9,5,4}, {4,5,8}, {4,8,1},
   {8,10,1}, {8,3,10}, {5,3,8}, {5,2,3}, {2,7,3},
   {7,10,3}, {7,6,10}, {7,11,6}, {11,0,6}, {0,1,6},
   {6,1,10}, {9,0,11}, {9,11,2}, {9,2,5}, {7,2,11}
};
static int edge[30][2] = {
   {0,1}, {0,4}, {0,6}, {0,9}, {0,11}, {1,4}, {1,6}, {1,8}, {1,10}, {2,3},
   {2,5}, {2,7}, {2,9}, {2,11}, {3,5}, {3,7}, {3,8}, {3,10}, {4,5}, {4,8},
   {4,9}, {5,8}, {5,9}, {6,7}, {6,10}, {6,11}, {7,10}, {7,11}, {8,10}, {9,11}
};

namespace itk 
{ 

typedef TriangleCell<SphericalHarmonicMeshSource::CellType>      TriangleType;

SphericalHarmonicMeshSource::SphericalHarmonicMeshSource()
{
  m_Dimension = 3;
  
  m_Level = 20;
  m_Degree = 0;
  m_FromL = 0;
  m_ToL = 0;
}

SphericalHarmonicMeshSource::~SphericalHarmonicMeshSource()
{

}

void SphericalHarmonicMeshSource::SetDegree(unsigned int d)
{
  if(m_Coefs.empty() || d > floor(sqrt(m_Coefs.size()))-1)
    throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "The maximum degree of the coefficient exceeds the size of coefficient list.");
  m_Degree = d; 
  m_ToL = d;
}

void SphericalHarmonicMeshSource::SetCoefs(SphericalHarmonicMeshSource::CoefListType& coeflist)
{
  m_Coefs = coeflist; 
  m_Degree =  (int) floor(sqrt(coeflist.size()))-1;
  m_ToL  = m_Degree;
  //std::cout<<"m_Degree = "<<m_Degree <<std::endl;
}

void SphericalHarmonicMeshSource::GenerateData()
{
  if(m_Coefs.empty())
    throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "Coefficients mustn't be empty.");
  if(m_FromL > m_ToL)
    throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "The starting degree should be smaller or equal to the ending degree.");
  if(m_ToL > m_Degree)
    throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "The evalueated degree mustn't exceed the size of the coefficients.");
  if(m_Level <= 0)
    throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, "Subdivision level must be greater than zero.");
  
  int n_phi, n_theta;
  int n_vert;
  int n_triag;
  
  //Calculate n_vertex, n_phi, n_theta, n_triag
  unsigned int n=0;
  if(m_Level > 2) 
  {
    for(unsigned int i=1; i<(m_Level-1); i++) 
    n += i;
  }
  n_vert = 12 + (m_Level - 1)*30 + n*20;
  n_theta = (int) ceil(sqrt(((double) (n_vert))/2.0));
  n_phi = 2*n_theta;
  int numtriags = 0;
  if(m_Level == 1)
  {
    numtriags = 20;
  }
  else if(m_Level > 1)
  {
    n = 1;
    do
    {
      for(unsigned int m=1; m<=n; m++)
      {
        numtriags = numtriags + 3;
        if(m != n)
          numtriags = numtriags + 3;
      
      }
      n++;
    }while(n<=m_Level);
    numtriags = numtriags * 20;
    numtriags = numtriags / 3;
  }
  n_triag = numtriags;
  
  //Allocate datas
  Point3* all_vert = new Point3[n_vert];
  Point3* all_triangs = new Point3[n_triag*3];//all possible vertices in triangs
  double* icos = new double[n_vert*2];
  int * triangs = new int[3*n_triag];
  Point3* mesh = new Point3[n_phi*n_theta];
  Point3* vertex = new Point3[n_vert];
  
  //Set up icosahedral triangles on the sphere, all_vert & triangs
  set_up_icosahedron_triangs(all_vert, all_triangs, m_Level, n_vert, n_phi, n_theta, icos, n_triag, triangs);
  
  SphericalHarmonicPolynomial<3> SPHARM;
  try
  {
    SPHARM.SetCoefs(m_Coefs);
    SPHARM.SetDegree(m_Degree);
  
    int phi, theta;
    //Calculate mesh on grids of the sphere
    for (theta= 0; theta < n_theta; theta++)
      for (phi= 0; phi < n_phi; phi++)
        SPHARM.Evaluate(m_FromL, m_ToL, 2.0*M_PI*phi/n_phi, M_PI * (theta+0.5) / n_theta, mesh[phi+n_phi*theta]);
  }
  catch(SphericalHarmonicPolynomialException ex)
  {
    throw SphericalHarmonicPolynomialException(__FILE__, __LINE__, ex.GetDescription());

  }
  //for(int i=0; i<n_phi*n_theta; i++)
  //    std::cout<<mesh[i][0]<<","<<mesh[i][1]<<","<<mesh[i][2]<<std::endl;
  
  //interpolation of the real vertice phi and theta
  interpol_vert(n_phi, n_theta, mesh, n_vert, icos, vertex);
  
  //for(int i=0; i<n_vert; i++)
  //  std::cout<<vertex[i][0]<<","<<vertex[i][1]<<","<<vertex[i][2]<<std::endl;
  
  //Construct the mesh in itk format
  MeshType::Pointer outputMesh = this->GetOutput();
   
  PointsContainerPointer points = PointsContainer::New();
  
  for(int i=0; i<n_vert; i++)
  {
    points->InsertElement(i, PointType(vertex[i]));
  }
  outputMesh->SetPoints(points);
  
  /** 
   * Specify the method used for allocating cells
   */
  outputMesh->SetCellsAllocationMethod( MeshType::CellsAllocatedDynamicallyCellByCell );
  
  for(int i=0; i<n_triag; i++)
  {
    CellType::CellAutoPointer cellpointer;
    cellpointer.TakeOwnership(new TriangleType);
    /**
    * Assign the points to the tetrahedron through their identifiers.
    */
    unsigned long triPoints[3];
    triPoints[0] = triangs[3*i];
    triPoints[1] = triangs[3*i+1];
    triPoints[2] = triangs[3*i+2];
    cellpointer->SetPointIds(triPoints);
    
    outputMesh->SetCell(i,cellpointer);
    
  }
  
  delete[] all_vert;
  delete[] all_triangs;
  delete[] icos;
  delete[] triangs;
  delete[] mesh;
  delete[] vertex;
  
}

void SphericalHarmonicMeshSource::set_up_icosahedron_triangs(Point3* all_vert,
        Point3* all_triangs,
        int subdiv,
        int n_vert,
        int n_phi,
        int n_theta,
        double *icos,
        int n_triangs,
        int *triangs)
{
   int i, n, m, k, numtriags;
   double x1, x2, y1, y2, z1, z2, x3, y3, z3; 
   double dx12, dy12, dz12, dx23, dy23, dz23;
   double length;   
   
   double epsilon = 0.00001;//machine epsilon??
   
   memcpy(all_vert, vert, 12*sizeof(Point3));
   
   //std::cout<<"after memcpy"<<std::endl;
   
   k=12;
   for(i=0; i<30; i++) 
   {
      x1 = vert[edge[i][0] ][0];
      y1 = vert[edge[i][0] ][1];
      z1 = vert[edge[i][0] ][2];
      x2 = vert[edge[i][1] ][0];
      y2 = vert[edge[i][1] ][1];
      z2 = vert[edge[i][1] ][2];
      dx12 = (x2 - x1)/subdiv;
      dy12 = (y2 - y1)/subdiv;
      dz12 = (z2 - z1)/subdiv;
      for(n=1; n<subdiv; n++) 
      {
         all_vert[k][0] = x1 + n*dx12;
         all_vert[k][1] = y1 + n*dy12;
         all_vert[k][2] = z1 + n*dz12;
         length = sqrt(all_vert[k][0]*all_vert[k][0]+
           all_vert[k][1]*all_vert[k][1]+
           all_vert[k][2]*all_vert[k][2]);
         all_vert[k][0] /= length;
         all_vert[k][1] /= length;
         all_vert[k][2] /= length;
         k++;
      }
   }

   if(subdiv > 2) 
   {
      for(i=0; i<20; i++) 
      {
         x1 = vert[triang[i][0] ][0];
         y1 = vert[triang[i][0] ][1];
         z1 = vert[triang[i][0] ][2];
         x2 = vert[triang[i][1] ][0];
         y2 = vert[triang[i][1] ][1];
         z2 = vert[triang[i][1] ][2];
         x3 = vert[triang[i][2] ][0];
         y3 = vert[triang[i][2] ][1];
         z3 = vert[triang[i][2] ][2];
         dx12 = (x2 - x1)/subdiv;
         dy12 = (y2 - y1)/subdiv;
         dz12 = (z2 - z1)/subdiv;
         dx23 = (x3 - x2)/subdiv;
         dy23 = (y3 - y2)/subdiv;
         dz23 = (z3 - z2)/subdiv;

         n = 1;
         do 
         {
            for(m=1; m<=n; m++) 
            {
               all_vert[k][0] = x1 + (n+1)*dx12 + m*dx23;
               all_vert[k][1] = y1 + (n+1)*dy12 + m*dy23;
               all_vert[k][2] = z1 + (n+1)*dz12 + m*dz23;
               length = sqrt(all_vert[k][0]*all_vert[k][0]+
                 all_vert[k][1]*all_vert[k][1]+
                 all_vert[k][2]*all_vert[k][2]);
               all_vert[k][0] /= length;
               all_vert[k][1] /= length;
               all_vert[k][2] /= length;
               k++;
            }
            n++;
         }while( n<=(subdiv-2) );
      }
   }
   numtriags=0;
   
   //std::cout<<"before get triangulation"<<std::endl;   
   //std::cout<<n_triangs<<std::endl;
   
   // get triangulation
   if (subdiv > 1) 
   {
      for(i=0; i<20; i++) 
      {
         x1 = vert[triang[i][0] ][0];
         y1 = vert[triang[i][0] ][1];
         z1 = vert[triang[i][0] ][2];
         x2 = vert[triang[i][1] ][0];
         y2 = vert[triang[i][1] ][1];
         z2 = vert[triang[i][1] ][2];
         x3 = vert[triang[i][2] ][0];
         y3 = vert[triang[i][2] ][1];
         z3 = vert[triang[i][2] ][2];
         dx12 = (x2 - x1)/subdiv;
         dy12 = (y2 - y1)/subdiv;
         dz12 = (z2 - z1)/subdiv;
         dx23 = (x3 - x2)/subdiv;
         dy23 = (y3 - y2)/subdiv;
         dz23 = (z3 - z2)/subdiv;

         n = 1;
         do 
         {
            for(m=1; m<=n; m++) 
            {
              // Draw lower triangle
              all_triangs[numtriags][0] = x1 + n*dx12 + m*dx23;
              all_triangs[numtriags][1] = y1 + n*dy12 + m*dy23;
              all_triangs[numtriags][2] = z1 + n*dz12 + m*dz23;
              length = sqrt(all_triangs[numtriags][0]*all_triangs[numtriags][0]+
                all_triangs[numtriags][1]*all_triangs[numtriags][1]+
                all_triangs[numtriags][2]*all_triangs[numtriags][2]);
              all_triangs[numtriags][0] /= length;
              all_triangs[numtriags][1] /= length;
              all_triangs[numtriags][2] /= length;
              numtriags++;
              all_triangs[numtriags][0] = x1 + (n-1)*dx12 + (m-1)*dx23;
              all_triangs[numtriags][1] = y1 + (n-1)*dy12 + (m-1)*dy23;
              all_triangs[numtriags][2] = z1 + (n-1)*dz12 + (m-1)*dz23;
              length = sqrt(all_triangs[numtriags][0]*all_triangs[numtriags][0]+
                all_triangs[numtriags][1]*all_triangs[numtriags][1]+
                all_triangs[numtriags][2]*all_triangs[numtriags][2]);
              all_triangs[numtriags][0] /= length;
              all_triangs[numtriags][1] /= length;
              all_triangs[numtriags][2] /= length;
              numtriags++;
              all_triangs[numtriags][0] = x1 + n*dx12 + (m-1)*dx23;
              all_triangs[numtriags][1] = y1 + n*dy12 + (m-1)*dy23;
              all_triangs[numtriags][2] = z1 + n*dz12 + (m-1)*dz23;
              length = sqrt(all_triangs[numtriags][0]*all_triangs[numtriags][0]+
                all_triangs[numtriags][1]*all_triangs[numtriags][1]+
                all_triangs[numtriags][2]*all_triangs[numtriags][2]);
              all_triangs[numtriags][0] /= length;
              all_triangs[numtriags][1] /= length;
              all_triangs[numtriags][2] /= length;
              numtriags++;
              if ( m != n ) 
              {
                  // Draw lower left triangle
                  all_triangs[numtriags][0] = x1 + n*dx12 + m*dx23;
                  all_triangs[numtriags][1] = y1 + n*dy12 + m*dy23;
                  all_triangs[numtriags][2] = z1 + n*dz12 + m*dz23;
                  length = sqrt(all_triangs[numtriags][0]*all_triangs[numtriags][0]+
                          all_triangs[numtriags][1]*all_triangs[numtriags][1]+
                          all_triangs[numtriags][2]*all_triangs[numtriags][2]);
                  all_triangs[numtriags][0] /= length;
                  all_triangs[numtriags][1] /= length;
                  all_triangs[numtriags][2] /= length;
                  numtriags++;
                  all_triangs[numtriags][0] = x1 + (n-1)*dx12 + m*dx23;
                  all_triangs[numtriags][1] = y1 + (n-1)*dy12 + m*dy23;
                  all_triangs[numtriags][2] = z1 + (n-1)*dz12 + m*dz23;
                  length = sqrt(all_triangs[numtriags][0]*all_triangs[numtriags][0]+
                          all_triangs[numtriags][1]*all_triangs[numtriags][1]+
                          all_triangs[numtriags][2]*all_triangs[numtriags][2]);
                  all_triangs[numtriags][0] /= length;
                  all_triangs[numtriags][1] /= length;
                  all_triangs[numtriags][2] /= length;
                  numtriags++;
                  all_triangs[numtriags][0] = x1 + (n-1)*dx12 + (m-1)*dx23;
                  all_triangs[numtriags][1] = y1 + (n-1)*dy12 + (m-1)*dy23;
                  all_triangs[numtriags][2] = z1 + (n-1)*dz12 + (m-1)*dz23;
                  length = sqrt(all_triangs[numtriags][0]*all_triangs[numtriags][0]+
                          all_triangs[numtriags][1]*all_triangs[numtriags][1]+
                          all_triangs[numtriags][2]*all_triangs[numtriags][2]);
                  all_triangs[numtriags][0] /= length;
                  all_triangs[numtriags][1] /= length;
                  all_triangs[numtriags][2] /= length;
                  numtriags++;
              }
            }
            n++;
           } while( n<=subdiv );
      }
   }
   
   //std::cout<<"before indexing of triangs"<<std::endl;
   
   // indexing of triangs
   if (subdiv == 1) 
   {
     memcpy(triangs, triang, 20*3*sizeof(int));
     numtriags = 20;
   } 
   else 
   {
     //find for every point in triangle list the corresponding index in all_vert
     
     // initialize
     for (i=0; i < numtriags; i ++) {
       triangs[i] = -1;
     }

     // find indexes
     for(i=0; i<n_vert; i++) 
     {
       for (int j = 0; j < numtriags; j++) 
       {
         if (triangs[j] < 0) 
         {
            if ( (fabs(all_vert[i][0] - all_triangs[j][0]) < epsilon) && 
                 (fabs(all_vert[i][1] - all_triangs[j][1]) < epsilon) && 
                 (fabs(all_vert[i][2] - all_triangs[j][2]) < epsilon ) ) 
            {
                 triangs[j] = i;
             }
         }
       }
     }
     
     //for(i=0; i<n_vert; i++) 
     //  std::cout<<triangs[3*i]<<","<<triangs[3*i+1]<<","<<triangs[3*i+2]<<std::endl;

     for (i=0; i < numtriags; i ++) 
     {
       if (triangs[i] == -1)
          std::cerr << " - " << i << " :" << all_triangs[i][0] 
          << "," << all_triangs[i][1] << "," << all_triangs[i][2] << std::endl;
     }
     
     // numtriags is the number of vertices in triangles -> divide it by 3 
     numtriags = numtriags / 3;
   }
   
   //std::cout<<"before get phi/theta"<<std::endl;

   // get phi/teta
   for(i=0; i<n_vert; i++) 
   {
      icos[2*i] = atan2(all_vert[i][1],all_vert[i][0]) + M_PI;
      icos[2*i+1] = atan(all_vert[i][2]/
        sqrt(all_vert[i][0]*all_vert[i][0]+
             all_vert[i][1]*all_vert[i][1])) + M_PI_2;
      //std::cout<<icos[2*i]<<","<<icos[2*i+1]<<std::endl;
   }
}

void SphericalHarmonicMeshSource::interpol_vert(int n_phi,
        int n_theta,
        Point3 *mesh,
        int n_vertex,
        double *icos,
        Point3 *vertex)
{
   int i;
   double phi, theta;
   double x, y;
   int xu, xd, yu, yd;
   double xi, yi, zi;
   double ksi, eta;

   for(i=0; i<n_vertex; i++) 
   {
      phi = icos[2*i];//phi and theta for every vertex
      theta = icos[2*i+1];

      x = (phi-1e-5)*((float)n_phi)/(2*M_PI);
      xd = (int) x;
      xu = xd +1;
      ksi = x - xd;
      if (xu >= n_phi) 
      { 
        xd = mod(xd, n_phi);
        xu = mod(xu, n_phi);
      }

      y = theta*n_theta/M_PI-0.5;
      yd = (int) y;
      yu = yd + 1;
      eta = y - yd;
      if (yu == n_theta) 
      {
        yu = yu - 1;
        xd = mod(xd+n_theta, n_phi);
        xu = mod(xu+n_theta, n_phi);
      }
      if (yd == -1) 
      {
        yd = 0;
        xd = mod(xd+n_theta, n_phi);
        xu = mod(xu+n_theta, n_phi);
      }

      interpol_2d(n_phi, n_theta, xd, xu, yd, yu, ksi, eta, mesh, &xi, &yi, &zi);//vertex
      vertex[i][0] = xi;
      vertex[i][1] = yi;
      vertex[i][2] = zi;

   }
}

void SphericalHarmonicMeshSource::interpol_2d(int n_phi,
      int n_theta,
      int xd,
      int xu,
      int yd,
      int yu,
      double ksi,
      double eta,
      Point3 *mesh,
      double *xi,
      double *yi,
      double *zi)
{
   double f00, f10, f01, f11;
   
   f00 = mesh[xd+n_phi*yd][0];
   f10 = mesh[xu+n_phi*yd][0];
   f01 = mesh[xd+n_phi*yu][0];
   f11 = mesh[xu+n_phi*yu][0];

   *xi = f00 + (f10 - f00)*ksi + (f01 - f00)*eta +
      (f11 + f00 - f10 - f01)*ksi*eta;

   f00 = mesh[xd+n_phi*yd][1];
   f10 = mesh[xu+n_phi*yd][1];
   f01 = mesh[xd+n_phi*yu][1];
   f11 = mesh[xu+n_phi*yu][1];

   *yi = f00 + (f10 - f00)*ksi + (f01 - f00)*eta +
      (f11 + f00 - f10 - f01)*ksi*eta;

   f00 = mesh[xd+n_phi*yd][2];
   f10 = mesh[xu+n_phi*yd][2];
   f01 = mesh[xd+n_phi*yu][2];
   f11 = mesh[xu+n_phi*yu][2];

   *zi = f00 + (f10 - f00)*ksi + (f01 - f00)*eta +
      (f11 + f00 - f10 - f01)*ksi*eta;
}

int SphericalHarmonicMeshSource::mod(int x, int y)
{
   if ((x%y) >= 0)
     return (x%y);
   else
     return (y+(x%y));
}

} // end namespace itk

