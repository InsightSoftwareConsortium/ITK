#ifndef _vect3_h
#define _vect3_h

#include <stdio.h>

struct Vect3
{
  double v[3];

  Vect3() { v[0] = 0; v[1] = 0; v[2] = 0; }
  Vect3(const Vect3& in_v) 
    {
      v[0] = in_v.v[0];
      v[1] = in_v.v[1];
      v[2] = in_v.v[2];
    }
  
  Vect3(double a, double b, double c)
    {
      v[0] = a;
      v[1] = b;
      v[2] = c;
    }
  
  void SetV(double a, double b, double c)
    {
      v[0] = a;
      v[1] = b;
      v[2] = c;
    }  

  void SetV(double x)
    {
      v[0] = x;
      v[1] = x;
      v[2] = x;
    }
  
  void SetV(const double* in_v)
    {
      v[0] = in_v[0];
      v[1] = in_v[1];
      v[2] = in_v[2];
    }
  
  void SetV(const Vect3* in_v)
    {
      v[0] = in_v->v[0];
      v[1] = in_v->v[1];
      v[2] = in_v->v[2];
    }
  
  void SetV(const Vect3& in_v)
    {
      v[0] = in_v.v[0];
      v[1] = in_v.v[1];
      v[2] = in_v.v[2];
    }  

  Vect3* GetPointer(void)
    {
      return this;
    }

  double* GetVector(void)
    {
      return v;
    }
  
  double Magnitude2(void)
    {
      return (v[0]*v[0] + v[1]*v[1] + v[2]*v[2]);
    }

  Vect3 operator+ (const Vect3& in_v)
    {
      return Vect3(v[0]+in_v.v[0],
		   v[1]+in_v.v[1],
		   v[2]+in_v.v[2]);
    }
  
  Vect3& operator+= (const Vect3& in_v)
    {
      v[0] += in_v.v[0];
      v[1] += in_v.v[1];
      v[2] += in_v.v[2];
      return *this;
    }
  
  void Print(void)
    {
      fprintf(stdout, "(%f, %f, %f)\n", v[0], v[1], v[2]);
    }
};

#endif
