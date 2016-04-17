//:
// \file
// \brief Tool to test performance of various vnl operations.
// \author Ian Scott

#include <vector>
#include <iostream>
#include <fstream>
#include <ctime>
#include <algorithm>
#include <string>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_random.h>
#include <vcl_compiler.h>

const unsigned nstests = 10;


void fill_with_rng(double * begin, double * end, double a, double b, vnl_random &rng)
{
  while (begin != end)
  {
    *begin = rng.drand64(a, b);
    ++begin;
  }
}

void fill_with_rng(float * begin, float * end, float a, float b, vnl_random &rng)
{
  while (begin != end)
  {
    *begin = (float) rng.drand32(a, b);
    ++begin;
  }
}

template <class T>
void distance_squared(const std::vector<vnl_vector<T> > &s1,
                      const std::vector<vnl_vector<T> > &s2,
                      std::vector<T> & d, int n_loops)
{
  vnl_vector<double> stats(nstests);
  for (unsigned st=0;st<nstests;++st)
  {
    std::clock_t t0=std::clock();
    for (int l=0;l<n_loops;++l)
    {
      for (unsigned i=0;i<d.size();++i)
        d[i] = vnl_vector_ssd(s1[i], s2[i]);
    }
    std::clock_t t1=std::clock();
    stats[st] = (1e9*((double(t1)-double(t0)))/((double)n_loops*(double)CLOCKS_PER_SEC));
  }
  std::sort(stats.begin(), stats.end());
  std::cout<<"  Mean: "<<stats.mean()
          <<"ns  +/-"<<stats((unsigned)(nstests*0.75))-stats((unsigned)(nstests*0.25))<<"ns\n\n";
}

template <class T>
void dot_product(const std::vector<vnl_vector<T> > &s1,
                 const std::vector<vnl_vector<T> > &s2,
                 std::vector<T> & d, int n_loops)
{
  vnl_vector<double> stats(nstests);
  for (unsigned st=0;st<nstests;++st)
  {
    std::clock_t t0=std::clock();
    for (int l=0;l<n_loops;++l)
    {
      for (unsigned i=0;i<d.size();++i)
        d[i] = dot_product(s1[i], s2[i]);
    }
    std::clock_t t1=std::clock();
    stats[st] = (1e9*((double(t1)-double(t0)))/((double)n_loops*(double)CLOCKS_PER_SEC));
  }
  std::sort(stats.begin(), stats.end());
  std::cout<<"  Mean: "<<stats.mean()
          <<"ns  +/-"<<stats((unsigned)(nstests*0.75))-stats((unsigned)(nstests*0.25))<<"ns\n\n";
}

template <class T>
void mat_x_vec(const vnl_matrix<T> &s1, const std::vector<vnl_vector<T> > &s2,
               int n_loops)
{
  vnl_vector<double> stats(nstests);
  for (unsigned st=0;st<nstests;++st)
  {
    std::clock_t t0=std::clock();
    for (int l=0;l<n_loops;++l)
    {
      for (unsigned i=0;i<s2.size();++i)
        s1 * s2[i];
    }
    std::clock_t t1=std::clock();
    stats[st] = (1e6*((double(t1)-double(t0)))/((double)n_loops*(double)CLOCKS_PER_SEC));
  }
  std::sort(stats.begin(), stats.end());
  std::cout<<"  Mean: "<<stats.mean()
          <<"us  +/-"<<stats((unsigned)(nstests*0.75))-stats((unsigned)(nstests*0.25))<<"us\n\n";
}

template <class T>
void vec_x_mat(const std::vector<vnl_vector<T> > &s1, const vnl_matrix<T> &s2,
               int n_loops)
{
  vnl_vector<double> stats(nstests);
  for (unsigned st=0;st<nstests;++st)
  {
    std::clock_t t0=std::clock();
    for (int l=0;l<n_loops;++l)
    {
      for (unsigned i=0;i<s2.size();++i)
        s1[i] * s2;
    }
    std::clock_t t1=std::clock();
    stats[st] = (1e6*((double(t1)-double(t0)))/((double)n_loops*(double)CLOCKS_PER_SEC));
  }
  std::sort(stats.begin(), stats.end());
  std::cout<<"  Mean: "<<stats.mean()
          <<"us  +/-"<<stats((unsigned)(nstests*0.75))-stats((unsigned)(nstests*0.25))<<"us\n\n";
}

template <class T>
void print_pointers(const std::vector<vnl_vector<T> >&va, const std::vector<vnl_vector<T> >&vb,
                    const std::vector<vnl_vector<T> >&vc, const std::vector<T>&na,
                    const vnl_matrix<T>&ma, const std::string& file)
{
#ifdef DEBUG
  unsigned i;
  std::ofstream os(file.c_str());
  os << "Data values\n"
     << "\nva:" << &va.front() << ' ' << &va.back() << '\n';
  for (i=0; i<va.size(); ++i)
  { os << va[i].data_block() << va[i].size() << '\n'; }

  os << "\n\nvb:" << &vb.front() << ' ' << &vb.back() << '\n';
  for (i=0; i<vb.size(); ++i)
  { os << vb[i].data_block() << vb[i].size() << '\n'; }

  os << "\n\nvc:" << &vc.front() << ' ' << &vc.back() << '\n';
  for (i=0; i<vc.size(); ++i)
  { os << vc[i].data_block() << vc[i].size() << '\n'; }

  os << "\n\nna:" << &na.front() << ' ' << &na.back() << '\n'

     << "\n\nma:" << ma.data_block() << ' ' << ma.rows() << ' ' << ma.cols() << '\n';
  for (i=0; i<ma.rows(); ++i)
  { os << ma[i] << '\n'; }
#else
  (void)va;
  (void)vb;
  (void)vc;
  (void)na;
  (void)ma;
  (void)file;
#endif // DEBUG
}

template <class T>
void run_for_size(unsigned m, unsigned n, T /*dummy*/, const char * type, const char *size,
                  vnl_random &rng)
{
  const unsigned n_data = 10;
  std::vector<vnl_vector<T> > x(n_data), y(n_data), z(n_data);
  std::vector<T> v(n_data);
  vnl_matrix<T> A(m,n);

  for (unsigned k=0;k<n_data;++k)
  {
    x[k].set_size(n);
    z[k].set_size(m);
    y[k].set_size(m);
    fill_with_rng(x[k].begin(), x[k].end(), T(-10000),T(10000), rng);
    fill_with_rng(y[k].begin(), y[k].end(), T(-10000),T(10000), rng);
    fill_with_rng(z[k].begin(), z[k].end(), T(-10000),T(10000), rng);
  }
  fill_with_rng(A.begin(), A.end(), -10000,10000, rng);

  int n_loops = 1000000/m;
  std::cout<<"\nTimes to operator on "<<type<<' '<<m<<"-d vectors and "
          <<m<<" x "<<n<<" matrices, size " << size << '\n'
          <<"Sum of square differences       " << std::flush;
  distance_squared(z,y,v,n_loops);
  print_pointers(z, y, x, v, A, std::string("testA")+type+size);
  std::cout<<"Vector dot product              " << std::flush;
  print_pointers(z, y, x, v, A, std::string("testB")+type+size);
  dot_product(z,y,v,n_loops);
  print_pointers(z, y, x, v, A, std::string("testC")+type+size);
  std::cout<<"Matrix x Vector multiplication  " << std::flush;
  mat_x_vec(A,x,n_loops/n+1);
  print_pointers(z, y, x, v, A, std::string("testD")+type+size);
  print_pointers(z, y, x, v, A, std::string("testE")+type+size);
}

int main(int, char *[])
{
  std::cout << "Range = 75%tile-25%tile\n";
  vnl_random rng(9667566ul);
  run_for_size(2, 20, double(), "double", "2x20", rng);
  run_for_size(300, 300, double(), "double", "300x300", rng);
  run_for_size(100, 10000, double(), "double", "100x10000", rng);
  run_for_size(10000, 100, double(), "double", "10000x100", rng);
  run_for_size(30, 30000, double(), "double", "30x30000", rng);
  run_for_size(2, 20, float(), "float", "2x20", rng);
  run_for_size(300, 300, float(), "float", "300x300", rng);
  run_for_size(100, 10000, float(), "float", "100x10000", rng);
  run_for_size(10000, 100, float(), "float", "10000x100", rng);
  run_for_size(30, 30000, float(), "float", "30x30000", rng);
  return 0;
}
