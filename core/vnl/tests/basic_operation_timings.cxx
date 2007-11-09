//:
// \file
// \brief Tool to test performance of various vnl operations.
// \author Ian Scott

#include <vcl_vector.h>
#include <vcl_iostream.h>
#include <vcl_fstream.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_random.h>
#include <vcl_ctime.h>
#include <vcl_algorithm.h>
#include <vcl_string.h>

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
void distance_squared(const vcl_vector<vnl_vector<T> > &s1,
                      const vcl_vector<vnl_vector<T> > &s2,
                      vcl_vector<T> & d, int n_loops)
{
  vnl_vector<double> stats(nstests);
  for (unsigned st=0;st<nstests;++st)
  {
    vcl_clock_t t0=vcl_clock();
    for (int l=0;l<n_loops;++l)
    {
      for (unsigned i=0;i<d.size();++i)
        d[i] = vnl_vector_ssd(s1[i], s2[i]);
    }
    vcl_clock_t t1=vcl_clock();
    stats[st] = (1e9*((double(t1)-double(t0)))/((double)n_loops*(double)CLOCKS_PER_SEC));
  }
  vcl_sort(stats.begin(), stats.end());
  vcl_cout<<"  Mean: "<<stats.mean()
          <<"ns  +/-"<<stats((unsigned)(nstests*0.75))-stats((unsigned)(nstests*0.25))<<"ns\n\n";
}

template <class T>
void dot_product(const vcl_vector<vnl_vector<T> > &s1,
                 const vcl_vector<vnl_vector<T> > &s2,
                 vcl_vector<T> & d, int n_loops)
{
  vnl_vector<double> stats(nstests);
  for (unsigned st=0;st<nstests;++st)
  {
    vcl_clock_t t0=vcl_clock();
    for (int l=0;l<n_loops;++l)
    {
      for (unsigned i=0;i<d.size();++i)
        d[i] = dot_product(s1[i], s2[i]);
    }
    vcl_clock_t t1=vcl_clock();
    stats[st] = (1e9*((double(t1)-double(t0)))/((double)n_loops*(double)CLOCKS_PER_SEC));
  }
  vcl_sort(stats.begin(), stats.end());
  vcl_cout<<"  Mean: "<<stats.mean()
          <<"ns  +/-"<<stats((unsigned)(nstests*0.75))-stats((unsigned)(nstests*0.25))<<"ns\n\n";
}

template <class T>
void mat_x_vec(const vnl_matrix<T> &s1, const vcl_vector<vnl_vector<T> > &s2,
               int n_loops)
{
  vnl_vector<double> stats(nstests);
  for (unsigned st=0;st<nstests;++st)
  {
    vcl_clock_t t0=vcl_clock();
    for (int l=0;l<n_loops;++l)
    {
      for (unsigned i=0;i<s2.size();++i)
        s1 * s2[i];
    }
    vcl_clock_t t1=vcl_clock();
    stats[st] = (1e6*((double(t1)-double(t0)))/((double)n_loops*(double)CLOCKS_PER_SEC));
  }
  vcl_sort(stats.begin(), stats.end());
  vcl_cout<<"  Mean: "<<stats.mean()
          <<"us  +/-"<<stats((unsigned)(nstests*0.75))-stats((unsigned)(nstests*0.25))<<"us\n\n";
}

template <class T>
void vec_x_mat(const vcl_vector<vnl_vector<T> > &s1, const vnl_matrix<T> &s2,
               int n_loops)
{
  vnl_vector<double> stats(nstests);
  for (unsigned st=0;st<nstests;++st)
  {
    vcl_clock_t t0=vcl_clock();
    for (int l=0;l<n_loops;++l)
    {
      for (unsigned i=0;i<s2.size();++i)
        s1[i] * s2;
    }
    vcl_clock_t t1=vcl_clock();
    stats[st] = (1e6*((double(t1)-double(t0)))/((double)n_loops*(double)CLOCKS_PER_SEC));
  }
  vcl_sort(stats.begin(), stats.end());
  vcl_cout<<"  Mean: "<<stats.mean()
          <<"us  +/-"<<stats((unsigned)(nstests*0.75))-stats((unsigned)(nstests*0.25))<<"us\n\n";
}

template <class T>
void print_pointers(const vcl_vector<vnl_vector<T> >&va, const vcl_vector<vnl_vector<T> >&vb,
                    const vcl_vector<vnl_vector<T> >&vc, const vcl_vector<T>&na,
                    const vnl_matrix<T>&ma, const vcl_string& file)
{
#ifdef DEBUG
  unsigned i;
  vcl_ofstream os(file.c_str());
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
  vcl_vector<vnl_vector<T> > x(n_data), y(n_data), z(n_data);
  vcl_vector<T> v(n_data);
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
  vcl_cout<<"\nTimes to operator on "<<type<<' '<<m<<"-d vectors and "
          <<m<<" x "<<n<<" matrices, size " << size << '\n'
          <<"Sum of square differences       " << vcl_flush;
  distance_squared(z,y,v,n_loops);
  print_pointers(z, y, x, v, A, vcl_string("testA")+type+size);
  vcl_cout<<"Vector dot product              " << vcl_flush;
  print_pointers(z, y, x, v, A, vcl_string("testB")+type+size);
  dot_product(z,y,v,n_loops);
  print_pointers(z, y, x, v, A, vcl_string("testC")+type+size);
  vcl_cout<<"Matrix x Vector multiplication  " << vcl_flush;
  mat_x_vec(A,x,n_loops/n+1);
  print_pointers(z, y, x, v, A, vcl_string("testD")+type+size);
#if 0
  vcl_cout<<"Vector x Matrix multiplication  " << vcl_flush;
  vec_x_mat(y,A,n_loops/n+1);
#endif
  print_pointers(z, y, x, v, A, vcl_string("testE")+type+size);
}

int main(int, char *[])
{
  vcl_cout << "Range = 75%tile-25%tile\n";
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
