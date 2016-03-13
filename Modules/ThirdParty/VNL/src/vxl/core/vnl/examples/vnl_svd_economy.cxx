#include <iostream>
#include <vcl_compiler.h>
#include <vul/vul_timer.h>
#include <vnl/vnl_random.h>
#include <vnl/vnl_matrix.h>
#include <vnl/vnl_matlab_print.h>
#include <vnl/algo/vnl_svd.h>
#include <vnl/algo/vnl_svd_economy.h>

int main()
{
  vnl_random rng(9667566ul);
  {
    vnl_matrix<double> M( 10, 4 );
    for (unsigned int i=0 ; i < M.size(); ++i) {
      M.data_block()[i] = rng.drand64(-1.0,1.0);
    }

    vnl_svd<double> svd( M );
    vnl_svd_economy<double> svd_e( M );

    vnl_matlab_print( std::cerr, svd.V() );
    std::cerr << std::endl;
    vnl_matlab_print( std::cerr, svd_e.V() );
    std::cerr << std::endl << std::endl;

    vnl_matlab_print( std::cerr, svd.W().diagonal() );
    std::cerr << std::endl;
    vnl_matlab_print( std::cerr, svd_e.lambdas() );

    std::cerr << "\n( svd.V() - svd_e.V() ).fro_norm() = " << ( svd.V() - svd_e.V() ).fro_norm()
             << "\n( svd.W().diagonal() - svd_e.lambdas() ).two_norm() = "
             << ( svd.W().diagonal() - svd_e.lambdas() ).two_norm() << std::endl;
  }

  {
    vnl_matrix<double> N( 2000, 12 );
    for (unsigned int i=0 ; i < N.size(); ++i)
      N.data_block()[i] = rng.drand64(-1.0,1.0);

    vul_timer timer;
    for (int i=0; i < 1000; ++i)
      vnl_svd<double> svd( N );

    int t1 = timer.user();
    timer.mark();
    for (int i=0; i < 1000; ++i)
      vnl_svd_economy<double> svd_e( N );

    int t2 = timer.user();

    std::cerr << "time for 1000*svd(1000x10) : vnl_svd = " << t1 << " msec, "
             << "vnl_svd_economy = " << t2 << " msec.\n";
  }

  return 0;
}
