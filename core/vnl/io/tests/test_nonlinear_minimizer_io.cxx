// This is core/vnl/io/tests/test_nonlinear_minimizer_io.cxx
#include <iostream>
#include <vcl_compiler.h>
#include <vnl/vnl_nonlinear_minimizer.h>
#include <vnl/io/vnl_io_nonlinear_minimizer.h>
#include <testlib/testlib_test.h>
#include <vpl/vpl.h>

void test_nonlinear_minimizer_io()
{
  std::cout << "**********************************\n"
           << "Testing vnl_nonlinear_minimizer_io\n"
           << "**********************************\n";

  //// test constructors, accessors
  vnl_nonlinear_minimizer minimizer_out, minimizer_in;

  // minimizer settings to be saved
  double xtol_out= 0.001;
  double ftol_out= xtol_out*0.01;
  double gtol_out= 0.005;
  int maxfev_out = 3000;
  double epsfcn_out = xtol_out* 0.001;
  bool trace_out = false;
  bool verbose_out = false;
  int cd_out =1;

  minimizer_out.set_f_tolerance(ftol_out);
  minimizer_out.set_x_tolerance(xtol_out);
  minimizer_out.set_g_tolerance(gtol_out);
  minimizer_out.set_max_function_evals(maxfev_out);
  minimizer_out.set_epsilon_function(epsfcn_out);
  minimizer_out.set_trace(trace_out);
  minimizer_out.set_verbose(verbose_out);
  minimizer_out.set_check_derivatives(cd_out);

  vsl_print_summary(std::cout, minimizer_out);
  std::cout << std::endl;

  vsl_b_ofstream bfs_out("vnl_nonlinear_minimizer_io.bvl.tmp");
  TEST("Created vnl_nonlinear_minimizer_test_io.bvl.tmp for writing", (!bfs_out), false);
  vsl_b_write(bfs_out, minimizer_out);
  bfs_out.close();

  vsl_b_ifstream bfs_in("vnl_nonlinear_minimizer_io.bvl.tmp");
  TEST("Opened vnl_nonlinear_minimizer_test_io.bvl.tmp for reading", (!bfs_in), false);
  vsl_b_read(bfs_in, minimizer_in);
  TEST("Finished reading file successfully", (!bfs_in), false);
  bfs_in.close();

  vpl_unlink ("vnl_nonlinear_minimizer_io.bvl.tmp");

  double ftol_in=minimizer_in.get_f_tolerance();
  double xtol_in=minimizer_in.get_x_tolerance();
  double gtol_in=minimizer_in.get_g_tolerance();
  int maxfev_in=minimizer_in.get_max_function_evals();
  double epsfcn_in=minimizer_in.get_epsilon_function();
  bool trace_in=minimizer_in.get_trace();
  bool verbose_in=minimizer_in.get_verbose();
  int cd_in=minimizer_in.get_check_derivatives();

  TEST("ftol_in == ftol_out", ftol_in == ftol_out, true);
  TEST("xtol_in == xtol_out", xtol_in == xtol_out, true);
  TEST("gtol_in == gtol_out", gtol_in == gtol_out, true);
  TEST("maxfev_in == maxfev_out", maxfev_in == maxfev_out, true);
  TEST("epsfcn_in == epsfcn_out", epsfcn_in == epsfcn_out, true);
  TEST("trace_in == trace_out", trace_in == trace_out, true);
  TEST("verbose_in == verbose_out", verbose_in == verbose_out, true);
  TEST("cd_in == cd_out", cd_in == cd_out, true);

  vsl_print_summary(std::cout, minimizer_in);
  std::cout << std::endl;
}

TESTMAIN(test_nonlinear_minimizer_io);
