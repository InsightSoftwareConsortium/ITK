// include log4cxx header files.
#include <log4cxx/logger.h>
#include <log4cxx/basicconfigurator.h>
#include <log4cxx/helpers/exception.h>


int main ( int argc, char* argv[] ) {

  // Basic configuration of the logger
  log4cxx::BasicConfigurator::configure();

  char buffer[1024];

  log4cxx::LoggerPtr logger(log4cxx::Logger::getLogger("log4cxxTest") );
  logger->debug ( "This is a debug message" );
  logger->info ( "This is an info message" );
  logger->warn ( "This is a warn message" );
  logger->error ( "This is a error message" );
  exit ( 0 );
}
