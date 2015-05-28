#include <iostream>
#include <fstream>
#include "CoherenceEnhancingDiffusionCommandLine.h"

int main(int argc, char **argv)
{    
    try {
        CoherenceEnhancingDiffusionCommandLine::Execute(argc, argv);
    } catch (itk::ExceptionObject& e) {
        std::cerr << "ITK Exception : " << e.GetDescription() << std::endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}


