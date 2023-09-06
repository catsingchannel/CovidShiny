#include "MPVProcess.h"

int main(int argc, char** argv){
    if (argc != 2 && argc != 4) {
        std::cout << "arg number incorrect" << std::endl;
        return 1;
    }
    
    if (argc == 2) {
        MpvProcess f;
        f(argv[1]);
    }
    else{
        MpvProcess f(argv[2], argv[3]);
        f(argv[1]);
    }
    return 0;
}
