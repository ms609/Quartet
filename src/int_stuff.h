#ifndef INT_STUFF_H
  
  #define INT_STUFF_H
  
  #include <iostream>
  #include <stdint.h>
  
  #ifdef _WIN32
    #define INTTYPE_N4 __int64 //previously long long, which fails -pedantic
    #define INTTYPE_REST __int64 //previously long long, which fails -pedantic
  #else
    #define INTTYPE_N4 __int64_t //previously long long, which fails -pedantic
    #define INTTYPE_REST __int64_t //previously long long, which fails -pedantic
  #endif
  
#endif