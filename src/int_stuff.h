#ifndef INT_STUFF_H
  
  #define INT_STUFF_H
  
  #include <iostream>
  #include <stdint.h>
  
  #ifdef _WIN32
  
    #ifdef N4INT128
      #define INTTYPE_N4 __int128
      #ifdef NRESETINT128
        #define INTTYPE_REST __int128
      #else
        #define INTTYPE_REST __int64 //previously long long, which fails -pedantic
      #endif
  
      std::ostream &operator<<(std::ostream &strm, __uint128 value);
      std::ostream &operator<<(std::ostream &strm, __int128 value);
    #else
      #define INTTYPE_N4 __int64 //previously long long, which fails -pedantic
      #define INTTYPE_REST __int64 //previously long long, which fails -pedantic
    #endif
  
  #else
  
    #ifdef N4INT128
      #define INTTYPE_N4 __int128_t
      #ifdef NRESETINT128
        #define INTTYPE_REST __int128_t
      #else
        #define INTTYPE_REST __int64_t //previously long long, which fails -pedantic
      #endif
      
      std::ostream &operator<<(std::ostream &strm, __uint128_t value);
      std::ostream &operator<<(std::ostream &strm, __int128_t value);
    #else
      #define INTTYPE_N4 __int64_t //previously long long, which fails -pedantic
      #define INTTYPE_REST __int64_t //previously long long, which fails -pedantic
    #endif
    
  #endif
  
#endif