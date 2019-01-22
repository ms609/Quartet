#ifndef INT_STUFF_H
  
  #define INT_STUFF_H
  
#if __cplusplus <= 199711L
  #define INTTYPE_N4 long int // long long fails -pedantic
  #define INTTYPE_REST long int // long long fails -pedantic
#else 
  #include <cstdint>
  #define INTTYPE_N4 int_fast64_t // long long fails -pedantic
  #define INTTYPE_REST int_fast64_t // long long fails -pedantic

#endif
#endif