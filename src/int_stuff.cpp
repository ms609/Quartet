#include "int_stuff.h"
#include <iostream>
#include <iomanip>

#ifdef N4INT128
#ifdef _WIN32
	std::ostream &operator<<(std::ostream &strm, unsigned __uint128 value)
	{
		const uint64_t uLongLongMax = 18446744073709551615ULL;
		const uint64_t uNineteenZeroes = 10000000000000000000ULL;
		if (value > uLongLongMax)
		{
			__uint128 first = value / uNineteenZeroes;
			uint64_t rest = value % uNineteenZeroes;
			operator<<(strm, first);
			strm << std::setfill ('0') << std::setw (19);
			strm << rest;
		}
		else
		{
			// Printable directly
			uint64_t shorterValue = value;
			strm << shorterValue;
		}
		return strm;
	}
	
	std::ostream &operator<<(std::ostream &strm, __int128 value)
	{
		strm << "value: " << value << std::endl;
		const uint64_t uLongLongMax = 18446744073709551615ULL;
		const uint64_t uNineteenZeroes = 10000000000000000000ULL;
		if (value < 0)
		{
			strm << "(negative value)";
		}
		else if (value > uLongLongMax)
		{
			__int128 first = value / uNineteenZeroes;
			uint64_t rest = value % uNineteenZeroes;
			operator<<(strm, first);
			strm << std::setfill ('0') << std::setw (19);
			strm << rest;
		}
		else
		{
			// Printable directly
			uint64_t shorterValue = value;
			strm << shorterValue;
		}
		return strm;
	}
#else
	std::ostream &operator<<(std::ostream &strm, __uint128_t value)
	{
		strm << "value2: " << value << std::endl;
		const uint64_t uLongLongMax = 18446744073709551615ULL;
		const uint64_t uNineteenZeroes = 10000000000000000000ULL;
		if (value > uLongLongMax)
		{
			__uint128_t first = value / uNineteenZeroes;
			uint64_t rest = value % uNineteenZeroes;
			operator<<(strm, first);
			strm << std::setfill ('0') << std::setw (19);
			strm << rest;
		}
		else
		{
			// Printable directly
			uint64_t shorterValue = value;
			strm << shorterValue;
		}
		return strm;
	}
	
	std::ostream &operator<<(std::ostream &strm, __int128_t value)
	{
		const uint64_t uLongLongMax = 18446744073709551615ULL;
		const uint64_t uNineteenZeroes = 10000000000000000000ULL;
		if (value < 0)
		{
			strm << "(negative value)";
		}
		else if (value > uLongLongMax)
		{
			__int128_t first = value / uNineteenZeroes;
			uint64_t rest = value % uNineteenZeroes;
			operator<<(strm, first);
			strm << std::setfill ('0') << std::setw (19);
			strm << rest;
		}
		else
		{
			// Printable directly
			uint64_t shorterValue = value;
			strm << shorterValue;
		}
		return strm;
	}	
#endif
#endif
