#include "int_stuff.h"
#include <iostream>
#include <iomanip>

#ifdef N4INT128
#ifdef _WIN32
	std::ostream &operator<<(std::ostream &strm, unsigned __uint128 value)
	{
		const unsigned long long uLongLongMax = 18446744073709551615ULL;
		const unsigned long long uNineteenZeroes = 10000000000000000000ULL;
		if (value > uLongLongMax)
		{
			__uint128 first = value / uNineteenZeroes;
			unsigned long long rest = value % uNineteenZeroes;
			operator<<(strm, first);
			strm << std::setfill ('0') << std::setw (19);
			strm << rest;
		}
		else
		{
			// Printable directly
			unsigned long long shorterValue = value;
			strm << shorterValue;
		}
		return strm;
	}
	
	std::ostream &operator<<(std::ostream &strm, __int128 value)
	{
		strm << "value: " << value << std::endl;
		const unsigned long long uLongLongMax = 18446744073709551615ULL;
		const unsigned long long uNineteenZeroes = 10000000000000000000ULL;
		if (value < 0)
		{
			strm << "(negative value)";
		}
		else if (value > uLongLongMax)
		{
			__int128 first = value / uNineteenZeroes;
			unsigned long long rest = value % uNineteenZeroes;
			operator<<(strm, first);
			strm << std::setfill ('0') << std::setw (19);
			strm << rest;
		}
		else
		{
			// Printable directly
			unsigned long long shorterValue = value;
			strm << shorterValue;
		}
		return strm;
	}
#else
	std::ostream &operator<<(std::ostream &strm, __uint128_t value)
	{
		strm << "value2: " << value << std::endl;
		const unsigned long long uLongLongMax = 18446744073709551615ULL;
		const unsigned long long uNineteenZeroes = 10000000000000000000ULL;
		if (value > uLongLongMax)
		{
			__uint128_t first = value / uNineteenZeroes;
			unsigned long long rest = value % uNineteenZeroes;
			operator<<(strm, first);
			strm << std::setfill ('0') << std::setw (19);
			strm << rest;
		}
		else
		{
			// Printable directly
			unsigned long long shorterValue = value;
			strm << shorterValue;
		}
		return strm;
	}
	
	std::ostream &operator<<(std::ostream &strm, __int128_t value)
	{
		const unsigned long long uLongLongMax = 18446744073709551615ULL;
		const unsigned long long uNineteenZeroes = 10000000000000000000ULL;
		if (value < 0)
		{
			strm << "(negative value)";
		}
		else if (value > uLongLongMax)
		{
			__int128_t first = value / uNineteenZeroes;
			unsigned long long rest = value % uNineteenZeroes;
			operator<<(strm, first);
			strm << std::setfill ('0') << std::setw (19);
			strm << rest;
		}
		else
		{
			// Printable directly
			unsigned long long shorterValue = value;
			strm << shorterValue;
		}
		return strm;
	}	
#endif
#endif
