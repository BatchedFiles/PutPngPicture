#ifndef BASE64_BI
#define BASE64_BI

#include once "windows.bi"

Declare Function Encode64( _
	ByVal sEncodedB As UByte Ptr, _
	ByVal BytesCount As Integer, _
	ByVal sOut As TCHAR Ptr _
)As Integer

#endif
