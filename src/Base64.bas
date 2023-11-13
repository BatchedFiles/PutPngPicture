Private Function E0(ByVal v1 As UByte)As UByte
	Return v1 shr 2
End Function

Private Function E1(ByVal v1 As UByte, ByVal v2 As UByte)As UByte
	Return ((v1 And 3) shl 4) + (v2 shr 4)
End Function

Private Function E2(ByVal v2 As UByte, ByVal v3 As UByte)As UByte
	Return ((v2 And &H0F) shl 2) + (v3 shr 6)
End Function

Private Function E3(ByVal v3 As UByte)As UByte
	Return v3 And &H3F
End Function

Public Function Encode64( _
		ByVal sEncodedB As UByte Ptr, _
		ByVal BytesCount As Integer, _
		ByVal sOut As TCHAR Ptr _
	)As Integer
	
	Dim ELM3 As Integer = BytesCount Mod 3
	Dim k As Integer = 0
	Dim j As Integer = 0
	
	For j = 0 To BytesCount - ELM3 - 1 Step 3
		Dim ch0 As UByte = sEncodedB[j + 0]
		Dim ch1 As UByte = sEncodedB[j + 1]
		Dim ch2 As UByte = sEncodedB[j + 2]
		
		Dim Index0 As Integer = E0(ch0)
		Dim Index1 As Integer = E1(ch0, ch1)
		Dim Index2 As Integer = E2(ch1, ch2)
		Dim Index3 As Integer = E3(ch2)
		
		sOut[k + 0] = (@EncodingLookupTable + Index0)[0]
		sOut[k + 1] = (@EncodingLookupTable + Index1)[0]
		sOut[k + 2] = (@EncodingLookupTable + Index2)[0]
		sOut[k + 3] = (@EncodingLookupTable + Index3)[0]
		
		k += 4
	Next
	
	Select Case ELM3
		
		Case 1
			Dim ch0 As UByte = sEncodedB[j + 0]
			Dim ch1 As UByte = sEncodedB[j + 1]
			
			Dim Index0 As Integer = E0(ch0)
			Dim Index1 As Integer = E1(ch0, ch1)
			
			sOut[k + 0] = (@EncodingLookupTable + Index0)[0]
			sOut[k + 1] = (@EncodingLookupTable + Index1)[0]
			sOut[k + 2] = Asc("=")
			sOut[k + 3] = Asc("=")
			
			k += 4
			
		Case 2
			Dim ch0 As UByte = sEncodedB[j + 0]
			Dim ch1 As UByte = sEncodedB[j + 1]
			Dim ch2 As UByte = sEncodedB[j + 2]
			
			Dim Index0 As Integer = E0(ch0)
			Dim Index1 As Integer = E1(ch0, ch1)
			Dim Index2 As Integer = E2(ch1, ch2)
			
			sOut[k + 0] = (@EncodingLookupTable + Index0)[0]
			sOut[k + 1] = (@EncodingLookupTable + Index1)[0]
			sOut[k + 2] = (@EncodingLookupTable + Index2)[0]
			sOut[k + 3] = Asc("=")
			
			k += 4
			
	End Select
	
	' Set terminate Nul Character
	sOut[k] = 0
	
	Return k
	
End Function
